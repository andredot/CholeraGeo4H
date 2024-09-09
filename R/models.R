
#' Time series spline model
#'
#' Creates a glm model to predict Status from other epidemiological
#' variables
#'
#' @param db (default, NULL) is a ycc_lag database with the cases_4w
#' + deaths_4w + cfr_4w + attack_4w variables
#'
#' @return confidence interval after fitting a GLM with 10 df spline
#' to model temporal dynamics
#'
#' @examples
#' \dontrun{
#'   model_spline(ycc_lag)
#' }
model_spline <- function(db) {
  spltime <- splines::bs(db$date, df=2*5)
  mspline <- glm(
    status ~ cases_4w + deaths_4w + cfr_4w + attack_4w + spltime,
    db, family=quasipoisson)
  Epi::ci.exp(mspline)
}

#' Cross basis function
#'
#' Creates a crossbasis function for a model to predict Status from a single lagged
#' epidemiological variable, with up to 4 weeks lag
#'
#' @param db (default, NULL) is a ycc_lag database with the cases_4w
#' + deaths_4w + cfr_4w + attack_4w variables
#' @param var (default, NULL) one of the variables in ycc_lag
#'
#' @return The crossbasis function
#'
#' @examples
#' \dontrun{
#'   cb_dlnm(ycc_lag, "cases_4w")
#' }
cb_dlnm <- function(db, var) {
  dlnm::crossbasis(
    db[{{var}}],
    lag=4,
    argvar=list(fun="lin"),
    arglag=list(fun="integer")
  )
}

#' Lag-linear model with cross basis function
#'
#' Creates a model to predict Status from a single lagged
#' epidemiological variable, with up to 4 weeks lag
#'
#' @param db (default, NULL) is a ycc_lag database with the cases_4w
#' + deaths_4w + cfr_4w + attack_4w variables
#' @param cb_var (default, NULL) the crossbasis function
#'
#' @return The fitted model
#'
#' @examples
#' \dontrun{
#'   model_dlnm(ycc_lag, "cases_4w")
#' }
model_dlnm <- function(db, cb_var) {
  spltime <- splines::bs(db$date, df=2*3)
  glm(status ~ cb_var + spltime,
      family=quasipoisson(),
      db)
}

#' Region specific estimates of Log RR and errors
#'
#' Get region specific estimates to be pooled later into a meta-
#' analysis through a glm model fitted on the usual 4 epidemiological
#' variables
#'
#' @param db (default, NULL) is a ycc_lag database with the cases_4w
#' + deaths_4w + cfr_4w + attack_4w variables
#'
#' @return A tibble with the estimates and standard errors for the
#' 3 epidemiological variables (attack rate is excluded since it's
#' collinear by definition)
#'
#' @examples
#' \dontrun{
#'   get_log_epi(ycc_lag)
#' }
get_log_epi <- function(db) {
  regs <- db[["ADM1REF_EN"]] |> unique()
  log_epi <- tibble::tibble()

  for(i in regs) {
    sub <- db |>
      dplyr::filter(ADM1REF_EN == i)
    spltime <- splines::bs(sub$date, df=3)
    m <- glm(status ~ cases_4w + deaths_4w + cfr_4w + spltime,
             data = sub, family = quasipoisson)
    line_result <- list(
      Governorate = i,
      Cases = coef(m)[["cases_4w"]],
      Deaths = coef(m)[["deaths_4w"]],
      CFR = coef(m)[["cfr_4w"]],
      #Attack_rate = coef(m)[["attack_4w"]], # is NA because Collinear with Cases in the same district
      CasesSE = sqrt(vcov(m)["cases_4w","cases_4w"]),
      DeathsSE = sqrt(vcov(m)["deaths_4w","deaths_4w"]),
      CFRSE = sqrt(vcov(m)["cfr_4w","cfr_4w"])
      #Attack_rateSE = sqrt(vcov(m)["attack_4w","attack_4w"])
    )
    log_epi <- log_epi |>
      dplyr::bind_rows(line_result)
  }

  log_epi
}

model_0 <- function(db) {
  glm(target_status ~ cases_exp + status,
      family = poisson,
      data = db)
}

pred_models <- function(db, mod0, mod1, mod2) {
  db |>
    dplyr::mutate(pois0 = predict(mod0) / exp,
           bym_cases = predict(mod1) / exp,
           bym_attack = predict(mod2) / exp
           )
}

save_data <- function(db) {
  exp <- db |>
    sf::st_drop_geometry() |>
    dplyr::select("status-2018-1","cases_exp-2017-51",
                  "attack_exp-2017-51","status-2017-51") |>
    dplyr::rename(
      "target_status[]" = "status-2018-1",
      "cases_exp[]" = "cases_exp-2017-51",
      "attack_exp[]" = "attack_exp-2017-51",
      "status[]" = "status-2017-51")
  exp$"attack_exp[]" <- format(exp$`attack_exp[]`, scientific = FALSE,
                              na.encode = NA)
  exp |>
    readr::write_delim("./openbugs/data_201751.txt", delim = " ")

  cat("END", file="./openbugs/data_201751.txt", append=TRUE, sep = "\n")
}
