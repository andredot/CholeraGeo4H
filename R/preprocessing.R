
# Here below put your main project's functions ---------------------


#' Null function
#'
#' Use this as template for your first function, and delete it!
#'
#' @note You can add all this documentation infrastructure pressing
#'   `CTRL + SHIFT + ALT + R` from anywhere inside the function's body.
#'
#' @param x (default, NULL)
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#'   null()
#'   null(1)
#' }
null <- function(x = NULL) {
  if (!is.null(x)) NULL else x
}

import_data <- function(.data_path) {
  file.path(.data_path) |>
    normalizePath() |>
    readr::read_csv()
}

import_shape <- function(.data_path) {
  file.path(.data_path) |>
    normalizePath() |>
    sf::read_sf()
}

#' Preprocessing of Yemen Cholera cases
#'
#' @param db : dataframe obtained by the Yemen Cholera cases csv
#'
#' @return a new db with
#' - clean column names,
#' - dates subdivided by epidemiological week and year,
#' - with all epi variables grouped by week,
#' - government names that are the same as OCHA adm boundaries
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   preprocess_ycc(ycc)
#' }
preprocess_ycc <- function(db, shp) {
  db |>
    ## Change Column names
    dplyr::select(-c("COD Gov English",
                     "COD Gov Arabic")) |>
    dplyr::rename(
      date = Date,
      govt = Governorate,
      cases = Cases,
      deaths = Deaths,
      cfr_abs = `CFR (%)`,
      attack_abs = `Attack Rate (per 1000)`,
      pcode = `COD Gov Pcode`
    ) |>
    ## Deal with "#NA" in pcode:
    ## Moklla amd Say'on are cities in Hadramawt (PCODE 19)
    dplyr::mutate(dplyr::across(pcode,
       ~ dplyr::if_else(
         .x == "#N/A" & (govt == "Moklla" | govt == "Say'on"),
         true = "19", false = .x))) |>
    dplyr::mutate(pcode = paste0("YE", pcode)) |>
    ## Group by Week
    dplyr::mutate(govt = as.factor(govt),
                  epiw = lubridate::epiweek(date),
                  epiy = lubridate::epiyear(date),
                  epi_date = paste0(epiy,"-",epiw)) |>
    dplyr::group_by(epi_date, pcode) |>
    dplyr::reframe(date = max(date),
                   cases = max(cases),
                   deaths = max(deaths),
                   cfr_abs = max(cfr_abs),
                   attack_abs = max(attack_abs)
                   ) |>
    dplyr::distinct() |>
    ## Add OCHA governatorate names
    dplyr::left_join(
      shp |>
        dplyr::select(ADM1_PCODE,ADM1REF_EN),
      by = dplyr::join_by(pcode == ADM1_PCODE)
    ) # |> sf::st_drop_geometry()
}

preprocess_shp <- function(db) {
  db |>
    dplyr::select(ADM1_PCODE, ADM1REF_EN, geometry)
}

preprocess_split <- function(db, col) {
  db |>
    dplyr::select(epi_date,pcode,{{col}}) |>
    tidyr::pivot_wider(names_from = epi_date,
                       values_from = {{col}},
                       names_prefix = paste0({{col}},"-"))
}

preprocess_join <- function(shp, db) {
  require(sf)
  dplyr::left_join(
    shp, db,
    by = dplyr::join_by(ADM1_PCODE == pcode)
    )
}

join_wider <- function(db1, db2, db3, db4, db5 = NULL) {
  t <- db1 |>
    dplyr::left_join(db2, by = dplyr::join_by(pcode)) |>
    dplyr::left_join(db3, by = dplyr::join_by(pcode)) |>
    dplyr::left_join(db4, by = dplyr::join_by(pcode))

  ifelse( "tbl" %in% class(db5),
          t <- t |> dplyr::left_join(db5, by = dplyr::join_by(pcode)),
          t
  )

  return(t)
}

adj_list <- function(shp, .data_path) {
  spdep::poly2nb(shp) |>
    spdep::nb2WB()
}

#' Preprocessing of Yemen Cholera cases
#'
#' @param db : Yemen Cholera Cases (ycc) database
#'
#' @return a new db with new cases, new deaths, CFR, Attack rate
#' within the last 4 weeks
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   preprocess_ycc_lags(ycc)
#' }
preprocess_ycc_lags <- function(db) {
  dbpop <- db |>
    ## generate population counts
    dplyr::group_by(pcode) |>
    dplyr::summarise(pop = median( cases/(attack_abs/1000)) )

  db |>
    dplyr::left_join(dbpop, by = dplyr::join_by(pcode)) |>
    ## generate epi variables in the previous 4 weeks
    dplyr::group_by(pcode) |>
    dplyr::mutate(
      dplyr::across(
        cases:deaths,
        ~ .x - dplyr::lag(.x, default = 4L),
        .names = "{.col}_4w"
      )) |>
    dplyr::mutate(cfr_4w = 100*deaths_4w / cases_4w,
                  cfr_4w = dplyr::if_else(
                    is.nan(cfr_4w), # get rid of div0
                    true = 0,
                    false = cfr_4w),
                  attack_4w = 1000*cases_4w / pop
    ) |>
    dplyr::ungroup()
}

#' Assign current status
#'
#' @param db : Yemen Cholera Cases (ycc_lag) database, variables
#' should be defined
#'
#' @return a new db with an additional status column that represent
#'  the region configuration, either the outbreak is controlled
#'  (level 0 - preparedness), cases are on the surge but with CFR
#'  under the 1% threshold (level 1 - early action), or CFR is over
#'  1%, signalling an unacceptable stress level (level 2 - response).
#'  Level 2 is declared also if the attack rate in the previous 4
#'  weeks has been higher that 2 per 1000 people.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   cholera_status(ycc_lag)
#' }
cholera_status <- function(db) {
  db |>
    dplyr::mutate(
      status = dplyr::case_when(
        cases_4w <= 0 ~ 0,
        (cfr_abs  > 1) | (attack_4w >= 2)  ~ 2,
        cfr_abs  <= 1 ~ 1
      )
    )

}



