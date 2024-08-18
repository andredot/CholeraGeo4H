
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
preprocess_ycc <- function(db) {

  ## Names aligned with the OCHA Shapefile
  # join_dict <- data.frame(
  #   csv = c("YE11", "YE12", "YE13", "YE14", "YE15",
  #           "YE16", "YE17", "YE18", "YE19", "YE20",
  #           "YE21", "YE22", "YE23", "YE24", "YE25",
  #           "YE26", "YE27", "YE28", "YE29", "YE30",
  #           "YE31", "YE32"
  #   ),
  #   shp = c("YE11", "YE12", "YE13", "YE14", "YE15",
  #           "YE16", "YE17", "YE18", "YE19", "YE20",
  #           "YE21", "YE22", "YE23", "YE24", "YE25",
  #           "YE26", "YE27", "YE28", "YE29", "YE30",
  #           "YE31", "YE32"
  #   )
  # )

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
    ## Group by Week
    dplyr::mutate(govt = as.factor(govt),
                  epiw = lubridate::epiweek(date),
                  epiy = lubridate::epiyear(date),
                  epi_date = paste0(epiy,"-",epiw)) |>
    dplyr::group_by(epi_date, pcode) |>
    dplyr::reframe(date = max(date),
                   cases = max(cases),
                   deaths = max(deaths),
                   cfr_abs = max(deaths),
                   attack_abs = max(attack_abs)
                   ) |>
    dplyr::distinct() #|>
    ## Adapt names to SHP ones
    # dplyr::left_join(join_dict,
    #                  by = dplyr::join_by(govt == csv),
    #                  relationship = "many-to-many") |>
    # dplyr::select(-govt) |>
    # dplyr::rename(govt = shp)
}

preprocess_shp <- function(db) {
  db |>
    dplyr::select(ADM1_EN, ADM1_PCODE, ADM1REF_EN)
}

preprocess_split <- function(db, col) {
  db |>
    dplyr::select(epi_date,govt,pcode,{{col}}) |>
    # dplyr::filter(!is.na(govt)) |>
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

join_wider <- function(db1, db2, db3, db4) {
  db1 |>
    dplyr::left_join(db2, by = dplyr::join_by(pcode)) |>
    dplyr::left_join(db3, by = dplyr::join_by(pcode)) |>
    dplyr::left_join(db4, by = dplyr::join_by(pcode))
}

adj_list <- function(shp, .data_path) {
  spdep::poly2nb(shp) |>
    spdep::nb2WB()
}
