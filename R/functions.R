
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

preprocess_ycc <- function(db) {
  db |>
    dplyr::select(-c("COD Gov English",
                     "COD Gov Arabic",
                     "COD Gov Pcode")) |>
    dplyr::rename(
      date = Date,
      govt = Governorate,
      cases = Cases,
      deaths = Deaths,
      cfr_abs = `CFR (%)`,
      attack_abs = `Attack Rate (per 1000)`
    ) |>
    dplyr::mutate(govt = as.factor(govt))
}

relevant_computation <- function(db) {
  2 * length(db) + 1
}
