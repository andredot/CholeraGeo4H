
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

make_fig_1b <- function(db, var, title, color) {
  db |>
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(x = date, y = {{var}}), color = color) +
    ggplot2::facet_wrap(~ ADM1REF_EN, ncol = 4,
                        scales = "free_y") +
    ggplot2::labs(title = title,
                  x = "Epidemiological week",
                  y = "")
}

make_fig_1c <- function(db, var, title, color) {
  db |>
    dplyr::group_by(epi_date) |>
    dplyr::summarise(tot = sum({{var}}),
                     date = median(date)) |>
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(x = date, y = tot),
      size = 2, color = color) +
    ggplot2::labs(title = title,
                  x = "Epidemiological week",
                  y = "")
}

make_fig_1d <- function(db, var, title) {
  breaks_fixed<-c(-0.1,0,1,2)
  palette_abs  <- c("lightgreen","orange","red")

  db |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = cut({{var}}, breaks_fixed)))+
    ggplot2::scale_fill_manual( values = palette_abs,
                                name = "Status"
    ) +
    ggplot2::labs(title = title) +
    ggplot2::theme_light()
}

make_fig_1ef <- function(
    db, var, title, color,
    subtitle = "Weekly cases in the past 4 weeks"
    ) {
  db |>
    ggplot2::ggplot() +
    ggplot2::geom_histogram(
      ggplot2::aes(x = {{var}}), bins = 50, fill = color) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  x = "", y = "")
}
