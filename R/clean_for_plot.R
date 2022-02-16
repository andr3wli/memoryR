#' Create the y mean column from the grouped_by poli and x.
#'
#' @param df The data set from the make_clean_redraw function.
#'
#' @return A clean data set ready to create the redraw plots.
#' @export

clean_for_plot <- function(df) {
  data <- df |>
    dplyr::group_by(poli, x) |>
    dplyr::summarise(y_mean = mean(y, na.rm = TRUE))

  return(data)
}
