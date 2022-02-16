#' Add a column that denotes salience.
#'
#' @param df The data frame from the make_clean_redraw function.
#'
#' @return A data frame with a salience column - to be used for the statistical analysis.
#' @export

add_salient_col <- function(df) {
  mid_point <- 366

  stats_df <- df |>
    dplyr::filter(poli != "Independent") |>
    dplyr::rename(x_axis = x,
                  y_axis = y) |>
    dplyr::mutate(salient = dplyr::case_when(x_axis <= mid_point ~ "Flat",
                                             x_axis >= mid_point ~ "Rising",
                                             TRUE ~ "FLAT"))

  return(stats_df)
}
