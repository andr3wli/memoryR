#' Pivot the data a bit
#'
#' @param data The data frame produced by tidy_redraw_data function.
#'
#' @return A clean and tidy data set that is ready for plotting and analysis.
#' @export

make_clean_redraw <- function(data) {

  long_data_all <- make_longer(data)
  clean_data <- make_wider(long_data_all)

  return(clean_data)
}

#' Make the data frame longer.
#'
#' @param data The data produced by the tidy_redraw_function aka the data provided by the user.
#'
#' @return A long form data set.

make_longer <- function(data) {
  long_data <- data |>
    tidyr::pivot_longer(!c("subject", "poli", "MTurk"), names_to = "axis", values_to = "value") |>
    dplyr::mutate(value = as.numeric(value))
}

#' Take the now too long data set and make it perfectly tidy for future analysis.
#'
#' @param data The data produced bt the make_longer function.
#'
#' @return A nice tidy data ready for plotting and analysis - with some minor changes.

make_wider <- function(data) {
  wide_data <- data |>
    tidyr::pivot_wider(names_from = axis, values_from = value) |>
    tidyr::unchop(tidyselect::everything())
}
