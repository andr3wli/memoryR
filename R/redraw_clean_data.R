#' Data cleaning for the participant redraw data set
#'
#' @param path A string variable for path for the redraw data set.
#' @param discard_data A numeric vector of the unique participant numbers
#'
#' @return A data frame containing the redraw data but cleaned up a bit.
#' @export

redraw_clean_data <- function(path, discard_data) {
  # read in the data
  redraw_df <- redraw_csv(path)

  # filter out participants that are not in the petition data set
  discard_df <- redraw_discard(discard_data)

  # clean the redraw data set
  redraw <- clean_redraw(redraw = redraw_df, discard = discard_df)

  return(redraw)
}

#' Read in the redraw data with no column names
#'
#' @param path A string variable of the path for the redraw data set.
#'
#' @return A data frame of the redraw data set with default R columns.
redraw_csv <- function(path) {
  redraw <- readr::read_csv(path, col_names = FALSE)
}

#' Filter out participants that are not in the petition data set
#'
#' @param discard_data
#'
#' @return A vector of the unique participant numbers from the petition data set.
redraw_discard <- function(discard_data) {
  discard_df <- unique(discard_data)
}

#' Cleans the redraw data set a bit.
#'
#' @param redraw The resulting data frame from the redraw_csv function.
#' @param discard The resulting vector from the discard_df function.
#'
#' @return a clean data set with the columns subject, MTurk, X7:X726
clean_redraw <- function(redraw, discard) {
  redraw <- redraw |>
    dplyr::filter(X3 %in% discard) |>
    dplyr::rename(subject = X1,
                  MTurk = X3) |>
    dplyr::select(- c(X2, X4, X5, X6))

  return(redraw)
}
