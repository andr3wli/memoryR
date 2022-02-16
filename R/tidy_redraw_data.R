#' Turn the redraw data with political orientation into something that is tidy
#'
#' @param data The data from produced by add_politics_col function
#'
#' @return A data frame that has 5 unique column names: poli, subject, MTurk, x, and y.
#' @export

tidy_redraw_data <- function(data) {

  # create new data set with only x and y coordinates
  data_only <- data |> dplyr::select(-c(poli, subject, MTurk))
  # create new data set without the x and y coordinates
  subject_only <- data |> dplyr::select(poli, subject, MTurk)

  # convert all data into numbers
  numeric_data <- purrr::map_df(data_only, as.numeric)

  # only keep those that are less then 1000 (this drops all information about the time)
  over_1000_data <- numeric_data |> dplyr::select_if(~any(. < 1000))

  # iterate over and rename the col names: x for odd and y for even
  xy_col <- change_col_names(data = over_1000_data)

  # bind the seperate data and subject sets together
  all_data <- cbind(subject_only, xy_col)

  return(all_data)
}

#' Iterate over the data frame and rename the default R columns so that it is named only x or y.
#'
#' @param data The over_1000_data variable.
#'
#' @return A data frame that with the 5 unique column names and no values are less then 1000

change_col_names <- function(data) {
  for (j in seq_along(data)) {
    if (j %% 2 == 0){
      names(data)[j] <- "y"
    } else (
      names(data)[j] <- "x"
    )
  }
  return(data)
}
