#' Add the political orientation from the analysis data set to the redraw data set.
#'
#' @param analysis_data The analysis data set containing the relevant demographic information.
#' @param redraw_data The redraw data set from participants the memory recall task.
#'
#' @return The redraw data set with the participants political orientation in the first column.
#' @export

add_politics_col <- function(analysis_data, redraw_data) {

  mturk_poli <- mturk_poli_data(data = analysis_data)

  unique_mturk_poli_only <- unique_mturk_poli(mturk_poli)

  join_data <- left_join_data(redraw_df = redraw_data, analysis_df = unique_mturk_poli_only)

  x <- relocate_data(join_data)

  return(x)
}

#' Find the political orientation of each participant.
#'
#' @param data The analysis data set - that which contains all the relevant demographic information.
#'
#' @return A data frame of the analysis grouped by participants with political orientation information.

mturk_poli_data <- function(data) {
  mturk_poli_data_only <- data |>
    dplyr::group_by(MTurk) |>
    dplyr::summarise(poli = Politic)
}

#' Get the political orientation for each unique participant via the MTurk code.
#'
#' @param data The data set created by the mturk_poli_data function.
#'
#' @return A data frame with the unique MTurk code and the corresponding political orientation.

unique_mturk_poli <- function(data) {
  unique_data <- unique(data, by = "MTurk")
}

#' Left join the political orientation from the analysis data set to the redraw data set.
#'
#' @param redraw_df the redraw data frame from the user input.
#' @param analysis_df The resulting data frame from the unique_mturk_poli function.
#'
#' @return The redraw data set with a new column that has the participants corresponding political orientation.

left_join_data <- function(redraw_df, analysis_df) {
  redraw <- dplyr::left_join(redraw_df, analysis_df, by = "MTurk")
}

#' Relocate the the poli column to be the first column of the data set.
#'
#' @param data The data frame produced by the left_join_data function.
#'
#' @return Exactly the same data frame as the df produced from left_join_data but the poli column has been relocated.

relocate_data <- function(data) {
  redraw <- data |> dplyr::relocate(poli)
}
