#' Tidy and clean data for plotting and analysis
#'
#' @param path1 A string variable of the data file path for the mturk data set.
#' @param path2 A string variable for the data file path for the analysis data set.
#' @param question A string variable of the attention check question.
#'
#' @return A clean tibble that is ready for more specific data cleaning (plotting and statistical analysis).
#' @export
clean_analysis <- function(path1, path2, question) {
  mturk <- mturk_csv(path1)
  analysis <- analysis_csv(path2)
  reject_vec <- drop_failed_attention_check(question, data = analysis)
  filter_petition(data = analysis, reject = reject_vec)
}


#' Read in the mturk csv data file and drop all NAs
#'
#' @param path1 The string variable that is passed on from the clean_analysis function. The mturk data file.
#'
#' @return The mturk data frame without NA values
mturk_csv <- function(path1) {
  mturk <- readr::read_csv(path1) |>
    tidyr::drop_na()
}

#' Read in the analysis csv data frame without unused columns
#'
#' @param path2 The string variable that is passed from the clean_analysis function. The analysis data file.
#'
#' @return The analysis data frame without columns that end with signbefore and donatebefore
analysis_csv <- function(path2) {
  analysis <- readr::read_csv(path2) |>
    dplyr::select(- dplyr::ends_with(c("signbefore", "donatebefore")))
}

#' Find out which particpants failed the attention check
#'
#' @param question The string variable of the rejection question.
#' @param data The analysis data frame that contains participants' response data
#'
#' @return A numeric vector that contains particpant numbers that failed the attention check.

drop_failed_attention_check <- function(question, data) {
  attention_check <- data |>
    dplyr::filter(Question == question & Rawresponse != 5) |>
    dplyr::select(MTurk, Question, Rawresponse)

  reject_vec <- attention_check |>
    dplyr::pull(MTurk)

  return(reject_vec)
}

#' Using the numeric vector, filter out participants that failed to pass the attention check. As well, exclude other known instances of non-participant data.
#'
#' @param data The analysis data set.
#' @param reject The numeric vector created by the drop_failed_attention_check function
#'
#' @return A clean data frame that only contains data from particpants that passed the attention check.

filter_petition <- function(data, reject) {
  try(
    analysis_df <- data |>
      tidyr::drop_na(IPaddress) |>
      dplyr::filter(!(MTurk %in% reject),
                    IPaddress != "::1",
                    City != "pheomix",
                    WhichGraph != "test",
                    MTurk != 70906611)
  )
  return(analysis_df)
}
