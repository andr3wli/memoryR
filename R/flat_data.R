#' Clean and manipulate the data from add_salient_col to be used for statistical analysis.
#'
#' @param df The data set provided by the make_clean_redraw function.
#'
#' @return A data set with only flat data that is ready for statistical tests.
#' @export

flat_data <- function(df) {

  # filter data set so that we only have flat rows
  flat_df <- df |>
    dplyr::filter(salient == "Flat") |>
    tidyr::drop_na(c(x_axis, y_axis))

  # iterate and create the nested model objects
  flat_df <- flat_df |>
    dplyr::group_by(poli, subject, salient, MTurk) |>
    tidyr::nest() |>
    dplyr::mutate(model = purrr::map(data, ~lm(formula = y_axis ~x_axis, data = .x) |>
                                       broom::tidy()))
  # unnest the model column
  flat_df <- flat_df |>
    tidyr::unnest(model) |>
    dplyr::select(-c("std.error", "statistic", "p.value"))

  # make the data wider from the model object
  flat_df <- flat_df |>
    tidyr::pivot_wider(names_from = term, values_from = estimate) |>
    dplyr::rename(intercept = `(Intercept)`,
                  slope = x_axis)

  # unnest the data column
  flat_df <- flat_df |>
    tidyr::unnest(data)

  # group by and summarize to get the final flat data set
  flat_df <- flat_df |>
    dplyr::group_by(poli, salient, subject) |>
    dplyr::summarise(mean_y = mean(y_axis, na.rm = TRUE),
                     mean_int = mean(intercept, na.rm = TRUE),
                     mean_slope = abs(mean(slope, na.rm = TRUE)))

  return(flat_df)
}
