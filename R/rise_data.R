#' Clean and manipulate the data from add_salient_col to be used for statistical analysis.
#'
#' @param df The data set provided by the make_clean_redraw function.
#'
#' @return A data set with only rise data that is ready for statistical tests.
#' @export

rise_data <- function(df) {
  # filter the data from the add_salient_col so that we only have the rise data
  rise_df <- df |>
    dplyr::filter(salient == "Rising") |>
    tidyr::drop_na(c(x_axis, y_axis))

  # nest and run regression over every subject grouped by political orientation
  rise_df <- rise_df |>
    dplyr::group_by(poli, subject, salient, MTurk) |>
    tidyr::nest() |>
    dplyr::mutate(model = purrr::map(data, ~lm(formula = y_axis ~ x_axis, data = .x) |>
                                       broom::tidy()))

  # unnest the model
  rise_df <- rise_df |>
    tidyr::unnest(model) |>
    dplyr::select(-c("std.error", "statistic", "p.value"))

  # make data wider from the newly unnested model
  rise_df  <- rise_df  |>
    tidyr::pivot_wider(names_from = term, values_from = estimate) |>
    dplyr::rename(intercept = `(Intercept)`,
                  slope = x_axis)

  # unnest the data column
  rise_df  <- rise_df  |>
    tidyr::unnest(data)

  # finally, summarise the data so that it can be ussed for statistical tests
  rise_df  <- rise_df  |>
    dplyr::group_by(poli, salient, subject) |>
    dplyr::summarise(mean_y = mean(y_axis, na.rm = TRUE),
                     mean_int = mean(intercept, na.rm = TRUE),
                     mean_slope = abs(mean(slope, na.rm = TRUE)))

  return(rise_df)
}
