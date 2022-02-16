#' Create plots that are salient or rising only.
#'
#' @param img_path A string with the image path.
#' @param df The data set created from the clean_for_plot function.
#' @param type One of flat or rise. If flat, data points <= the midpoint of the x will be plotted.
#'
#' @return A ggplot graph with either data <= the midpoint or >= the midpoint.
#' @export

half_plot <- function(img_path, df, type = "flat") {

  img <- jpeg::readJPEG(img_path)
  mid_point <- 366

  if (type == "flat") {
    flat_plot(df, image = img, mid = mid_point)
  } else if (type == "rise") {
    rise_plot(df, image = img, mid = mid_point)
  } else {
    print("type needs to be one of flat or rise.")
  }
}

#' Makes the plot where the salient column is <= the mid point.
#'
#' @param df The data set created from the clean_for_plot function.
#' @param image A string with the image path.
#' @param mid The mid point: 366
#'
#' @return A ggplot graph with the first half of the data aka the flat portion of the data.

flat_plot <- function(df, image, mid) {
  Xmin <- 0
  Xmax <- 837
  Ymin <- 0
  Ymax <- 450
  df |>
    dplyr::filter(poli != "Independent") |>
    dplyr::filter(x <= mid) |>
    ggplot2::ggplot(ggplot2::aes(x = x, y = y_mean, color = poli)) +
    ggplot2::annotation_custom(grid::rasterGrob(image,
                                                width = grid::unit(1, "npc"),
                                                height = grid::unit(1, "npc")),
                               Xmin, Xmax, -Ymax, Ymin) +
    ggplot2::geom_smooth(ggplot2::aes(group = poli)) +
    ggplot2::scale_color_manual(values = c("blue", "red")) +
    ggplot2::xlim(c(Xmin, Xmax)) +
    ggplot2::ylim(c(Ymin, Ymax)) +
    ggplot2::scale_y_reverse() +
    ggplot2::expand_limits(y = c(Ymin, Ymax)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}

#' Makes the plot where the salient column is >= the mid point.
#'
#' @param df The data set created from the clean_for_plot function.
#' @param image A string with the image path.
#' @param mid The mid point: 366
#'
#' @return A ggplot graph with the last half of the data aka the rise portion of the data.

rise_plot <- function(df, image, mid) {
  Xmin <- 0
  Xmax <- 837
  Ymin <- 0
  Ymax <- 450
  df |>
    dplyr::filter(poli != "Independent") |>
    dplyr::filter(x >= mid) |>
    ggplot2::ggplot(ggplot2::aes(x = x, y = y_mean, color = poli)) +
    ggplot2::annotation_custom(grid::rasterGrob(image,
                                                width = grid::unit(1, "npc"),
                                                height = grid::unit(1, "npc")),
                               Xmin, Xmax, -Ymax, Ymin) +
    ggplot2::geom_smooth(ggplot2::aes(group = poli)) +
    ggplot2::scale_color_manual(values = c("blue", "red")) +
    ggplot2::xlim(c(Xmin, Xmax)) +
    ggplot2::ylim(c(Ymin, Ymax)) +
    ggplot2::scale_y_reverse() +
    ggplot2::expand_limits(y = c(Ymin, Ymax)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}
