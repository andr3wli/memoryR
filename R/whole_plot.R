#' Create the average redrawn plot by political orientation.
#'
#' @param img_path A string that has the path for the original plot image.
#' @param df The resulting data frame from the clean_for_plot function.
#' @param line Default set to false, add geom_line grouped by political orientation if set to true.
#'
#' @return A ggplot graph with the recreated redraw map from the recall task.
#' @export

whole_plot <- function(img_path, df, line = FALSE) {

  img <- jpeg::readJPEG(img_path)

  if (line == TRUE) {
    plot1 <- plot_with_line(df, image = img)
    return(plot1)
  } else {
    plot2 <- plot_no_line(df, image = img)
    return(plot2)
  }
}

#' Graph with the geom_line geometric.
#'
#' @param df  The data frame from the clean_from_plot function.
#' @param image The string image path.

plot_with_line <- function(df, image) {
  #img <- jpeg::readJPEG(image)
  Xmin <- 0
  Xmax <- 837
  Ymin <- 0
  Ymax <- 450

  df |>
    dplyr::filter(poli != "Independent") |>
    ggplot2::ggplot(ggplot2::aes(x = x, y = y_mean, color = poli)) +
    ggplot2::annotation_custom(grid::rasterGrob(image,
                                                width = grid::unit(1, "npc"),
                                                height = grid::unit(1, "npc")),
                               Xmin, Xmax, -Ymax, Ymin) +
    ggplot2::geom_line(ggplot2::aes(group = poli)) +
    ggplot2::geom_smooth(ggplot2::aes(group = poli)) +
    ggplot2::scale_color_manual(values = c("blue", "red")) +
    ggplot2::xlim(c(Xmin, Xmax)) +
    ggplot2::ylim(c(Ymin, Ymax)) +
    ggplot2::scale_y_reverse() +
    ggplot2::expand_limits(y = c(Ymin, Ymax)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}

#' Graph without the geom_line geometric obeject.
#'
#' @param df The data frame from the clean_from_plot function.
#' @param image The string image path.

plot_no_line <- function(df, image) {
  #img <- jpeg::readJPEG(image)
  Xmin <- 0
  Xmax <- 837
  Ymin <- 0
  Ymax <- 450

  df |>
    dplyr::filter(poli != "Independent") |>
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
