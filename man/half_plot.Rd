% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/half_plot.R
\name{half_plot}
\alias{half_plot}
\title{Create plots that are salient or rising only.}
\usage{
half_plot(img_path, df, type = "flat")
}
\arguments{
\item{img_path}{A string with the image path.}

\item{df}{The data set created from the clean_for_plot function.}

\item{type}{One of flat or rise. If flat, data points <= the midpoint of the x will be plotted.}
}
\value{
A ggplot graph with either data <= the midpoint or >= the midpoint.
}
\description{
Create plots that are salient or rising only.
}
