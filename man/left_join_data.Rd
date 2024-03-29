% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_politics_col.R
\name{left_join_data}
\alias{left_join_data}
\title{Left join the political orientation from the analysis data set to the redraw data set.}
\usage{
left_join_data(redraw_df, analysis_df)
}
\arguments{
\item{redraw_df}{the redraw data frame from the user input.}

\item{analysis_df}{The resulting data frame from the unique_mturk_poli function.}
}
\value{
The redraw data set with a new column that has the participants corresponding political orientation.
}
\description{
Left join the political orientation from the analysis data set to the redraw data set.
}
