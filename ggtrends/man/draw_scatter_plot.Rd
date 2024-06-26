% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_visualize.R
\name{draw_scatter_plot}
\alias{draw_scatter_plot}
\title{Draw a scatter plot of values for different types across time ranges}
\usage{
draw_scatter_plot(data)
}
\arguments{
\item{data}{A data frame containing `range_of_years`, `value`, and `type` columns.}
}
\value{
A ggplot object showing the scatter plot.
}
\description{
This function creates a scatter plot showing the values for different types across different time ranges.
}
\examples{
\dontrun{
data <- data.frame(range_of_years = rep(c("2004-2006", "2007-2009"), each = 5),
                   value = rnorm(10), type = rep(c("A", "B"), 5))
draw_scatter_plot(data)
}
}
