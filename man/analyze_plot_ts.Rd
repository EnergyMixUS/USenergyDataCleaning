% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_helper_analyze_data_to_estimate.R
\name{analyze_plot_ts}
\alias{analyze_plot_ts}
\title{Function that generates a single time series plot.}
\usage{
analyze_plot_ts(
  data_ts,
  name_state,
  names_ts,
  names_to_display,
  thresholds,
  explode_here,
  explode_offset,
  check_me = FALSE
)
}
\arguments{
\item{data_ts}{tibble/data.frame of energy types (i.e. shares and/or raw
counts) or energy prices}

\item{name_state}{character string of the name of the state for which to plot}

\item{names_ts}{names of the time series variables to plot}

\item{names_to_display}{how the names should occur in the final plots (labels
etc. typically energy type names or names of the prices)}

\item{thresholds}{numeric vector of dimension equal to the number of energy
types giving the threshold values for each energy component where to
position the dummy}

\item{explode_here}{point to explode}

\item{explode_offset}{magnitude of explode}

\item{check_me}{bool; if \code{TRUE}, then this sries is checked}
}
\value{
a plot of the data: either 3x2 (for shares or raw counts) and a plot
on top; for prices a 3x1 plot
}
\description{
The time series can be either the energy types directly i.e. their shares or
the raw counts, or, the energy prices.
}
