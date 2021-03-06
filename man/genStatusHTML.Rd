% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeseries.R
\name{genStatusHTML}
\alias{genStatusHTML}
\title{Generate status matrix for the survey timeseries processing.}
\usage{
genStatusHTML(timeSeriesStatus, series = "", year = "", item = "")
}
\arguments{
\item{timeSeriesStatus}{return from loadTimeSeriesStatus().}

\item{series}{specific series. Default "".}

\item{year}{specific year. Default ""}

\item{item}{specific item. Default ""}
}
\description{
\code{genStatusHTML} generates and returns HTML status matrix.
}
\examples{
genStatusHTML(loadTimeSeriesStatus())
}
