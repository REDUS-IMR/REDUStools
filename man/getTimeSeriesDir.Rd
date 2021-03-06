% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/common.R
\name{getTimeSeriesDir}
\alias{getTimeSeriesDir}
\title{Get the time series directory path}
\usage{
getTimeSeriesDir(type)
}
\arguments{
\item{type}{The requested type of survey time series.}
}
\value{
The absolute path to the survey time type series requested.
}
\description{
\code{getTimeSeriesDir} returns the path of the time series working directory,
which the type can be "survey" or "catch".
}
\examples{
getTimeSeriesDir("survey")

}
