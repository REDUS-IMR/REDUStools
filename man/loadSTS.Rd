% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imrsts.R
\name{loadSTS}
\alias{loadSTS}
\title{Load the processed REDUS time series database.}
\usage{
loadSTS(mySTS, ts = NULL)
}
\arguments{
\item{mySTS}{STS name.}

\item{ts}{timestamp. Must be in "YYMMDD.HHMMSS" or "YYMMDD" character format. Default is NULL.}
}
\value{
On a valid input a REDUS database object will be returned or NULL otherwise.
}
\description{
\code{loadSTS} returns an Rstox processed REDUS time series database.
}
\examples{
\dontrun{
loadSTS("Barents Sea Northeast Arctic cod bottom trawl index in winter", "20181215")
}

}
