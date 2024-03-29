% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_metric.R
\name{new_metric}
\alias{new_metric}
\title{Creating new metric object}
\usage{
new_metric(var, years, op)
}
\arguments{
\item{var}{A character string of Hector variables}

\item{years}{A range of years}

\item{op}{An operation to apply to data (e.g. mean, median, max, min, etc.)}
}
\value{
Prints \code{R} object containing metric parameters.
}
\description{
This function creates and stores a new set of metric parameters
for running \code{\link{iterate_model}} and other functions in \code{matilda}.
}
\examples{
# Creating a new metric to produce an output for mean global temperature increase
# for years between 2000 and 2100
new_metric(GLOBAL_TAS(), years = 2000:2100, op = mean)
}
