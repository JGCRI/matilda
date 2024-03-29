% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iterate_model.R
\name{metric_calc_1run}
\alias{metric_calc_1run}
\title{Metric calculation for single Hector run}
\usage{
metric_calc_1run(x, metric)
}
\arguments{
\item{x}{A data frame result from a single Hector run.}

\item{metric}{An object identifying a variable, year range, and operation
(e.g. mean, median, max, min, etc.) to apply to data.}
}
\value{
A numeric value calculated from information defined in \code{metric}
object.
}
\description{
This function calculates a metric variable using Hector output data.
}
\examples{
# Load scenario file and initiate a Hector core
ssp245 <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ssp245)

# Run Hector
run(core)

# Fetch Hector results
h_result <- fetchvars(core, dates = 2000:2300)

# Create a new metric
metric <- new_metric(GLOBAL_TAS(), years = 2000:2100, op = mean)
print(metric)

# Calculate the metric value from Hector results
metric_calc_1run(h_result, metric)
}
