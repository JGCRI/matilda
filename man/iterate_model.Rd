% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iterate_model.R
\name{iterate_model}
\alias{iterate_model}
\title{Iterate Hector Runs}
\usage{
iterate_model(core, params, save_years = NULL, save_vars = NULL)
}
\arguments{
\item{core}{A core object to initiate Hector runs.}

\item{params}{A data frame object containing parameter values.}

\item{save_years}{Range of years to save in the output data frame. Default is
set to save entire year range 1745:2300.}

\item{save_vars}{Identifiers corresponding to variables to fetch and save in
the output data frame. Variable identifiers are provided as functions. The
default list of variables to fetch is CO2 concentration, total radiative forcing,
CO2 forcing, and global mean air temperature anomaly.}
}
\value{
A data frame with a column of \code{run_number} indicating the total
number of Hector runs completed. Values for the variables and year ranges
identified in the user defined \code{metric} and \code{criterion}.
}
\description{
This function runs Hector in an iterative process with parameter
uncertainty. Parameter values for each Hector run are set using the output of
the \code{\link{generate_params}} function. Output of model iterations will
be filtered according to metric information.
}
\examples{
options("matilda.verbose" = TRUE)
# Load scenario file and initiate a new Hector core
ssp245 <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ssp245)

# Compute parameter values for Hector iterations
params <- generate_params(core, 10)
params

# Iterate Hector runs with parameter uncertainty
h_result <- iterate_model(core, params,
  save_years = 1900:2100,
  save_vars = c(GLOBAL_TAS(), CONCENTRATIONS_CO2())
)
head(h_result)
}
