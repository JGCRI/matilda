#' Creating New Criterion Object
#'
#' @description This function will create a new criterion used to screen Hector
#' runs for realism. User can load data for comparing Hector variable outputs to
#' observed data values.
#'
#' @param var A variable from Hector output to compare with observed data
#' @param years A range of years from observed data
#' @param obs_values Numeric values from observed data
#'
#' @return Pretty print of R object containing new criterion parameters.
#' @export
#'
#' @examples
#' # Assign observed data to an object
#' my_criterion <- new_criterion(GLOBAL_TAS(),
#'                              years = 1951:2000,
#'                              obs_values = seq(0.4, 1.0, length.out = 50))
#'
#' # View printed criterion
#' my_criterion

new_criterion <- function(var, years, obs_values) {

  crit <- list(var = var,
               years = years,
               obs_values = obs_values)

  class(crit) <- "criterion"

  return(crit)

}

#' Print new criterion
#'
#' @description This prints the screening criterion provided as \code{x}.
#'
#' @param x Criterion object created in \code{\link{new_criterion}}
#' @param ... Other arguments to or from other methods.
#'
#' @return Printed version of criterion stored in \code{R} object.
#' @export

print.criterion <- function(x, ...) {

  cat("Criterion for screening Hector: ",
      x$var, min(x$years), " to ", max(x$years), "\n")

}

#' Is an object of type criterion?
#'
#' @description Checks whether \code{x} has an attribute class of \code{"criterion"}
#'
#' @param x an \code{R} object.
#'
#' @return \code{is.crit} returns \code{TRUE} or \code{FALSE} depending on whether \code{x}
#' has an attribute class of \code{"criterion"} or not.
#' @export
#'
#' @examples
#' x <- criterion_co2_obs()
#'
#' # Is x of class 'criterion'?
#' is.criterion(x)

is.criterion <- function(x) {

  isa(x, "criterion")

}

#' Screening criterion using Mauna Loa atmospheric CO2
#'
#' @description This is a criterion identifier for screening Hector runs using
#' observed CO2 levels (ppm) from the Mauna Loa data set. The data set provides the
#' longest running measurements of mean annual atmospheric CO2, collected from the
#' Mauna Loa Observatory.
#'
#' @return A criterion identifier using Mauna Loa atmospheric CO2
#' @note This function uses the Mauna Loa data set from
#' \href{https://gml.noaa.gov/ccgg/trends/data.html}{https://gml.noaa.gov/ccgg/trends/data.html}
#' downloaded on 01/03/2023
#' @export
#'
#' @examples
#' criterion_co2_obs()

criterion_co2_obs <- function() {

  new_criterion(CONCENTRATIONS_CO2(),
           years = metricdata_co2$year,
           obs_values = metricdata_co2$co2_ppm)

}


#' Screening criterion using HadCRUT5 mean global temperature anomaly
#'
#' @description This is a criterion identifier for screening Hector runs using
#' observed global temperature anomaly (C) from the HadCRUT5 data set. These data
#' are presented as temperature anomalies relative to 1961-1990.
#'
#' @return A criterion identifier using HadCRUT5 global temperature anomaly
#' @note This function uses the HadCRUT global annual mean temperature anomaly data
#' from \href{https://www.metoffice.gov.uk/hadobs/hadcrut5/data/current/download.html}
#' @export
#'
#' @examples
#' criterion_tas_obs()

criterion_tas_obs <- function() {

  new_criterion(GLOBAL_TAS(),
                years = metricdata_tas$year,
                obs_values = metricdata_tas$anomaly_C)
}
