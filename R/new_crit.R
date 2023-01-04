#' Creating New Criterion Object
#'
#' @param var A variable from Hector output to compare with observed data
#' @param years A range of years from observed data
#' @param obs_values Numeric values from observed data
#'
#' @return Pretty print of R object containing new criterion parameters.
#' @export
#'
#' @examples
#' x <- hectorpractice:::metricdata_co2
#' new_crit(ATMOSPHERIC_CO2(), years = x$year, obs_values = x$co2_ppm)

new_crit <- function(var, years, obs_values) {

  crit <- list(var = var,
               years = years,
               obs_values = obs_values)

  class(crit) <- "criterion"

  return(crit)

}

#' Print new criterion
#'
#' @param x Criterion object created in \code{\link{new_crit}}
#' @param ... Other arguments to or from other methods.
#'
#' @return Pretty print of criterion stored in R object.
#' @export
#'
#' @examples
#' hectorpractice:::print.crit(crit_co2_obs())

print.crit <- function(x, ...) {

  cat("Criterion for screening Hector: ",
      x$var, min(x$years), " to ", max(x$years), "\n")

}

#' Is an object of type criterion?
#'
#' @description Checks whether the supplied argument has an attribute class of criterion
#'
#' @param x an R object.
#'
#' @return TRUE if x has an attribute class of criterion, and FALSE otherwise.
#' @export
#'
#' @examples
#' x <- crit_co2_obs()
#' is.crit(x)

is.crit <- function(x) {

  isa(x, "criterion")

}

#' Screening criterion using Mauna Loa atmospheric CO2
#'
#' @description This data set gives the longest running measurements of mean annual
#' atmospheric CO2 levels (ppm) from the Mauna Loa Observatory.
#'
#' @return A criterion using Mauna Loa atmospheric CO2
#' @note This uses the Mauna Loa data set from
#' \href{https://gml.noaa.gov/ccgg/trends/data.html}{https://gml.noaa.gov/ccgg/trends/data.html}
#' downloaded on 01/03/2023
#' @export
#'
#' @examples
#' crit_co2_obs()

crit_co2_obs <- function() {

  new_crit(ATMOSPHERIC_CO2(),
           years = metricdata_co2$year,
           obs_values = metricdata_co2$co2_ppm)

}
