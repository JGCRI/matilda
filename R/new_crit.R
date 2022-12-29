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
#' new_crit(ATMOSPHERIC_CO2(), years = mlo$year, obs_values = mlo$mean)

new_crit <- function(var, years, obs_values) {

  crit <- list(var = var,
               years = years,
               obs_values = obs_values)

  class(crit) <- "criterion"

  return(crit)

}

#' Pretty print of new criterion
#'
#' @param x Criterion object created in \code{\link{new_crit}}
#' @param ...
#'
#' @return Pretty print of criterion stored in R object.
#' @export
#'

print.criterion <- function(x, ...) {

  cat("Criterion for screening Hector: ",
      x$var, min(x$years), " to ", max(x$years), "\n")

}

#' Screening criterion using Mauna Loa atmospheric CO2
#'
#' @return A criterion using Mauna Loa atmospheric CO2
#' @note This uses the Mauna Loa on 'URL' on 'DATE'
#' @export
#'
#' @examples
#' crit_co2_obs()

crit_co2_obs <- function() {

  new_crit(ATMOSPHERIC_CO2(),
           years = metricdata_co2$year,
           obs_values = metricdata_co2$co2_PgC)

}
