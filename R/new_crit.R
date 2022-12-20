# Goal: function that screens hector runs from a result, assigns boolean for whether
# each times step fails to fall within MLO obs CO2 range.
#
# Will want to calc % of values in each run that fall outside obs range
# (inidcated by fail == 'TRUE') and if greater than specifed percent (maybe 33%
# based on leeyas C tracking), drop run.

#' New Criterion Object
#'
#' @param var
#' @param years
#' @param op
#'
#' @return
#' @export
#'
#' @examples
new_crit <- function(var, years, obs_values) {

  crit <- list(var = var,
            years = years,
            obs_values = obs_values)

  class(crit) <- "criterion"

  return(crit)

}

#' Pretty print of new criterion
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.criterion <- function(x, ...) {

  cat("Criterion for screening Hector: ",
      x$var, min(x$years), " to ", max(x$years), "\n")

}


#' Screening criterion using Mauna Loa atmospheric CO2
#'
#' @return A criterion using Mauna Loa atmospheric CO2
#' @export
#'
#' @examples
#' crit_CO2_obs()

crit_CO2_obs <- function() {

  new_crit(ATMOSPHERIC_CO2(), years = mlo$year, obs_values = mlo$mean)

}
