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
new_crit <- function(var, years, op) {

  c <- list(var = var,
            years = years,
            op = op)

  class(c) <- "criterion"

  return(c)

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

  cat("Criterion for screening Hector: ", x$op_name,
      x$var, min(x$years), " to ", max(x$years), "\n")

}

