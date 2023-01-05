#' Creating new metric object
#'
#' @description This creates and stores a new set of metric parameters for running
#' \code{\link{iterate_hector}} and other functions in \code{hectorpractice}
#' that require the parameters stored in the object.
#'
#' @param var A character string of Hector variable
#' @param years A range of years
#' @param op An operation to apply to data
#'
#' @return Prints \code{R} object containing metric parameters.
#' @export
#'
#' @examples
#' new_metric(GLOBAL_TAS(), years = 2000:2100, op = mean)

new_metric <- function(var, years, op){

  # creating a list of metrics to store in object
  m <- list(var = var,
            years = years,
            op = op,
            # deparse makes an unevaluated expression to character string
            op_name = deparse(substitute(op)))

  # subscribe class attribute to m and store in h_metric?
  class(m) <- "h_metric"

  return(m)

}

#' Printing new metric
#'
#' @description Prints the Hector metric provided as \code{x}.
#'
#' @param x An \code{R} object storing the Hector metric.
#' Built using \code{\link{new_metric}}.
#' @param ... Other arguments passed to or from other methods.
#'
#' @return Printed version of metrics stored in \code{R} object.
#' @export

print.h_metric <- function(x, ...) {

  cat("Probabilistic Hector Metric: ", x$op_name,
      x$var, min(x$years), " to ", max(x$years), "\n")

}
