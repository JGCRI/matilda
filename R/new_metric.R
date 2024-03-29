#' Creating new metric object
#'
#' @description This function creates and stores a new set of metric parameters
#' for running \code{\link{iterate_model}} and other functions in \code{matilda}.
#'
#' @param var A character string of Hector variables
#' @param years A range of years
#' @param op An operation to apply to data (e.g. mean, median, max, min, etc.)
#'
#' @return Prints \code{R} object containing metric parameters.
#' @export
#'
#' @examples
#' # Creating a new metric to produce an output for mean global temperature increase
#' # for years between 2000 and 2100
#' new_metric(GLOBAL_TAS(), years = 2000:2100, op = mean)
new_metric <- function(var, years, op) {
  # creating a list of metrics to store in object
  m <- list(
    var = var,
    years = years,
    op = op,
    # deparse makes an unevaluated expression to character string
    op_name = deparse(substitute(op))
  )

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
  cat(
    "Probabilistic Hector Metric: ", x$op_name,
    x$var, min(x$years), " to ", max(x$years), "\n"
  )
}
