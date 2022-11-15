#' Creating new metric object
#'
#' @description Stores metric values in an object for easily interchanging new
#' metrics for functions within hectorpractice.
#'
#' @param var A character string of Hector variable
#' @param years A range of years
#' @param op An operation to apply to data
#'
#' @return Pretty print of R object containing new metric values.
#' @export

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
#' @description Prints Hector metric objects produced using.
#' \code{\link{new_metric}}
#'
#' @param x R object storing class attributes.
#' @param ... Other arguments passed to or from other methods.
#'
#' @return Pretty print of metrics stored in R object.
#' @export

print.h_metric <- function(x, ...) {

  cat("Probabilistic Hector Metric: ", x$op_name,
      x$var, min(x$years), " to ", max(x$years), "\n")

}
