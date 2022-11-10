#' New Metric
#'
#' @param var
#' @param years
#' @param op
#'
#' @return
#' @export
#'
#' @examples
new_metric <- function(var, years, op){

  m <- list(var = var, years= years, op = op, op_name = deparse(substitute(op)))

  class(m) <- "h_metric"

  return(m)

}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
print.h_metric <- function(x) {

  cat("Probabilistic Hector Metric ", x$op_name,
      x$var, min(x$years), " to ", max(x$years), "\n")

}
