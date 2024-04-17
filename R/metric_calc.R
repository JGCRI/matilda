#' Metric Calculation
#'
#' @description This is a function that calculates metric values for multiple
#' Hector runs. Metrics to be calculated from Hector runs are defined using
#' \code{\link{new_metric}}.
#'
#' @param x A data frame object from \code{\link{iterate_model}} output.
#' @param metric An object identifying a variable, year range, and operation
#' (e.g. mean, median, max, min, etc.) to fetch from Hector result.
#'
#' @return A data frame with columns for \code{run_number} and
#'  \code{metric_result} values for each Hector run.
#'
#' @export
#'
#' @examples
#' # Create new metric
#' metric <- new_metric(var = "global_tas", years = 2000:2100, op = mean)
#'
#' # Calculate metric values for each Hector run
#' metric_calc(matilda_result, metric)
metric_calc <- function(x, metric) {
  # error code if x is empty
  if (length(x) == 0) stop("x has no data")

  # error code if year arg exceeds years in x
  if (any(metric$years > max(x$year))) stop("year range exceeds years in x")

  # Splitting x (df of hector output) by run number
  result_split <- split(x, x$run_number)

  # calculating metric for split result using metric_calc_1run
  metric_result <- sapply(result_split, metric_calc_1run, metric)

  # result as a data frame
  df <- as.data.frame(metric_result)

  # producing run numbers
  run_number <- unique(x$run_number)

  # Adding cols for run_number
  df$run_number <- run_number

  # Reorder columns and return
  df[, c(2, 1)]
}
