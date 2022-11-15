# Goals: remove metric_calc_1run from iterative_hector. Dev new function - metric-calc.
# New metric_calc function should take the resulting df from iterative_hector,
# split it (by run number), then combine results for output with one metric value
# for each run completed in iterative_hector.

#' Metric Calculation
#'
#' @description A function that accepts a data frame result from an iterative Hector
#' run and calculates a specified metric for each run.
#'
#' @param x A data frame object from \code{\link{iterative_hector}} output.
#' @param metric An object identifying a variable, year range, and operation
#' (e.g. mean, median, max, min, etc.) to fetch from Hector result.
#'
#' ?me@return A data frame with columns for run_number, variable, and a metric
#' calculation for each Hector run.
#'
#' @export

metric_calc <- function(x, metric) {

  # error code if x is empty
  if( length(x) == 0) stop('x has no data')

  # error code if year arg exceeds years in x
  if( any(metric$years > max(x$year)) ) stop('year range exceeds years in x')

  # error code if variable arg is not in x
  if( any(metric$var != x$variable)) stop('variable is not present in x')

  # Splitting x (df of hector output) by run number
  result_split <- split(x, x$run_number)

  # calculating metric for split result using metric_calc_1run
  metric_result <- sapply(result_split, metric_calc_1run, metric)

  # result as a data frame
  df <- as.data.frame(metric_result)

  # producing run numbers
  run_number <- 1:nrow(df)

  # Adding cols for run_number
  df$run_number <- run_number

  # Ordering columns
  df <- df[,c(2, 1)]

  # output
  return(df)
}

