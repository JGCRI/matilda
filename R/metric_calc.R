#' Metric Calculation
#'
#' @description A function that accepts a data frame result from an iterative Hector
#' run and calculates a specified metric for each run.
#'
#' @param x A data frame object from \code{\link{iterate_hector}} output.
#' @param metric An object identifying a variable, year range, and operation
#' (e.g. mean, median, max, min, etc.) to fetch from Hector result.
#'
#' @return A data frame with columns for run_number, variable, and a metric
#' calculation for each Hector run.
#'
#' @export
#'
#' @examples
#' # Load scenario file and initiate a new hector core
#' ssp245 <- system.file("input/hector_ssp245.ini", package = "hector")
#' core <- newcore(ssp245)
#'
#' # Create new metric
#' metric <- new_metric(GLOBAL_TAS(), years = 2000:2100, op = mean)
#'
#' # Compute parameter values for Hector iterations
#' params <- generate_params(10)
#'
#' # Iterate Hector runs with parameter uncertainty
#' h_result <- iterate_hector(core, metric, params)
#'
#' # Calculate metric values for each Hector run
#' metric_calc(h_result, metric)

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

