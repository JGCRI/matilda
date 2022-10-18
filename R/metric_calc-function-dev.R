# Goals: remove metric_calc_1run from iterative_hector. Dev new function - metric-calc.
# New metric_calc function should take the resulting df from iterative_hector,
# split it (by run number?), then combine results for output with one metric value
# for each run completed in iterative_hector.

## See issue from Ben in github repo for suggestions.
## based on description - look at code for metric_calc_bin to apply split and
## sapply - I think this is what Ben is looking for.

## Run ex_iterative_hector.R to get a data output to help with metric_calc

#' Metric Calculation
#'
#' @description A function that accepts a data frame result from an iterative Hector
#' run and calculates a specified metric for each run.
#'
#' @param result A data frame object from `iterative_hector` output
#' @param op An operation to be applied to the climatic variable values (e.g.,
#' mean, median, max, min)
#' @param var A character string of the variable name to apply the operation.
#' @param years A year range.
#'
#' @return A table with a metric calculation for each Hector run.
#' @export
#'
#' @examples
metric_calc <- function(result, op, var, years) {
  result_split <- split(result, result$run_number)
  metric_result <- sapply(result_split, metric_calc_1run, op, var, years)

  # output
  df <- as.data.frame(metric_result)

}

# The above chunk does not complete the entire goal yet -- but it does use the
# result of an iterative_hector run and calculates the metric for each run.
# Want to add columns with row numbers and the variable name.
# running into a few issues, not sure why. Probably just been thinkning about it
# too much. Solution is probably super simple.
