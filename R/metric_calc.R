# Goals: remove metric_calc_1run from iterative_hector. Dev new function - metric-calc.
# New metric_calc function should take the resulting df from iterative_hector,
# split it (by run number), then combine results for output with one metric value
# for each run completed in iterative_hector.

## See issue from Ben in github repo for suggestions.
## based on description - look at code for metric_calc_bin to apply split and
## sapply

## Run ex_iterative_hector.R to get a data output to help with viewing metric_calc
## functionality.

#' Metric Calculation
#'
#' @description A function that accepts a data frame result from an iterative Hector
#' run and calculates a specified metric for each run.
#'
#' @param x A data frame object from iterative_hector() output.
#' @param op An operation to be applied to the climatic variable values (e.g.,
#' mean, median, max, min).
#' @param var A character string indicating which variable to apply the operation.
#' @param years A year range.
#'
#' @return A data frame with columns for run_number, variable, and a metric
#' calculation for each Hector run.
#'
#' @export
#'
#' @examples
metric_calc <- function(x, op, var, years) {

  # Splitting x (df of hector output) by run number
  result_split <- split(x, x$run_number)

  # calculating metric for split result using metric_calc_1run
  metric_result <- sapply(result_split, metric_calc_1run, op, var, years)

  # result as a data frame
  df <- as.data.frame(metric_result)

  # producing run numbers and variable names
  run_number <- rep(c(1:nrow(df)))
  variable <- rep(c(var), each = nrow(df))

  # Adding cols for run_number and variable
  df$variable <- variable
  df$run_number <- run_number

  # Ordering columns
  df <- df[,c(3, 2, 1)]

  # output
  return(df)
}


## Note
## I have no idea if the code to produce variable names for the variable column
## will work if the user attempts to run multiple this code on multiple variables
## from the iterative_hector output.
##
## This will be something I can check on a little later. But will probably be important.
## It is also probably easy enough to fix.
