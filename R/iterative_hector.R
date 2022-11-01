#' Log normal parameterization
#'
#' @description Function used to supply proper values to rlnorm() in order to produce
#' a log-normal distribution when arithmetic mean and standard deviation of a parameter are
#' supplied.
#'
#' @param m mean
#' @param sd standard deviation
#'
#' @return Mean and standard deviation parameters that can be used for appropriate
#' calculation of a log normal distribution for random draws.
#'
#'
#' @examples

lognorm <- function(m, sd){

  # re-parameterization of supplied mean value
  mn <- log(m^2 / sqrt(sd^2 + m^2))

  # re-parameterization of supplied sd value
  stdev <- sqrt(log(1 + (sd^2 / m^2)))

  # stores new value in list - when pushed to rlnorm(), will provide normal distribution
  # of arithmetic mean (m) and standard deviation (sd)
  c(mn, stdev)

}

#' Metric calc from single Hector Run
#'
#' @description Function for calculating a variable metric from Hector output data.
#'
#' @param x A data frame result from a single Hector run.
#' @param op An operation to apply to data (e.g. mean, median, max, min, etc.).
#' @param var A variable name.
#' @param years A year range.
#'
#' @return A numeric value calculated from the operation for each variable in the
#' year range.
#' @export
#'
#' @examples

metric_calc_1run <- function(x, op, var, years){

  if(any (years > max(x$year))) stop('year range must be subset of years in x')

  # subsets single hector run to only include variables and years of interest
  x <- subset(x, variable == var & year %in% years)

  # applies operation to the variable values in the Hector data frame
  # if the returned value is NA return error
  # else return metric_value
  if ( is.na(op(x$value))) stop('variable not present in x, metric_value == NA')
  else (op(x$value))

}

#' Iterative Hector Runs
#'
#' @description Runs Hector in an iterative process with parameter uncertainty.
#'
#' @param core A core object to initiate Hector runs.
#' @param var A variable name.
#' @param years A year range.
#' @param runs A numeric value indicating the number of Hector runs to complete.
#' @import hector
#' @return A data frame with added columns indicating parameter values used for
#' each Hector run, metric value for each Hector run, and a run_number from one
#' to the total number of Hector runs completed.
#' @export
#'
#' @examples
iterative_hector <- function(core, var, years, params) {

  # store results
  result_list <- list()

  # set number of model iterations

  for(i in colnames(params)) {

    # set var
    setvar(core, NA, do.call(i, list()), params[i][[1]], unit = param_units[i])

    # resets model after each run
    reset(core, date = 0)

    # run the model
    run(core)

    # fetch model results based on function arguments provided by the user
    # Stores in object 'dat'
    dat <- fetchvars(core = core, dates = years, vars = var)

    # stores resulting dfs (dat) from each run in result_list()
    result_list[[i]] <- dat

  }

  # binds rows from result_list into df
  df = as.data.frame(do.call("rbind", result_list))

  # what to return?
  return(df)
}
