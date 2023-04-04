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

lognorm <- function(m, sd){

  # re-parameterization of supplied mean value
  mn <- log(m^2 / sqrt(sd^2 + m^2))

  # re-parameterization of supplied sd value
  stdev <- sqrt(log(1 + (sd^2 / m^2)))

  # stores new value in list - when pushed to rlnorm(), will provide normal distribution
  # of arithmetic mean (m) and standard deviation (sd)
  c(mn, stdev)

}

#' Metric calculation for single Hector run
#'
#' @description This function calculates a metric variable using Hector output data.
#'
#' @param x A data frame result from a single Hector run.
#' @param metric An object identifying a variable, year range, and operation
#' (e.g. mean, median, max, min, etc.) to apply to data.
#'
#' @return A numeric value calculated from information defined in \code{metric}
#' object.
#' @export

#' @examples
#' # Load scenario file and initiate a Hector core
#' ssp245 <- system.file("input/hector_ssp245.ini", package = "hector")
#' core <- newcore(ssp245)
#'
#' # Run Hector
#' run(core)
#'
#' # Fetch Hector results
#' h_result <- fetchvars(core, dates = 2000:2300)
#'
#' # Create a new metric
#' metric <- new_metric(GLOBAL_TAS(), years = 2000:2100, op = mean)
#' print(metric)
#'
#' # Calculate the metric value from Hector results
#' metric_calc_1run(h_result, metric)

metric_calc_1run <- function(x, metric) {

  if(any (metric$years > max(x$year))) stop('year range must be subset of years in x')

  # subsets single hector run to only include variables and years of interest
  x <- subset(x, x$variable == metric$var & x$year %in% metric$years)

  # applies operation to the variable values in the Hector data frame
  # if the returned value is NA return error
  # else return metric_value
  if ( is.na(metric$op(x$value))) {
    stop('variable not present in x, metric_value == NA')
  } else {
    return (metric$op(x$value))
  }
}

#' Iterate Hector Runs
#'
#' @description This function runs Hector in an iterative process with parameter
#' uncertainty. Parameter values for each Hector run are set using the output of
#' the \code{\link{generate_params}} function. Output of model iterations will
#' be filtered according to metric information.
#'
#' @param core A core object to initiate Hector runs.
#' @param params A data frame object containing parameter values.
#'
#' @import hector
#'
#' @importFrom stats rnorm rlnorm
#'
#' @return A data frame with a column of \code{run_number} indicating the total
#' number of Hector runs completed. Values for the variables and year ranges
#' identified in the user defined \code{metric} and \code{criterion}.
#'
#' @export
#'
#' @examples
#' # Load scenario file and initiate a new Hector core
#' ssp245 <- system.file("input/hector_ssp245.ini", package = "hector")
#' core <- newcore(ssp245)
#'
#' # Compute parameter values for Hector iterations
#' params <- generate_params(core, 10)
#' params
#'
#' # Iterate Hector runs with parameter uncertainty
#' h_result <- iterate_hector(core, params)
#' head(h_result)

iterate_hector <- function(core,
                           params,
                           save_years = NULL,
                           save_vars = NULL) {
  # store results
  result_list <- list()

  # iterate hector across all param values
  for (i in seq_len(nrow(params))) {

    # If ncol == 1, parameter names need to be added to establish correct input
    # for set_params()
    if (ncol(params) == 1) {

      # convert params to numeric vector
      single_param_values <- params[, 1]

      # Get names associated with each value in the vector
      # This is the name of the parameter
      names(single_param_values) <-
        rep(colnames(params), length(single_param_values))

      # set variable values -- needs core and numeric param values
      set_params(core, single_param_values)

    # If ncol > 1, unlist parameters, names are correctly set in set_params()
    } else {

      # convert params to numeric
      params_i <- unlist(params [i,])

      # set variable values -- needs core and numeric param values
      set_params(core, params_i)

    }

    # If an error is encounter, complete run and then produce message
    tryCatch({
      # resets model after each run
      reset(core, date = 0)

      # run the model
      run(core)

      # fetch model results based on function arguments provided by the user
      # if save_years is null, fetch full date range in core
      if (is.null(save_years)) {
        save_years <- core$strtdate:core$enddate
      }

      # if save_vars is null, fetch all variables
      if (is.null(save_vars)) {
        dat <- fetchvars(core = core,
                         dates = save_years)
      }

      # otherwise fetch the years and variables specified by the user
      else {
        dat <- fetchvars(core = core,
                         dates = save_years,
                         vars = save_vars)
      }

      # adding run_number column to metric_data
      dat$run_number <- i

      # stores results
      result_list[[i]] <- dat
    },
    error = function(e) {
      message("An error occurred")
    })
  }
  # concatenate list entries into a data frame and return
  do.call("rbind", result_list)
}
