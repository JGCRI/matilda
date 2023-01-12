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

#' Metric calc from single Hector Run
#'
#' @description Function for calculating a variable metric from Hector output data.
#'
#' @param x A data frame result from a single Hector run.
#' @param metric An object identifying a variable, year range, and operation
#' (e.g. mean, median, max, min, etc.) to apply to data.
#'
#' @return A numeric value calculated from the operation for each variable in the
#' year range.
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
  x <- subset(x, variable == metric$var & year %in% metric$years)

  # applies operation to the variable values in the Hector data frame
  # if the returned value is NA return error
  # else return metric_value
  if ( is.na(metric$op(x$value))) stop('variable not present in x, metric_value == NA')
  else (metric$op(x$value))

}

#' Iterate Hector Runs
#'
#' @description Runs Hector in an iterative process with parameter uncertainty.
#'
#' @param core A core object to initiate Hector runs.
#' @param metric An object identifying a variable, year range, and operation
#' (e.g. mean, median, max, min, etc.) to fetch from Hector result.
#' @param params A data frame object containing parameter values.
#'
#' @import hector
#'
#' @importFrom stats rnorm rlnorm
#'
#' @return A data frame with a run_number from one to the total number of Hector
#' runs completed and values for the variables and year range identified in
#' the metric argument for each Hector run.
#'
#' @export
#'
#' @examples
#' # Load scenario file and initiate a new Hector core
#' ssp245 <- system.file("input/hector_ssp245.ini", package = "hector")
#' core <- newcore(ssp245)
#'
#' # Create and new metric
#' metric <- new_metric(GLOBAL_TAS(), years = 2000:2100, op = mean)
#' print(metric)
#'
#' # Compute parameter values for Hector iterations
#' params <- generate_params(10)
#' params
#'
#' # Iterate Hector runs with parameter uncertainty
#' h_result <- iterate_hector(core, metric, params)
#' head(h_result)

iterate_hector <- function(core, metric, params) {

  # store results
  result_list <- list()

  # set number of model iterations
  for(i in seq_len(nrow(params))) {


    # convert params to numeric
    params_i <- unlist(params [i, ])

    # set variable values -- needs core and numeric param values
    set_params(core, params_i)

    # resets model after each run
    reset(core, date = 0)

    # run the model
    run(core)

    # fetch model results based on function arguments provided by the user
    dat <- fetchvars(core = core, dates = metric$years, vars = metric$var)

    # adding run_number column
    dat$run_number <- i

    # stores results
    result_list[[i]] <- dat

  }

  # binds rows from result_list into df
  df = as.data.frame(do.call("rbind", result_list))

  # what to return?
  return(df)
}

