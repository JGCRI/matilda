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
#' @param save_years Range of years to save in the output data frame. Default is
#' set to save entire year range 1745:2300.
#' @param save_vars Identifiers corresponding to variables to fetch and save in
#' the output data frame. Variable identifiers are provided as functions. The
#' default list of variables to fetch is CO2 concentration, total radiative forcing,
#' CO2 forcing, and global mean air temperature anomaly.
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
#' h_result <- iterate_hector(core, params, save_years = 1900:2100,
#' save_vars = c(GLOBAL_TAS(), CONCENTRATIONS_CO2()))
#' head(h_result)

iterate_hector <- function(core, params, save_years = NULL, save_vars = NULL) {

  # Store results
  result_list <- list()

  for (i in seq_len(nrow(params))) {
    if (ncol(params) == 1) {
      single_param_vals <- params[i, ]
      single_param_vals <- setNames(single_param_vals, colnames(params))
      set_params(core, single_param_vals)
    } else {
      params_i <- unlist(params[i, ])
      set_params(core, params_i)
    }

    # Create a placeholder dataframe for the run
    dat <- data.frame(
      scenario = core$name[i],
      year = save_years,
      variable = save_vars,
      value = NA,
      units = NA,
      run_number = i
    )

    tryCatch({
      reset(core, date = 0)
      run(core)

      if (is.null(save_years)) {
        save_years <- core$strtdate:core$enddate
      }

      if (is.null(save_vars)) {
        dat <- fetchvars(core = core, dates = save_years)
      } else {
        dat <- fetchvars(core = core, dates = save_years, vars = save_vars)
      }

      dat$run_number <- i
    }, error = function(e) {
      message("An error occurred")
    })

    result_list[[i]] <- dat
  }

  # Concatenate list entries into a data frame and return
  final_result <- do.call("rbind", result_list)
  return(final_result)
}
