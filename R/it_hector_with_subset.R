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
#' @export
#'
#' @examples

lognorm <- function(m, sd){
  # re-parameterization of supplied mean value
  mn <- log(m^2 / sqrt(sd^2 + m^2))
  # re-parameterization of supplied sd value
  stdev <- sqrt(log(1 + (sd^2 / m^2)))
  # stores new value in lst - when pushed to rlnorm(), will provide normal distribution
  # of arithmetic mean (m) and standard deviation (sd)
  v <- c(mn, stdev)
}

#' Metric calc from single Hector Run
#'
#' @description Function for calculating a variable metric from Hector output data.
#'
#' @param df_1run A data frame result from a single Hector run.
#' @param op An operation to apply to data (e.g. mean, median, max, min, etc.).
#' @param var A variable name.
#' @param years A year range.
#'
#' @return A numeric value calculated from the operation for each variable in the
#' year range.
#' @export
#'
#' @examples

metric_calc_1run <- function(df_1run, op, var, years){
  # subsets single hector run to only include variables and years of interest
  df_1run <- subset(df_1run, variable == var & year %in% years)
  # applies operation to the variable values in the Hector data frame
  op(df_1run$value)
}

#' Iterative Hector Runs
#'
#' @description Runs Hector in an iterative process with parameter uncertainty.
#'
#' @param core A core object to initiate Hector runs.
#' @param op An operation to be applied to Hector data (e.g. mean, median, max, min).
#' @param var A variable name.
#' @param years A year range.
#' @param runs A numeric value indicating the number of Hector runs to complete.
#'
#' @return A data frame with added columns indicating parameter values used for
#' each Hector run, metric value for each Hector run, and a run_number from one
#' to the total number of Hector runs completed.
#' @export
#'
#' @examples
iterative_hector <- function(core, op, var, years, runs = 20) {

  # where store results?
  result_list <- list()

  # set number of model iterations
  for(i in 1:runs) {

    # produces a normal distribution and sampling 1 value randomly for each run
    beta = rnorm(1, mean = 0.54, sd = 0.1)
    q10 = rlnorm(1,lognorm(2, 1.0) [1], lognorm(2, 1.0) [2])
    npp_flux0 = rnorm(1, mean = 56.2, sd = 14.3)
    aero_scale = rnorm(1, mean = 1.01, sd = 0.23)

    # set variable using value randomly sampled from distribution
    setvar(core, NA, BETA(), beta, unit = "(unitless)")
    setvar(core, NA, Q10_RH(), q10, unit = "(unitless)")
    setvar(core, NA, NPP_FLUX0(), npp_flux0, unit = "Pg C/yr")
    setvar(core, NA, AERO_SCALE(), aero_scale, unit = "(unitless)")

    # resets model after each run
    reset(core, date = 0)

    # run the model
    run(core)

    # fetch model results based on function arguments provided by the user
    # Stores in object 'dat'
    dat <- fetchvars(core = core, dates = years, vars = var)

    # subset model results based on function arguments provided by user
    metric <- metric_calc_1run(dat, op, var, years)

    # add columns for new information:
    # calculated metric values
    # param values for model
    dat$beta <- beta # Adds col name to the results for each param
    dat$q10 <- q10
    dat$npp_flux <- npp_flux0
    dat$aero_scale <- aero_scale
    dat$metric <- metric # Adds col name for the metric calculated

    # stores resulting dfs (dat) from each run in result_list()
    result_list[[i]] <- dat
  }

  # binds rows from result_list into df
  df = as.data.frame(do.call("rbind", result_list))

  # computes run_numbers based on number of runs and number of records in df
  run_number <- rep(c(1:runs), each = nrow(df) / runs)

  # adds run number column (will be needed for probabilistic functions)
  df$run_number <- run_number

  # what return?
  return(df)
}
