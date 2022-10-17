#' Log norm calc
#'
#' @param m mean
#' @param sd standard deviation
#'
#' @return Mean and standard deviation parameters that can be used for appropriate calculation of a log normal distribution for random draws.
#' @export
#'
#' @examples
lognorm <- function(m, sd){
  mn <- log(m^2 / sqrt(sd^2 + m^2))
  stdev <- sqrt(log(1 + (sd^2 / m^2)))
  v <- c(mn, stdev)
}

#' Metric calc from single Hector Run
#'
#' @param df_1run A data frame result from a single Hector run.
#' @param op An operation to apply to data (e.g. mean, median, max, min, etc.).
#' @param var A variable name.
#' @param years A year range.
#'
#' @return A numeric value calculated from the operation for each variable in the year range.
#' @export
#'
#' @examples
metric_calc_1run <- function(df_1run, op, var, years){
  df_1run <- subset(df_1run, variable == var & year %in% years)
  op(df_1run$value)
}


#### Building function that will run hector for user defined run iterations ####

# The idea is to include this function in the run_ensemble() function which will accept list of scenarios

# Function produces runs for Hector with some param uncertainty when user provides a core(s) to initialize the model.
#
# This function is an update to previous version and includes the ability to specify metrics, vars, and years of
# interest for the user - previous version can be viewed in testing_functionality_hector_data_piecewise.R
#
# Updates to this function version uses metric_calc_1run() to subset the resulting df to include only the information of most interest to the user.
# The benefit of this is the preservation of memory by reducing the output (example, reducing from ~24k records to ~2k)
#
# Finally, the function gives the user the following capabilities:
# 1) freedom to indicate how many hector runs they want to complete.
# 2) will combine a list of runs into one df (useful for use with many scenarios).
# 3) will add run numbers column to the final df.
# Having run_numbers will make the result compatible with the metric_calc_bin() function without extra steps outside the function.

it_hector_with_subset <- function(core, op, var, years, runs = 20) {

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
    dat$metric <- metric # Adds col name for the metric calculated
    dat$beta <- beta # Adds col name to the results for each param
    dat$q10 <- q10
    dat$npp_flux <- npp_flux0
    dat$aero_scale <- aero_scale

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
