# Goal: Write a function that produces normal distribution draws for parameters
# of the model and store in a table that can be passed into iterative hector.
#
# Why: still trying to figure this out conceptually - But it saves a fraction of
# time and will not require `interative_hector` to run through the param random draw
# process.
# Removing the random draw step from `iterative_hector` will make the function
# more simple, with its only responsibility being to run hector through an iterative
# process.
#
# The parameter table should be able to be passed to `iterative-hector` -- will
# need to figure this out next.
#
## Leeya's `generate_params` function already does this using tidyr/dplyr
## `generate-param` takes "run_numbers" arg, stores its length in "runs", then passes
## run_numbers to "run_number column and creates columns for each of the parameters
## using the appropriate normal distribution function, with "runs" as the arg for the
## number of random draws to take from the normal distribution.
## Can also see how Leeya applies her tibble of param values to her run_hector function
## (lines 185-193 in 01_generate_data.Rmd)

#' Generating Parameter Values
#'
#' @description Generates parameter values from a normal distribution user
#' provided number of random draws.
#'
#' @param draws Number of random draws for each parameter.
#'
#' @return A data frame object with a row for each parameter value generated for each
#' run. Columns indicate run_number and each parameter.
#' @export
#'
#' @examples
gen_param <- function(draws){

  # Building list of runs and random parameter draws based on run number
  params <- list(

    "run_number" = 1:draws,
    "BETA" = rnorm(draws, mean = 0.54, sd = 0.1),
    "Q10_RH" = rlnorm(draws,lognorm(2, 1.0) [1], lognorm(2, 1.0) [2]),
    "NPP_FLUX0" = rnorm(draws, mean = 56.2, sd = 14.3),
    "AERO_SCALE" = rnorm(draws, mean = 1.01, sd = 0.23))

  # Output - converting list to df
  return(as.data.frame(params))

}
