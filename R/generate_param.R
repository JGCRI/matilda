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
#' generate_params(10)

generate_params <- function(draws){

  # Building list of runs and random parameter draws based on run number
  params <- list(

    "BETA" = rnorm(draws, mean = 0.54, sd = 0.1),
    "Q10_RH" = rlnorm(draws,lognorm(2, 1.0) [1], lognorm(2, 1.0) [2]),
    "NPP_FLUX0" = rnorm(draws, mean = 56.2, sd = 14.3),
    "AERO_SCALE" = rnorm(draws, mean = 1.01, sd = 0.23))

  # converting list to data frame
  result <- as.data.frame(params)

  return(result)

}
