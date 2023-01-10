#' Generating Parameter Values
#'
#' @description This function will generate parameter values for running Hector.
#' Parameters are generated from random draws of a normal distribution.
#'
#' @param draws Number of random draws for each parameter.
#'
#' @return A data frame object with parameter values generated for each
#' draw. The column \code{run_number} indicates the number of \code{draws} provided
#' as an argument in the function.
#' @export
#'
#' @examples
#' # Generate parameters for Hector iterations
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
