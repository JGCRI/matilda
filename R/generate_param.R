#' Generating Parameter Values
#'
#' @description This function will generate parameter values that will be set for
#' multiple Hector iterations.Parameters are generated from random draws of a
#' normal distribution. The mean of each distribution is set using values from
#' a core object initiated using \code{\link{newcore}}.
#'
#' @param draws Number of random draws for each parameter.
#' @param core An initiated Hector core.
#'
#' @return A data frame object with parameter values generated for each
#' draw. The column \code{run_number} indicates the number of \code{draws} provided
#' as an argument in the function.
#' @export
#'
#' @examples
#' # Initiate a Hector core
#' ssp_245 <- system.file("input/hector_ssp245.ini", package = "hector")
#' core <- newcore(ssp_245)
#'
#' # Generate parameters for Hector iterations
#' generate_params(core, 10)

generate_params <- function(core, draws){

  beta <- fetchvars(core, NA, BETA())
  q10 <- fetchvars(core, NA, Q10_RH())
  npp <- fetchvars(core, NA, NPP_FLUX0())
  aero <- fetchvars(core, NA, AERO_SCALE())

  # data frame of random parameter values drawn from normal or lognormal distributions
  data.frame(
    "BETA" = rlnorm(draws, lognorm(beta$value, 0.1) [1], lognorm(beta$value, 0.1) [2]),
    "Q10_RH" = rlnorm(draws,lognorm(q10$value, 1.0) [1], lognorm(q10$value, 1.0) [2]),
    "NPP_FLUX0" = rnorm(draws, mean = npp$value, sd = 14.3),
    "AERO_SCALE" = rnorm(draws, mean = aero$value, sd = 0.23)
  )
}
