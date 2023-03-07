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
#' @note Mean values used to build parameter distributions are pulled from
#' default values in \code{\link{hector}} v3.0.
#'
#' Standard deviation values for each parameter were pulled from peer-reviewed
#' literature as follows:
#'
#' \code{BETA} ± 0.10 from Jones et al. (2018)
#'
#' \code{Q10} ± 1.0 from \href{https://doi.org/10.1038/nature04514}{Davidson and Janssens (2006)}
#'
#' \code{NPP} ± 14.3 from \href{https://doi.org/10.1111/j.1365-2486.2011.02450.x}{Ito et al. (2011)}
#'
#' \code{AERO_SCALE} ± 0.23 from \href{https://doi.org/10.5194/acp-20-9591-2020}{Smith et al. (2020)}
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
  ecs <- fetchvars(core, NA, ECS())
  ohd <- fetchvars(core, NA, DIFFUSIVITY())

  # data frame of random parameter values drawn from parameter distributions
  data.frame(
    "BETA" = rlnorm(draws, lognorm(beta$value, 0.1) [1], lognorm(beta$value, 0.1) [2]),
    "Q10_RH" = rlnorm(draws,lognorm(q10$value, 1.0) [1], lognorm(q10$value, 1.0) [2]),
    "NPP_FLUX0" = rnorm(draws, mean = npp$value, sd = 14.3),
    "AERO_SCALE" = rnorm(draws, mean = aero$value, sd = 0.23),
    "ECS" = joint_pdf_sample(draws,
                             dist1 = lognorm_to_norm(draws, ecs$value, 0.65),
                             dist2 = rnorm(draws, ohd$value, 0.23))[1],
    "DIFFUSIVITY" = joint_pdf_sample(draws,
                                     dist1 = lognorm_to_norm(draws, ecs$value, 0.65),
                                     dist2 = rnorm(draws, ohd$value, 0.23))[2]
  )
}
