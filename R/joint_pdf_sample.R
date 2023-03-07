#' Build and Sample Joint Probability Distribution
#'
#' @description A joint probability distribution is implemented for parameters that
#' are correlated. A default correlation of -0.75 is used, but is specific to the
#' inversely correlated parameters of \code{\link{ECS}} and \code{\link{DIFFUSIVITY}}.
#' This function builds a variance-covariance matrix to randomly sample from a multivariate
#' distribution \href{link}{(Pressburger et al. 2023)}.
#'
#' @param core An initiated Hector core.
#' @param draws Number of random draws for each parameter.
#' @param param_cor Correlation of the covariates contributing to the joint
#' probability distribution.
#'
#' @notes To calculate covariance, we use the used defined parameter correlation
#' \code{param_cor}. We multiply the correlation by the standard deviation of each
#' parameter contributing to the joint probability distribution. The function
#' \code{\link{mvnorm}} in the package \code{\link{MASS}} was used to randomly
#' sample our variance-covariance matrix.
#'
#' @return A data frame object with parameter values generated for each
#' draw. Values randomly sample from a multivariate normal distribution of
#' correlated Hector parameters.
#' @export
#'
#' @examples
#'
joint_pdf_sample <- function(core, draws, param_cor = -0.75){

  # this will need to change later - for now just keep it in this function"
  ecs <- fetchvars(core, NA, ECS())
  diffusivity <- fetchvars(core, NA, DIFFUSIVITY())

  # building initial param distributions
  ## ecs lognormal distribution
  ecs_lognorm <- rlnorm(draws,
                        lognorm(ecs$value, 0.65)[1],
                        lognorm(ecs$value, 0.65)[2])
  ## producing normal ecs values
  ecs_norm <- log(ecs_lognorm)
  ## diffusivity normal distribution
  diffusivity_norm <- rnorm(draws,
                            diffusivity$value, 0.23)

  # calculating covariance value
  cov_val <- param_cor * sd(ecs_norm) * sd (diffusivity_norm)

  # building the variance-covariance matrix
  sigma <- matrix(c(var(ecs_norm),
                    cov_val, cov_val,
                    var(diffusivity_norm)),
                  2, 2)

  # sampling param values from joint pdf
  ## May want to seaparate this out? talk with Ben about options
  joint_values <- MASS::mvrnorm(draws * 2,
                                c(mean(ecs_vals_to_norm),
                                  mean(diffusion_vals)),
                                sigma)
  ## give column names to values sample from joint pdf
  joint_values <- data.frame(ECS = joint_values [, 1],
                             DIFFUSIVITY = joint_values [, 2])

  ## !! use exp() on ECS -- not really sure the purpose of this line in Leeyas code !! ##
  joint_values$ECS <- exp(joint_values$ECS)

  # subset to exclude rows with ECS values < 0
  jv_subset <- subset(joint_values, ECS > 0)

  # sample the filtered results
  jv_sample <- jv_subset[sample(1:nrow(jv_subset), draws), ]

}
