#' Converting lognormal distribution to normal distribution
#'
#' @param n Number of observations.
#' @param mean mean of the distribution
#' @param sd deviation of the distribution
#'
#' @return A vector list of randomly sample values. The length of the results is
#' determined by \code{n}. Lognormal distribution in this function is in reference
#' to the result produced by \code{\link{lognorm}}.
#' @export
#'
#' @examples
lognorm_to_norm <- function(n, mean, sd){
  lognormal <- rlnorm(n,
                    lognorm(mean, sd)[1],
                    lognorm(mean, sd)[2])
  norm <- log(lognormal)
}

#' Building Variance-Covariance Matrix
#'
#' @description This function builds a variance-covariance matrix that contains
#' the variances and covariances of associated with provided variable distributions.
#'
#' @param dist1 Probability distribution of the first covariate.
#' @param dist2 Probability distribution of the second covariate.
#' @param param_cor Correlation of the covariates contributing to the joint
#' probability distribution.
#'
#' @return A matrix object with diagonal elements containing the variances of the
#' variables and off-diagonal elements containing variance of the paired variables.
#' @export
#'
#' @examples
covariance_matrix <- function(dist1, dist2, param_cor = -0.75){
  # calculating covariance value
  cov_val <- param_cor * sd(dist1) * sd (dist2)

  # building the variance-covariance matrix
  sigma <- matrix(c(var(dist1),
                    cov_val, cov_val,
                    var(dist2)),
                  2, 2)
}

#' Build and Sample Joint Probability Distribution
#'
#' @description A joint probability distribution is implemented for parameters that
#' are correlated. A default correlation of -0.75 is used, but is specific to the
#' inversely correlated parameters of \code{\link{ECS}} and \code{\link{DIFFUSIVITY}}.
#' This function builds a variance-covariance matrix to randomly sample from a multivariate
#' distribution \href{link}{(Pressburger et al. 2023)}.
#'
#' @param draws Number of random draws for each parameter.
#' @param dist1 Probability distribution of the first covariate.
#' @param dist2 Probability distribution of the second covariate.
#' @param param_cor Correlation of the covariates contributing to the joint
#' probability distribution.
#'
#' @details This function is currently specific to the joint PDF of \code{\link{ECS}} and
#' \code{\link{DIFFUSIVTIY}} and therefore uses a default parameter correlation
#' specific to these parameters. The user can also define their own parameter
#' correlation \code{param_cor}. Correlation value is multiplied by the standard
#' deviation of each parameter contributing to the joint probability distribution.
#' The function \code{\link{mvnorm}} in the package \code{\link{MASS}} was used
#' to randomly sample the variance-covariance matrix.
#'
#' @return A data frame object with parameter values generated for each
#' draw. Values randomly sample from a multivariate normal distribution of
#' correlated Hector parameters.
#' @export
#'
#' @examples
#'
joint_pdf_sample <- function(draws, dist1, dist2, param_cor = -0.75){

  # computing sigma
  sigma = covariance_matrix(dist1, dist2, param_cor)

  # sampling param values from joint pdf
  ## May want to separate this out? talk with Ben about options
  joint_values <- MASS::mvrnorm(draws * 2,
                                c(mean(dist1),
                                  mean(dist2)),
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
