#' Computing Model Scores as Posterior Probabilities using Bayesian Inference
#'
#' @description This function uses observed data to compute scores that represent
#' how well modeled values reflect what is occurring in reality. To do this, the
#' function uses root mean square error (RMSE) values to compute the likelihood
#' of observing modeled values given observed values, assuming normal distribution
#' of errors. Likelihood values are used to compute posterior probabilities which
#' are used as scores for each model iteration.
#'
#' @param m A Matrix of values. The first column of the matrix should be
#' a vector of observed data for a give variable. Subsequent vectors should be
#' representative of modeled values for a given variable.
#' @param e A values for 0-Inf. This value controls the decay rate. It controls
#' how much models are penalized for deviation from observed data. The default is
#' set to 2, larger values will increase rate of decay.
#'
#' @return Returns a vector of scores with a length equal to the number of
#' model iterations in the input matrix. Or, if the input matrix has K columns,
#' the return vector will = K - 1.
#' @export
#'
#' @examples
#' # creating sample matrix
#' mat <- matrix(data = 1:15, nrow = 5, ncol = 3)
#'
#' # scoring with a decay rate of 2
#' score_Bayesian(mat, e = 2)

score_bayesian <- function(m, e = 2) {

  # initialize vector to store RMSE values from loop
  rmse_vector <- numeric()

  # Stop execution if number of columns in the matrix is less the 2
  # indicates that there is only one model result stored in matrix
  stopifnot(ncol(m) > 2)

  # indicate that observed data are in first column of matrix
  obs_data <- m[, 1]

  # loop across columns of the matrix. For each column (i) after col 2
  for(i in 2:ncol(m)) {

    # indicate modeled data are in subsequent columns
    model_data <- m[, i]

    # compute RMSE using obs_data and model_data
    rmse_vals = RMSE_calc(obs_data, model_data)

    # vector of RMSE value for each model iteration
    rmse_vector[i] <- rmse_vals

  }

  # Compute likelihood using normal distribution likelihood function.
  # This is the probability of observing the modeled data given the
  # observed data.
  # Remove first value when calling rmse_vector (first values should be NA because
  # it represented obs_data)
  likelihood = exp(-0.5 * (rmse_vector[-1]) ^ e)

  # Computing unnormalized posterior scores
  # Currently only computing posterior scores using uniform prior.
  # uniform prior is calculated as 1/length(likelihood) which is
  # the same as 1 / # of runs.
  posterior = likelihood * (1 / length(likelihood))

  # Computes posterior probabilities - normalized posterior weights.
  # Will sum to 1 and there for get significantly smaller as number
  # of runs increases.
  posterior_probs = posterior / sum(posterior)

  # Create data frame of results - get run_numbers from the list where RMSE values
  # are computed (names of the split_list components)
  return(posterior_probs)

}