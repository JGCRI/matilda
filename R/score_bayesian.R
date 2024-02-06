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
#' @param sigma Numeric value (optional). The standard deviation parameter for
#' the normal distribution used in the Bayesian analysis. If not provided, the
#' function will automatically compute it as the standard deviation of the
#' Root Mean Square Error (RMSE). A smaller value of `sigma` will make the
#' Bayesian analysis give more weight to models with lower RMSE values.
#'
#' @note Note: In Bayesian statistics, the choice of `sigma` can significantly
#' impact the results and conclusions of the analysis. Users are encouraged to
#' experiment with different values and understand the implications for their
#' specific use case.
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
#' score_bayesian(mat, sigma = 2)
score_bayesian <- function(m, sigma = NULL, sensitivity = NULL) {
  # initialize vector to store RMSE values from loop
  rmse_vector <- numeric()

  # Stop execution if number of columns in the matrix is less than 2
  # indicates that there is only one model result stored in matrix
  stopifnot("More than 2 columns must be included in input matrix" = ncol(m) > 2)

  # indicate that observed data are in the first column of the matrix
  obs_data <- m[, 1]

  # error if the observed data has no non-NA values
  if (all(is.na(obs_data))) stop("No non-NA values present in observed data.")

  # loop across columns of the matrix. For each column (i) after col 2
  for (i in 2:ncol(m)) {
    # indicate modeled data are in subsequent columns
    model_data <- m[, i]

    # If an entire model is NA result - set RMSE value to NA
    if (all(is.na(model_data))) {
      rmse_vals <- NA # Set RMSE to NA for this column
    } else {
      rmse_vals <- RMSE_calc(obs_data, model_data, sigma = sigma)
    }

    # vector of RMSE value for each model iteration
    rmse_vector[i] <- rmse_vals
  }

  # Compute likelihood sensitivity using multiplier provided by the user
  if (is.null(sensitivity)) {
    sensitivity_value <- sd(obs_data) # Calculate sensitivity as the standard deviation of the observed data
  } else {
    sensitivity_value <- sensitivity * sd(obs_data)
    if (length(sigma) != 1)
        stop( "Sensitivity must be a single value.")
  }


  # Check if sigma is negative, if so throw error
  if (any(!is.na(sensitivity) & (sensitivity < 0))) stop("Sensitivity cannot be negative.")

  # Compute likelihood using normal distribution likelihood function.
  # This is the probability of observing the modeled data given the
  # observed data.
  # Remove first value when calling rmse_vector (first values should be NA because
  # it represented obs_data)
  likelihood <- exp(-0.5 * ((rmse_vector[-1]) / sensitivity_value)^2)

  # Computing unnormalized posterior scores
  # Currently only computing posterior scores using uniform prior.
  # uniform prior is calculated as 1/length(likelihood) which is
  # the same as 1 / # of runs.
  posterior <- likelihood * (1 / length(likelihood)) ### DONT THINK THIS IS A NECESSARY STEP MAY JUST WANT TO RETURN "LIKELIHOOD" ###

  # Create data frame of results - get run_numbers from the list where RMSE values
  # are computed (names of the split_list components)
  return(posterior)
}
