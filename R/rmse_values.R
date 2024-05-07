#' Computing Root Mean Square Error (RMSE) Values for Ensemble Members
#'
#' @description This function computes RMSE values for each perturbed parameter ensemble member. It uses the result
#' of \code{\link{iterate_model}} to compute RMSE using the user specified criterion. Time varying error can be applied
#' using \code{sigma}, which supplies a single value or a vector of values representing criterion error at each time step.
#'
#' @param df Result data frame from \code{\link{iterate_model}}.
#' @param criterion A scoring criterion to use for screening Hector runs (e.g., \code{\link{criterion_gmst_obs}})
#' @param sigma A single value or vector of error terms the same length as y.
#' A single value will apply a constant error term. User can provide a vector
#' of error terms to incorporate time-varying error into the RMSE calculation.
#' `sigma` default assume homoscedasticity of residuals and applies a constant
#' error term equal to the standard deviation of observed data (scoring criterion).

#' @return Returns a data frame with a \code{run_number} each ensemble member and
#' a corresponding RMSE value.
#' @export
#'
rmse_values <- function(df, criterion, sigma = NULL) {

  # subset to include years for criterion screening
  df_subset <-
    subset(df, df$year %in% criterion$years &
             df$variable == criterion$var)

  # error if variable in x does not match variable in the criterion being used
  if (!nrow(df_subset)) {
    stop("criterion year and variable combination not represented in data")
  }

  # converts x_subset to matrix - columns are vectors of values for each model iteration
  model_matrix <- hector_matrix(df_subset, columns = "value")

  # creates observed data frame
  obs_dat <- data.frame(value_obs = criterion$obs_values)

  # merge hector results with calibration data (obs_dat)
  input_matrix <- cbind(obs_dat$value, model_matrix)

  # initialize vector to store RMSE values
  rmse_vector <- rep(NA_real_, ncol(input_matrix))

  # Stop execution if number of columns in the matrix is less than 2
  # indicates that there is only one model result stored in matrix
  stopifnot("More than 3 columns must be included in input matrix" = ncol(input_matrix) > 3)

  # indicate that observed data are in the first column of the matrix
  obs_data <- input_matrix[, 1]

  # error if the observed data has no non-NA values
  if (all(is.na(obs_data)))
    stop("No non-NA values present in observed data.")

  # If sigma is not provided, use sd(y) as default
  if (is.null(sigma)) {
    sigma <- sd(obs_data)
  }

  # loop across columns of the matrix. For each column (i) starting with col 2
  for (i in 2:ncol(input_matrix)) {
    # indicate modeled data are in subsequent columns
    model_data <- input_matrix[, i]

    # If an entire model is NA result - set RMSE value to NA
    if (all(is.na(model_data))) {
      rmse_vals <- NA # Set RMSE to NA for this column
    } else {
      rmse_vals <- RMSE_calc(model_data, obs_data, sigma = sigma)
    }

    # vector of RMSE value for each model iteration
    rmse_vector[i] <- rmse_vals
  }

  # remove column where observed data was
  rmse_vector <- rmse_vector[-1]

  return(data.frame(
    run_number = unique(df$run_number),
    rmse_value = rmse_vector
  ))

}
