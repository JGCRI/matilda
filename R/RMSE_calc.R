#' Calculating Root Mean Square Error (RMSE)
#'
#' @description Function takes vectors of modeled data values and compares them
#' to observed data values to report the RMSE.
#'
#' @param x A vector of modeled data values
#' @param y A vector of observed data values
#' @param sigma A single value or vector of error terms the same length as y.
#' A single value will apply a constant error term. User can provide a vector
#' of error terms to incorporate time-varying error into the RMSE calculation.
#'
#' @return Returns a vector of RMSE values
#' @export
#'
#' @examples
#' x <- c(1:5)
#' y <- c(5:9)
#'
#' # constant error
#' RMSE_calc(x, y)
RMSE_calc <- function(x, y, sigma) {
  # Check if all values are NA
  if (all(is.na(x)) || all(is.na(y))) {
    return(NA)
  }

  # set sigma to default if not provided by the user
  if( is.null(sigma)) {
    sigma = sd(y) # if sigma is NULL compute as sd of observed data (y)
  } else {
    # if user provides sigma values it should be length = 1 or same length as the obs data.
    required_length <- length(y)
    if(length(sigma) != 1 && length(sigma) != required_length)
      stop(paste("Length of sigma must be a single value or a vector matching the length of ", required_length, ".", sep = ""))
  }

  # compute RMSE
  rmse_vals <- sqrt(mean((x - y)^2) / sigma)

  # return a vector of RMSE values
  return(rmse_vals)
}
