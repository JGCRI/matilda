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
#' `sigma` default assume homoscedasticity of residuals and applies a constant
#' error term equal to the standard deviation of observed data (scoring criterion).

#' @return Returns a vector of RMSE values
#' @export
#'
#' @examples
#' x <- c(1:5)
#' y <- c(5:9)
#'
#' RMSE_calc(x, y)
RMSE_calc <- function(x, y, sigma = sd(y)) {
  # Check if all values are NA
  if (all(is.na(x)) || all(is.na(y))) {
    return(NA)
  }

  # if user provides sigma values compute RMSE with error.
  # sigma should be length = 1 or same length as the obs data.
  if (length(sigma) != 1 && length(sigma) != length(y))
    stop(
      paste(
        "Length of sigma must be a single value or a vector matching the length of observed data = ",
        length(y),
        ".",
        sep = ""
      )
    )

  rmse_vals <- sqrt(mean(((x - y) / sigma) ^ 2))

  # return a vector of RMSE values
  return(rmse_vals)
}
