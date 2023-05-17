#' Calculating Root Mean Square Error (RMSE)
#'
#' @description Function takes vectors of modeled data values and compares them
#' to observed data values to report the RMSE.
#'
#' @param x A vector of modeled data values
#' @param y A vector of observed data values
#'
#' @return Returns a vector of RMSE values
#' @export
#'
#' @examples
#' x = c(1:5)
#' y = c(5:10)
#'
#' RMSE_calc(x, y)

RMSE_calc <- function(x, y) {

  # compute RMSE
  rmse_vals = sqrt(mean((x - y)^2))

  # return a vector of RMSE values
  return(rmse_vals)

}
