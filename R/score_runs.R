#' Screen Hector outputs with observed data
#'
#' @description This function uses any scoring function to screen Hector runs
#' based on proximity of climate variable values to observed data. Internal scoring
#' functions are provided in \code{matilda}, but users can also supply their own.
#' Criterion used for scoring are also available in the package. Alternatively,
#' users can build their own scoring criterion with \code{\link{new_criterion}}.
#'
#' @param x Result data frame from \code{\link{iterate_model}}.
#' @param score_function Scoring function to use for screening Hector model runs.
#' @param criterion A scoring criterion to use for screening Hector runs.
#' @param ... Additional arguments needed to run the selected scoring function.
#'
#' @return Data frame of scored for each Hector run
#' @export
#'
#' @import stats
#'
#' @examples
#' # Score Hector using observed CO2 data with the score_ramp method
#' score_hruns(matilda_result, criterion_co2_obs(), score_ramp, w1 = 2, w2 = 20)
score_hruns <- function(x, criterion, score_function, ...) {
  # error if x is not a data frame
  if (!is.data.frame(x)) {
    stop("user supplied x is not a data frame")
  }

  # error if criterion object is not a criterion
  if (!is.criterion(criterion)) {
    stop("user supplied crit is not a criterion")
  }

  # error if score_function is not a function
  if (!is.function(score_function)) {
    stop("user supplied score_function is not a function")
  }

  # error if x year range does not include criterion year range
  if (!all(criterion$years %in% x$year)) {
    stop("The year range in x must contain all years in criterion")
  }

  # subset to include years for criterion screening
  x_subset <-
    subset(x, x$year %in% criterion$years &
      x$variable == criterion$var)

  # error if variable in x does not match variable in the criterion being used
  if (!nrow(x_subset)) {
    stop("criterion year and variable combination not represented in data")
  }

  # converts x_subset to matrix - columns are vectors of values for each model iteration
  model_matrix <- hector_matrix(x_subset, columns = "value")

  # creates observed data frame
  obs_dat <- data.frame(value_obs = criterion$obs_values)

  # merge hector results with calibration data (obs_dat)
  x_matrix <- cbind(obs_dat$value, model_matrix)

  # computing scores using the user specified score_function
  scores <- score_function(x_matrix, ...)

  # Normalize scores from the scoring function - normalized score weights.
  # Will sum to 1.
  sum_non_na <- sum(scores, na.rm = TRUE)
  score_norm <- ifelse(is.na(scores), NA, scores / sum_non_na)

  return(data.frame(
    weights = score_norm,
    run_number = 1:length(scores)
  ))
}
