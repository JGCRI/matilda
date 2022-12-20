# compute vector of score for two numeric vectors (x and y)
# w1 - distance at which score ramps down from 1.
# w2 - distance at which score becomes 0.

#' Computing vector of score for numeric vectors
#'
#' @param x First set of numeric vectors
#' @param y Second set of numeric vectors
#' @param w1 Distance at which score ramps down from 1.
#' @param w2 Distance at which score becomes 0
#'
#' @return Vector of scores
#' @export
#'
#' @examples
score_ramp <- function(x, y, w1, w2, na.omit = FALSE) {

  if (na.omit) {
    x <- na.omit(x)
    y <- na.omit(y)
  }

  if (w1 < 0) stop("w1 must be at least 0")

  if (w2 < w1) stop("w2 must be at least as big as w1")

  if (length(x) != length(y)) stop("Length of x must be equal to length of y")

  if ( all(is.na(x))) stop("No non-NA values in x")

  if ( all(is.na(y))) stop("No non-NA values in y")

  abs_diffs <- abs(x - y)

  scores <- rep(NA_real_, length(x))

  # the order of the following two statements matters, because
  # we want a diff exactly equal to w1 to get a score of 1, even when w1 = w2
  scores [abs_diffs >= w2] <- 0
  scores [abs_diffs <= w1] <- 1

  # for abs_diffs between w1 and w2 - compute how far between w1 and w2 the
  # abs_diff value is
  between_w1_w2 <- abs_diffs > w1 & abs_diffs < w2
  w1_w2_frac <- (abs_diffs [between_w1_w2] - w1) / (w2 - w1)

  # for scores between w1 and w2 use (1- computed distance) as score value
  scores [between_w1_w2] <- 1 - w1_w2_frac

  return(scores)

}

# Goal: score hector runs based on divergence of CO2 projections in relation to
# observed values (using mlo data)

#' Screen Hector outputs with observed data
#'
#' @param x result data frame from iterative_hector.
#' @param obs_dat observed data used to score hector runs.
#' @param years year range of used for screening hector runs.
#' @param w1 low divergence bound to assign highest score.
#' @param w2 high divergence bound to assign lowest score.
#'
#' @return Data frame with mean score for each Hector run
#' @export
#'
#' @examples

score_hruns <- function(x, crit, score_function,...) {

  # subset to include years for CO2 screening
  x_subset <- subset(x, year %in% crit$years & variable == crit$var)

  #creates observed data frame
  obs_dat <- data.frame(year = crit$years, value_obs = crit$obs_values)

  # merge hector results with calibration data observed CO2 data
  x_merge <- merge(x_subset, obs_dat, by = 'year')

  # add new column to res_merge computing scores so that we can
  x_merge$scores <- score_function(x_merge$value_obs, x_merge$value,...)

  # calculate mean scores for each run.
  score_mean <- aggregate(scores ~ run_number, data = x_merge, FUN = mean)

  # df of mean scores
  mean_score <- as.data.frame(score_mean)

  return(mean_score)

}
