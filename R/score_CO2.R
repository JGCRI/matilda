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
#' @return
#' @export
#'
#' @examples
score_ramp <- function(x, y, w1, w2) {

  stopifnot(w1 >= 0)

  stopifnot(w2 >= w1)

  stopifnot(length(x) == length(y))

  if ( any(is.na(x))) warning("result contains NAs")

  if ( any(is.na(y))) warning("result contains NAs")

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

#' Screen Hector outputs by observed CO2
#'
#' @param x result data frame from iterative_hector
#' @param w1 low divergence bound to assign highest score
#' @param w2 high divergence bound to assign lowest score
#'
#' @return Data frame with mean score for each Hector run
#' @export
#'
#' @examples

score_CO2 <- function(x, w1 = 2, w2 = 20) {

  # subset to include years for CO2 screening
  res_subset <- subset(x, year >= 1959 & year <= 2021)

  # merge hector results with mlo observed CO2 data
  res_merge <- merge(x, mlo, by.x = 'year', by.y = 'year')

  # add new column to res_merge computing scores so that we can
  # calculate mean scores for each run.
  res_merge$scores <- score_ramp(res_merge$mean, res_merge$value, w1, w2)

  score_mean <- aggregate(scores ~ run_number, data = res_merge, FUN = mean)

  # df of mean scores
  mean_score <- as.data.frame(score_mean)

  return(mean_score)

}
