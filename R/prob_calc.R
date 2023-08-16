#' Binning scored climate metrics and calculating probabilities
#'
#' @description This function organizes metric data into bins and computes the
#' sum of the scored Hector runs in each bin. This method effectively weights bins
#' by how closely each run represents observed data. This function also calculates
#' the probability that a metric will occur in each bin range.
#'
#' @param metrics A vector of metrics calculated for each model run. For
#' example, an output from \code{\link{metric_calc}}.
#' @param bins A vector of variable ranges used to bin metric data.
#' @param scores A vector of scores calculated for each model run. For
#' example, an output from \code{\link{score_runs}}.
#'
#' @return A data frame object of bins, the summed scores of model runs producing
#' metrics in the value range of each bin, and the probability of each bin outcome.
#' @export
#'
#' @examples
#' # Example vector containing metric values
#' metrics <- runif(5, 690, 805)
#' metrics
#'
#' # Example vector containing scores for each run
#' scores <- runif(5, 0, 1)
#' scores
#'
#' # Creating bins using variable ranges
#' bins <- c(690, 720, 740, 760, 780, 805)
#'
#' # Calculating probabilities for each bin
#' prob_calc(metrics = metrics, bins = bins, score = scores)
prob_calc <- function(metrics, bins, scores = rep(1, length(metrics))) {
  # if metrics have a length of 0, produce error.
  if (length(metrics) == 0) stop("metrics has no data.")

  # if bins are not defined by user, produce error.
  if (length(bins) == 0) stop("bins must be defined.")

  # metrics and scores must be the same length.
  if (length(metrics) != length(scores)) {
    stop("Length of metrics does not equal length of scores.")
  }

  # cut data be metric values and assign to bin
  bin_df <- data.frame(
    bins = cut(metrics, bins),
    score = scores
  )

  # compute summed scores by metric bins
  sum_scores <- aggregate(scores ~ bins, bin_df, sum)

  # calculate probabilities
  sum_scores$probability <- (sum_scores$scores / sum(sum_scores$scores))

  sum_scores
}
