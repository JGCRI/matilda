# Goal: Probability function

#' Calculating probabilities of climate metric outcomes
#'
#' @description This is a function that calculates basic probabilities of metric outcomes
#' from multiple model runs.
#'
#' @param x A data frame of scored frequencies of each model run organized into data bins.
#' For example, an output from \code{\link{bin_scored_metrics}}.
#'
#' @return A data frame of probabilities using breaks assigned by user in
#' \code{\link{bin_scored_metrics}}
#'
#' @export

prob_calc <- function(x) {

  prob = data.frame(
    bins = x$bins,
    scores = x$scores,
    probability = x$scores / sum(x$scores))

  prob

}
