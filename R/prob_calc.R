# Goal: Probability function

#' Calculating probabilities of climate metric outcomes
#'
#' @description A function that calculates basic probabilities of outcomes based
#' on previously defined bin groupings.
#'
#' @param x Table of binned climate variable metrics.
#'
#' @return Table of probabilities using breaks assigned by user in
#' \code{\link{bin_metrics}}
#'
#' @export

prob_calc <- function(x) {

  prob = x / sum(x)

  prob

}
