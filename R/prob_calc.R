#' Binning scored climate metrics and calculating probabilities
#'
#'@description This function organizes metric data into bins and computes the sum of the
#'scored Hector runs in each bin. This method effectively weights bins by how closely each run
#'represents observed data. This function also calculates the probability that a metric will occur in
#'each bin range.
#'
#' @param metric_df A data frame of metrics calculated for each model run. For example,
#' an output from \code{\link{metric_calc}}.
#' @param bins A vector of variable ranges used to bin metric data.
#' @param score_result A data frame of scores calculated for each model run. For example,
#' an output from \code{\link{score_hruns}}.
#'
#' @return A data frame object of bins, the summed scores of model runs producing
#' metrics in the value range of each bin, and the probability of each bin outcome.
#' @export
#'
#' @examples
#' # Example of data frame containing metric values
#' metric_df <- data.frame(run_number = 1:10,
#'                         metric_result = runif(10, 690, 805))
#' metric_df
#'
#' # Example of data frame containing scores for each run
#' scores_df <- data.frame(run_number = 1:10,
#'                         scores = runif(10, 0, 1))
#' scores_df
#'
#' # Creating bins using variable ranges
#' bins <- c(690, 720, 740, 760, 780, 805)
#'
#' # Calculating probabilities for each bin
#' prob_calc(metric_df = metric_df, bins = bins, score_result = scores_df)

prob_calc <- function(metric_df, bins, score_result) {

  # if data frames have a length of 0, produce error.
  if( length(metric_df) == 0) stop("metric_df has no data.")
  if( length(score_result) == 0) stop("score_result has no data.")

  # if bins are not defined by user, produce error.
  if( length(bins) == 0) stop("bins must be defined.")

  # Order of this warning matters. Want to throw error if there is no data, but
  # throw a warning is there is data of unequal length.
  # if data frames have different lengths run_numbers produce error
  if( nrow(metric_df) != nrow(score_result))
    warning("run_number of metric_df does not equal run_number of score_result.")

  # cut data be metric values and assign to bin
  bin_df <- as.data.frame(cut(metric_df$metric_result, bins))

  # edit column names and add run numbers for merge
  colnames(bin_df)[1] <- "bins"
  bin_df$run_number <- metric_df$run_number

  # merge binned metrics and score for each run by run_number
  merge_df <- merge(bin_df, score_result, by = 'run_number')

  # compute summed scores by metric bins
  sum_scores <- aggregate(scores ~ bins, merge_df, sum)

  # calculate probabilities
  sum_scores$probability <- (sum_scores$scores / sum(sum_scores$scores))

  sum_scores

}
