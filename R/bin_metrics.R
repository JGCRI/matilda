#' Binning scored climate metrics and calculating probabilities
#'
#'@description This function organizes metric data into bins and computes the sum of the
#'scored runs in each bin. This method effectively weights bins by how closely each run
#'represents observed data, and calculates the probability that a metric will occur in
#'each bin range.
#'
#' @param metric_df A data frame of metrics calculated for each model run. For example,
#' an output from \code{\link{metric_calc}}.
#' @param bins A vector of variable ranges used to bin metric data.
#' @param score_result A data frame of scores calculated for each model run. For example,
#' an output from \code{\link{score_hruns}}.
#'
#' @return A data frame object of bins and the summed scores of model runs producing
#' metrics in the value range of each bin.
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
#' bin_scored_metrics(metric_df = metric_df, bins = bins, score_result = scores_df)

bin_scored_metrics <- function(metric_df, bins, score_result) {

  # if data frames have different lengths (different run_numbers) produce error
  if( length(metric_df) != length(score_result))
    stop("Length of metric_df must equal length of score_result.")

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
