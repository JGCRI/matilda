# Goal: develop a function that takes an output from metric_calc and bins the results
# and stores in a table that will eventually be put in the probability calculation function.

#' Binning un-scored climate metrics
#'
#' @description A function that accepts data frame of climatic metrics from
#' \code{\link{metric_calc}} output and groups metric values in user defined
#' bins.
#'
#' @param metric_df Data frame output from \code{\link{metric_calc}} result.
#' @param bins a vector of breaks for data binning.
#'
#' @return A table object that bins metric values using breaks supplied
#' by user.
#' @export

bin_metrics <- function(metric_df, bins) {

  table(cut(metric_df$metric_result, bins))

}


#' Binning scored climate metrics
#'
#'@description This function organizes metric data into bins and computes the sum of the
#'scored runs in each bin. This method effectively weights bins by how closely each run
#'represents observed data.
#'
#' @param metric_df A data frame of metrics calculated for each model run. For example,
#' an output from \code{\link{metric_calc}}.
#' @param score_result A data frame of scores calculated for each model run. For example,
#' an output from \code{\link{score_hruns}}.
#' @param bins A vector of variable ranges used to bin metric data.
#'
#' @return A data frame object of bins and the summed scores of model runs producing
#' metrics in the value range of each bin.
#' @export
#'
#' @examples
bin_scored_metrics <- function(metric_df, score_result, bins) {

  # if data frames have different lengths (different run_numbers) produce error
  if( length(metric_df) != length(score_result))
    stop("Length of metric_df must equal length of score_result.")

  # cut data be metric values and assign to bin
  bin_df <- as.data.frame(cut(metric_df$metric_result, bins))

  # Edit column names and add run numbers for merge
  colnames(bin_df)[1] <- "bins"
  bin_df$run_number <- metric_df$run_number

  # merge binned metrics and score for each run by run_number
  merge_df <- merge(bin_df, score_result, by = 'run_number')

  # compute summed scores by metric bins
  sum_scores <- aggregate(scores ~ bins, merge_df, sum)

  sum_scores

}
