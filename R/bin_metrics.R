# Goal: develop a function that takes an output from metric_calc and bins the results
# and stores in a table that will eventually be put in the probability calculation function.

#' Data binning for climate metrics
#'
#' @description A function that accepts data frame of climatic metrics from
#' \code{\link{metric_calc}} output and groups metric values in user defined
#' bins.
#'
#' @param metric_df Data frame output from \code{\link{metric_calc}} result.
#' @param bins a vector of breaks for data binning.
#'
#' @return An object of class table binning metric values using breaks supplied
#' by user.
#' @export

bin_metrics <- function(metric_df, bins) {

  table(cut(metric_df$metric_result, bins))

}


#' Sum scores
#'
#' @param metric_df
#' @param score_result
#' @param bins
#'
#' @return
#' @export
#'
#' @examples
sum_scores <- function(metric_df, score_result, bins) {

  bin_df <- as.data.frame(cut(metric_df$metric_result, bins), col.names = 'bin')

  colnames(bin_df)[1] <- "bins"
  bin_df$run_number <- metric_df$run_number

  merge_df <- merge(bin_df, score_result, by = 'run_number')

  sum_scores <- aggregate(scores ~ bins, merge_df, sum)

}
