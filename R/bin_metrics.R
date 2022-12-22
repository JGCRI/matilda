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
