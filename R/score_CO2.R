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

  # merge hector reuslts with mlo observed CO2 data
  res_merge <- merge(x, mlo, by.x = 'year', by.y = 'year')

  # calculate differences between observed and predicted CO2
  CO2_diff <- abs(res_merge$mean - res_merge$value)

  # establishing scoring bounds
  scores <- rep(NA, length(CO2_diff))
  scores [CO2_diff <= w1] <- 1
  scores [CO2_diff >= w2] <- 0

  # establishing a df of CO2_difference, score, and run number
  score <- as.data.frame(scores)
  score$scores <- scores
  score$run_number <- res_merge$run_number

  # split score df by run_number
  score_split <- split(score, score$run_number)

  # calculate mean for each run_number group
  score_mean <- sapply(score_split, mean)

  # df of mean scores
  mean_score <- as.data.frame(do.call("rbind", score_mean))

  return(mean_score)

}

