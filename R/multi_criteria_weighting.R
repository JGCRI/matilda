multi_criteria_weighting <- function(scores_list, criterion_weights = NULL) {

  # Check to make sure a score list is provided
  if (length(scores_list) == 0)
    stop("At least one set of ensemble scores is required.")

  # Check if criterion_weights are supplied.
  # if NULL all score_list should be weighted equally.
  if (is.null(criterion_weights)) {
    criterion_number <- length(scores_list)
    criterion_weights <- rep(1/criterion_number, length(scores_list))
  }

  # Check to make sure the length of the scores_list and criterion_weights is equal.
  if (length(scores_list) != length(criterion_weights))
    stop("Number of score dfs in scores_list and number of weights in criterion_weights should be the same.")

  # compute new scores based on weight of each df
  # using mapply to pair dfs with corresponding weight
  weighted_new_scores <- mapply(function(df, weight) df$weights * weight,
                                scores_list,
                                criterion_weights)

  # using rowsums to combine scores scross dfs in scores_list
  combined_scores <- rowSums(weighted_new_scores)

  # normalize the combined weights to sum to 1
  normalized_combined_scores <- combined_scores / sum(combined_scores)

  # return a df with run number and normalized multi-criteria weights
  return(data.frame(
    run_number = 1:length(normalized_combined_scores),
    mc_weight = normalized_combined_scores))
}


