# Sample data
score_df1 <- data.frame(weights = c(0.6, 0.4))
score_df2 <- data.frame(weights = c(0.5, 0.5))
score_df3 <- data.frame(weights = c(0.9, 0.1))

# Sample lists
L1 <- list()
L2 <- list(score_df1, score_df2)
L3 <- list(score_df1, score_df2, score_df3)

# Sample weights
W1 <- c(0.4, 0.6)

test_that("Function stops and publishes correct error messages.", {

  # Error thrown when list is empty
  expect_error(multi_criteria_weighting(scores_list =  L1),
               regexp = "At least one set of ensemble scores is required.")

  # Error thrown when elements in list of criterion score does not match
  # the number of elements in the list of weights for each criterion.
  expect_error(multi_criteria_weighting(scores_list = L3, criterion_weights =  W1),
               regexp = "Number of score dfs in scores_list and number of weights in criterion_weights should be the same.")

})

test_that("Function weights scores appropriately.", {

  # when using default, criterion_weights are even
  result_default <- multi_criteria_weighting(L2)
  expect_equal(result_default$mc_weight, c(score_df1$weights * 0.5 + score_df2$weights * 0.5))

  # when weights are supplied they are used to adjust multi-criterion weights
  result_W1 <- multi_criteria_weighting(L2, W1)
  expect_equal(result_W1$mc_weight, c(score_df1$weights * 0.4 + score_df2$weights * 0.6))

})

test_that("Function returns a dataframe.", {

  result <- multi_criteria_weighting(L2)

  expect_s3_class(result, "data.frame")

})

