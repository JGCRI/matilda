# Goals: code to run tests on the following:
# 1) what happens if a df with NAs is read into the function (all or partial)
# 2) Check the structure and class of the output
# 3) test that the correct errors are being produced in the correct cases.
# 4) check that the probability is being calculated correctly.

# test df
metric_df <- data.frame(run_number = 1:5,
                        metric_result = runif(5, 1, 5))
score_result <- data.frame(run_number = 1:5,
                           scores = runif(5, 0, 1))
bins <- c(1, 2, 3, 4, 5)

# shorter score result
score_result_short <- data.frame(run_number = 1:4,
                                 scores = runif(4, 0, 1))

# dfs testing errors
metric_no_data <- data.frame(c())
scores_no_data <- data.frame(c())
no_bins <- c()

# test class and structure

test_that("result is proper class and structure", {

  result <- prob_calc(metric_df, bins = bins, score_result)

  # return is a df
  expect_s3_class(result, "data.frame")

  # structure is correct for each column
  expect_true(is.factor(result$bins))
  expect_true(is.numeric(c(result$score, result$probability)))

})

# test errors and warnings

test_that("errors and warnings are produced in proper cases", {

  # error thrown when metric_df is empty
  expect_error(prob_calc(metric_no_data, bins, score_result),
               regexp = "metric_df has no data.")

  # error thrown when score_result is empty
  expect_error(prob_calc(metric_df, bins, scores_no_data),
               regexp = "score_result has no data.")

  # error thrown when bins are not defined
  expect_error(prob_calc(metric_df, no_bins, score_result),
               regexp = "bins must be defined.")


  # warning thrown when metric_df and score_result are not equal length
  expect_warning(prob_calc(metric_df, bins, score_result_short),
                 regexp = "run_number of metric_df does not equal run_number of score_result.")

})

# test probability functionality

met_df <- data.frame(run_number = 1:3,
                     metric_result = c(1, 2, 3))

scr_df <- data.frame(run_number = 1:3,
                     scores = runif(1, 2, 3))

bins_prob_test <- c(1,2)

test_that("expected probability is calculated", {

  prob_df <- prob_calc(met_df, bins_prob_test, scr_df)

  expect_equal(prob_df$probability, 1)

})

