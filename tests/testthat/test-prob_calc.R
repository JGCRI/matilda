# metric and bin data
metrics <- 1:10
bins <- c(2, 4, 6, 8, 10)

# metrics and/or bins not defined
no_metrics <- c()
no_bins <- c()

# test class and structure

test_that("result is proper class and structure", {
  result <- prob_calc(metrics, bins)

  # return is a df
  expect_s3_class(result, "data.frame")

  # structure is correct for each column
  expect_true(is.factor(result$bins))
  expect_true(is.numeric(c(result$scores, result$probability)))
})

# test errors and warnings

test_that("errors and warnings are produced in proper cases", {
  # error thrown when metric_df is empty
  expect_error(prob_calc(no_metrics, bins),
    regexp = "metrics has no data."
  )

  # error thrown when bins are not defined
  expect_error(prob_calc(metrics, no_bins),
    regexp = "bins must be defined."
  )

  # error thrown when metrics length and scores length are not equal
  expect_error(prob_calc(metrics, bins, scores = 1:9),
    regexp = "Length of metrics does not equal length of scores."
  )
})

# test probability functionality

test_that("expected probability is calculated", {
  prob_df <- prob_calc(metrics, bins)

  expect_equal(prob_df$probability, rep(0.25, 4))
})
