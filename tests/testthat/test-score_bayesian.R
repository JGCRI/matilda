# # sample matrix
m <- matrix(c(rep(1, 6), rep(2, 3), rep(3, 3)), nrow = 3, ncol = 4)

# Testing error/warning cases

test_that("function stops and produces error messages", {
  # error when matrix has less than three columns
  # m with only 2 cols
  m_2col <- matrix(data = c(1:6), nrow = 3, ncol = 2)
  expect_error(score_bayesian(m_2col),
    regexp = "More than 3 columns must be included in input matrix"
  )

  # error when entire m is NA
  # No non-NAs in obs col
  m_NA_obs <- matrix(data = c(rep(NA, 5), 1:15), nrow = 5, ncol = 4)
  expect_error(score_bayesian(m_NA_obs),
    regexp = "No non-NA values present in observed data"
  )

  # error when user supplies negative sensitivity value
  expect_error(score_bayesian(m, sensitivity = -1),
    regexp = "Sensitivity cannot be negative."
  )

  # error when user supplies more than one sensitivity value
  expect_error(score_bayesian(m, sensitivity = c(1, 2)),
               regexp = "Sensitivity must be a single value.")
})

# Testing output accuracy

test_that("scores assessed correctly", {
  # If entire column of modeled data is NA, set RMSE value to NA
  m_NA <- matrix(data = c(rep(1, 3), rep(1, 3), rep(2, 3), rep(NA, 3)), nrow = 3, ncol = 4)
  test_result_1 <- score_bayesian(m_NA, sigma = 1)
  expect_equal(test_result_1 [3], as.numeric(NA))

  # scores equal when calculated RMSE = 0
  m2 <- matrix(data = c(rep(1,3), rep(1, 3), rep(1, 3), rep(1, 3)), nrow = 3, ncol = 4)
  expect_equal(score_bayesian(m2, sigma = 1), c(1, 1, 1))

  # when sensitivity = 0 all scores are identical
  m3 <- matrix(data = c(1:4), nrow = 1, ncol = 4)
  expect_equal(score_bayesian(m3, sigma = 1, sensitivity = 0), c(1, 1, 1))

 })
