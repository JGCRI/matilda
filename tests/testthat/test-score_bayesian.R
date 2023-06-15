# # sample matrix
m <- matrix(c(rep(1, 6), rep(2, 3), rep(3, 3)), nrow = 3, ncol = 4)

# Testing error/warning cases

test_that("function stops and produces error messages", {

  # error when matrix has less than two columns
  # m with only 2 cols
  m_2row <- matrix(data = c(1:2), nrow = 2, ncol = 2)
  expect_error(score_bayesian(m_2row),
               regexp = "More than 2 columns must be included in input matrix")

  # error when entire m is NA
  # No non-NAs in obs col
  m_NA_obs <- matrix(data = c(rep(NA, 5), 1:15), nrow = 5, ncol = 4)
  expect_error(score_bayesian(m_NA_obs),
               regexp = "No non-NA values in observed data")

  # error when one row in m is NA
  # No non-NAs in any model data
  m_NA_dat <- matrix(data = c(1:5, rep(NA, 15)), nrow = 5, ncol = 4)
  expect_error(score_bayesian(m_NA_dat),
               regexp = "NAs detected in data. Analysis halted to prevent bad result.")

  # error when an NA is present anywhere in matrix array
  # NA present in model data
  m_NA_single_case <- matrix(data = c(1:3, 2, NA, 4, 3:5, 4:6), nrow = 3, ncol = 4)
  expect_error(score_bayesian(m_NA_single_case),
               regexp = "NAs detected in data. Analysis halted to prevent bad result.")

  # error when user supplies negative e values
  expect_error(score_bayesian(m, -1),
              regexp = "e must be value greater than 0.")

})

# Testing output accuracy

test_that("scores assessed correctly", {

  # scores equal when RMSE = 0
  m2 = matrix(data = c(1, 1, 1), nrow = 1, ncol = 3)
  expect_equal(score_bayesian(m2), c(0.5, 0.5))

  # when e = 0 all scores are identical
  m3 = matrix(data = c(1:3), nrow = 1, ncol = 3)
  result <- score_bayesian(m3, 0)
  expect_identical(result, c(rep(result[1], length(result))))

  # expect the output to be ex(-0.5 * (RMSE_calc)^2/ncol(m)-1
  m4 = matrix(data = c(1, 2, 2), nrow = 1, ncol = 3)
  expect_equal(score_bayesian(m4, 2), rep(exp(-0.5 * c(1, 1) ^ 2)/(ncol(m4)-1)))

})
