# # sample matrix
m <- matrix(c(rep(1, 6), rep(2, 3), rep(3, 3)), nrow = 3, ncol = 4)

## Error sample matrices
# m with only 2 cols
m_2row <- matrix(data = c(1:2), nrow = 2, ncol = 2)
# No non-NAs in obs col
m_NA_obs <- matrix(data = c(rep(NA, 5), 1:15), nrow = 5, ncol = 4)
# No non-NAs in any model data
m_NA_dat <- matrix(data = c(1:5, rep(NA, 15)), nrow = 5, ncol = 4)
# NA present in model data
m_NA_single_case <- matrix(data = c(1:3, 2, NA, 4, 3:5, 4:6), nrow = 3, ncol = 4)

# Testing error/warning cases

test_that("function stops and produces error messages", {

  # error when matrix has less than two columns
  expect_error(score_bayesian(m_2row),
               regexp = "More than 2 columns must be included in input matrix")

  # error when entire m is NA
  expect_error(score_bayesian(m_NA_obs),
               regexp = "No non-NA values in observed data")

  # error when one row in m is NA
  expect_error(score_bayesian(m_NA_dat),
               regexp = "NAs detected in data. Analysis halted to prevent bad result.")

  # error when an NA is present anywhere in matrix array
  expect_error(score_bayesian(m_NA_single_case),
               regexp = "NAs detected in data. Analysis halted to prevent bad result.")

})

# testing output accuracy

test_that("scores assessed correctly", {

  m2 = matrix(data = c(1, 1, 1), nrow = 1, ncol = 3)

  # score uniform when
  expect_equal(score_bayesian(m2), )

})
