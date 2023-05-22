# testing df
r <- data.frame(x = rep(1,3), y = 1:3)

# sample matrix
m <- matrix(data = 1:15, nrow = 5, ncol = 3)

# df with NAs - for entire variable
m_2row <- matrix(data = c(1:2), nrow = 2, ncol = 2)
m_NA_obs <- matrix(data = c(rep(NA, 5), 1:15), nrow = 5, ncol = 4)
m_NA_dat <- matrix(data = c(1:5, rep(NA, 15)), nrow = 5, ncol = 4)

# Testing error/warning cases

test_that("function stops and produces error messages", {

  # error when matrix has less than two columns
  expect_error(score_ramp(m_2row, w1 = 1, w2 = 2),
               regexp = "More than 2 columns must be included in input matrix")

  # error when w1 not > 0
  expect_error(score_ramp(m, w1 = -1, w2 = 1),
               regexp = "w1 must be at least 0")

  # error when w2 not > w1
  expect_error(score_ramp(m, w1 = 1, w2 = 0),
               regexp = "w2 must be at least as big as w1")

  # error when entire m is NA
  expect_error(score_ramp(m_NA_obs, w1 = 0, w2 = 2),
               regexp = "No non-NA values in observed")

  # error when one row in m is NA
  expect_error(score_ramp(m = m_NA_dat, w1 = 0, w2 = 2),
               regexp = "No non-NA values in modeled")

})

# Testing edge cases

test_that("scores assessed correctly based on w1 and w2 values", {

  # when w1 = 0 -- only diffs = 0, score = 1; diffs > 0, score = 0
  expect_equal(score_ramp(x = r$x, y = r$y, w1 = 0, w2 = 1), c(1, 0, 0))

  # when w1 & w2 both = 1 -- diffs <= 1, score = 1; diffs > 1, score = 0
  expect_equal(score_ramp(x = r$x, y = r$y, w1 = 1, w2 = 1), c(1, 1, 0))

  # when w1 and w2 = 0
  expect_equal(score_ramp(x = r$x, y = r$y, w1 = 0, w2 = 0), c(1, 0, 0))

  })

# Testing score calculation

test_that("x-y differences between w1 & w2 are computed accurately", {

  # when x-y diff is between w1 & w2 expect value between 0-1
  expect_gt(score_ramp(x = 2, y = 1, w1 = 0, w2 = 2), 0)
  expect_lt(score_ramp(x = 2, y = 1, w1 = 0, w2 = 2), 1)

  # when x-y diff is between w1 & w2 expect computed score to equal
  # 1 - (abs_diff - w1) / (w2-w1)
  expect_equal(score_ramp(x = 2, y = 1, w1 = 0, w2 = 2), 1 - (abs(2 - 1) - 0) / (2 - 0))

})
