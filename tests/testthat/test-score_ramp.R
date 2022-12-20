# testing df
r <- data.frame(x = rep(1,3), y = 1:3)

# df with NAs - for entire variable
df_na <- data.frame(x = rep(NA, 3), y = 1:3)

# Testing error cases

test_that("function stops and produces error messages", {

  # error when x and y not equal length
  expect_error(score_ramp(1:5, 1:6, w1 = 1, w2 = 2))

  # error when w1 not >= 0
  expect_error(score_ramp(1:5, 1:5, w1 = -1, w2 = 1))

  # error when w2 not >= w1
  expect_error(score_ramp(1:5, 1:5, w1 = 1, w2 = 0))

  # error when entire x is NA
  expect_error(score_ramp(x = na$x, y = na$y, w1 = 0, w2 = 2))

  # error when entire y is NA

})

# Testing edge cases

test_that("scores assessed correctly based on w1 and w2 values", {

  # when w1 = 0 -- only diffs = 0, score = 1; diffs > 0, score = 0
  expect_equal(score_ramp(x = r$x, y = r$y, w1 = 0, w2 = 1), c(1, 0, 0))

  # when w1 & w2 both = 1 -- diffs <= 1, score = 1; diffs > 1, score = 0
  expect_equal(score_ramp(x = r$x, y = r$y, w1 = 1, w2 = 1), c(1, 1, 0))

  # when w1 and w2 = 0

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
