# What types of tests do we need to run on the score_ramp function?

# Think about the 3 basic types of tests that are important to consider:
#
# 1) If you have error cases (that produce an error if not followed) - do those
# error cases work and produce an error when they should?
#
# 2) Does the function compute data properly? Do we get a result we would expect?
#
# 3) Edge cases. These are cases in which there are arguments that would fall on
# the edges of the data that would be provided.
# For example:
# What would happen if we fed the function a data frame with data that doesn't exist?
# What happens when dat = 0 or dat = NA?

# testing df
r <- data.frame(x = rep(1,3), y = 1:3)

# df with nas
na <- data.frame(x = rep(NA, 3), y = 1:3)

# Testing error cases

test_that("function stops and produces error messgages", {

  # error when x and y not equal length
  expect_error(score_ramp(1:5, 1:6, w1 = 1, w2 = 2))

  # error when w1 not >= 0
  expect_error(score_ramp(1:5, 1:5, w1 = -1, w2 = 1))

  # error when w2 not >= w1
  expect_error(score_ramp(1:5, 1:5, w1 = 1, w2 = 0))

  # error when NAs in data
  expect_error(score_ramp(x = na$x, y = na$y, w1 = 0, w2 = 2))

})

# Testing edge cases and computation

test_that("score calculated appropriately", {

  # when w1 = 0
  expect_equal(score_ramp(x = r$x, y = r$y, w1 = 0, w2 = 1), c(1, 0, 0))

  # when w1 & w2 = 0
  expect_equal(score_ramp(x = r$x, y = r$y, w1 = 1, w2 = 1), c(1, 1, 0))

  # when x-y diff is between w1 & w2 expect value between 0-1
  expect_gt(score_ramp(x = 2, y = 1, w1 = 0, w2 = 2), 0)
  expect_lt(score_ramp(x = 2, y = 1, w1 = 0, w2 = 2), 1)

  # when x-y diff is between w1 & w2 expect computed score to equal
  # 1 - (abs_diff - w1) / (w2-w1)
  expect_equal(score_ramp(x = 2, y = 1, w1 = 0, w2 = 2), 1 - (abs(2 - 1) - 0) / (2 - 0))

  })
