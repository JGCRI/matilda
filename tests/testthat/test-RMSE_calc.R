# test NA behavior
test_that("NAs passed to function are treated correctly", {
  expect_equal(RMSE_calc(x = c(rep(NA, 3)), y = 1:3), c(NA))
  expect_equal(RMSE_calc(x = 1:3, y = c(rep(NA, 3))), c(NA))
})

# test RMSE functionality

test_that("RMSE is computed accurately", {
  expect_equal(RMSE_calc(1, 5), sqrt((mean(1-5)/sd(5)^2)))
})

# test return is vector

test_that("RMSE return is a vector", {
  expect_vector(RMSE_calc(x = 1:3, y = 3:5))
})

# tests for sigma
test_that("sigma is a single value", {
  expect_equal(RMSE_calc(1, 5, sigma = 1), sqrt(((mean(1-5)/1)^2)))
})

test_that("error if sigma is a vector of a different length than y", {
  expect_error(RMSE_calc(1, 5, sigma = c(1, 2, 3)),
               regexp = "Length of sigma must be a single value or a vector matching the length of observed data = 1")
})
