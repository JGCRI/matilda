# test RMSE functionality

test_that("RMSE is computed accurately", {
  expect_equal(RMSE_calc(1, 5), sqrt(mean(1 - 5)^2))
})

# test return is vector

test_that("RMSE return is a vector", {
  expect_vector(RMSE_calc(x = 1:3, y = 3:5))
})
