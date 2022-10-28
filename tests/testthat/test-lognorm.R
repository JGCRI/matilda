# Testing lognorm

# lognorm tests
test_that("lognorm returns two numeric values", {
  x <- lognorm(1, 0.1)

  expect_equal(length(x), 2)
  expect_true(is.numeric(x))

})
