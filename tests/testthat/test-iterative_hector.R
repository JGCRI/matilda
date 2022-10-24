# lognorm tests
test_that("lognorm returns two numeric values", {
  x <- lognorm(1, 0.1)

  expect_equal(length(x), 2)
  expect_true(is.numeric(x))

})

# metric_calc_1run test set-up
ssp245 <- newcore(system.file("input/hector_ssp245.ini", package = "hector"))
r <- iterative_hector(ssp245,
                      "global_tas",
                      2000:2100,
                      10)

# metric_calc_1run tests
test_that("metric_calc_1run returns a single numeric value", {
  x <- metric_calc_1run(r,
                        mean,
                        "global_tas",
                        2000:2100)

  # is value length equal to one
  expect_equal(length(x), 1)

  # is value numeric
  expect_true(is.numeric(x))

})
