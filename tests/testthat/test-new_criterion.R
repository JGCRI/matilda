# creating new_metric object for tests
co2_crit <- criterion_co2_obs()

# Testing attribute class

test_that("result has correct attribute class", {
  expect_s3_class(co2_crit, "criterion")
})

# Testing structure of result

test_that("output has correct data structure", {
  expect_true(is.integer(c(co2_crit$year)))

  expect_true(is.character(c(co2_crit$var)))

  expect_true(is.numeric(c(co2_crit$obs_values)))
})

# Testing message print is accurate

test_that("print.criterion prints the criterion accurately", {
  expect_equal(capture.output(print.criterion(co2_crit)), "Criterion for screening Hector:  CO2_concentration 1959  to  2021 ")
})
