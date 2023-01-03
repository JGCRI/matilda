# creating new_metric object for tests
co2_crit <- crit_co2_obs()

# Testing attribute class

test_that("result inherits correct attribute class", {

  expect_s3_class(co2_crit, "criterion")

})

# Testing structure of result
test_that("output has correct data structure", {

  expect_true(is.integer(c(co2_crit$year)))

  expect_true(is.character(c(co2_crit$var)))

  expect_true(is.numeric(c(co2_crit$obs_values)))

})

