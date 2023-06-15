# creating new_metric object for tests
tas_crit <- criterion_tas_obs()

# Testing attribute class

test_that("result has correct attribute class", {

  expect_s3_class(tas_crit, "criterion")

})

# Testing structure of result

test_that("output has correct data structure", {

  expect_true(is.integer(c(tas_crit$year)))

  expect_true(is.character(c(tas_crit$var)))

  expect_true(is.numeric(c(tas_crit$obs_values)))

})

