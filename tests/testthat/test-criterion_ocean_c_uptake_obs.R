# creating new_metric object for tests
ocean_c_uptake_crit <- criterion_ocean_c_uptake_obs()

# Testing attribute class

test_that("result has correct attribute class", {
  expect_s3_class(ocean_c_uptake_crit, "criterion")
})

# Testing structure of result

test_that("output has correct data structure", {
  expect_true(is.integer(c(ocean_c_uptake_crit$year)))

  expect_true(is.character(c(ocean_c_uptake_crit$var)))

  expect_true(is.numeric(c(ocean_c_uptake_crit$obs_values)))
})
