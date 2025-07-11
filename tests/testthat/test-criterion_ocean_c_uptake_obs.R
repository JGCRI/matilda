# creating new_metric object for tests
ocean_c_uptake_crit <-

# Testing attribute class

test_that("result has correct attribute class", {
  expect_s3_class(gmst_crit, "criterion")
})

# Testing structure of result

test_that("output has correct data structure", {
  expect_true(is.integer(c(gmst_crit$year)))

  expect_true(is.character(c(gmst_crit$var)))

  expect_true(is.numeric(c(gmst_crit$obs_values)))
})
