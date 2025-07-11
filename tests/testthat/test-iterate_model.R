# Testing iterate_model
library(hector)

# Define a set of parameters that we know should work.
param_values <- data.frame("BETA" = c(0.54, 0.48),
                           "Q10_RH" = c(1.4, 1.3),
                           "NPP_FLUX0" = c(59.2, 53),
                           "AERO_SCALE" = c(1.3, 0.93),
                           "DIFFUSIVITY" = c(2.1, 1),
                           "ECS" = c(2.4, 3.7))

# Set up the hector core.
ini <- system.file("input/hector_ssp245.ini", package = "hector")
hc <- newcore(ini)

test_that("iterate model runs", {

  # Confirm that the default parameter values are returned from a run.
  rslts <- iterate_model(core = hc, params = param_values)
  expect_true(length(unique(rslts$run_number)) == 2)
  default_vars <- c("CO2_concentration", "RF_tot", "RF_CO2", "global_tas")
  expect_true(all(default_vars %in% rslts$variable))
  expect_true(sum(!rslts$variable %in% default_vars) == 0)

  # Confirm that changing default arguments is reflected in output
  yr <- 1900
  rslts <- iterate_model(core = hc, params = param_values, save_years = yr)
  expect_equal(unique(rslts$year), yr)

  var <- NPP()
  rslts <- iterate_model(core = hc, params = param_values,
                         save_years = yr, save_vars = var)
  expect_equal(unique(rslts$variable), var)

})

test_that("iterate model runs even with an error", {

  # Intentionally pass a parameter value that will cause Hector to crash.
  param_values$DIFFUSIVITY[2] <- -1

  # Function should run without error and return a data frame.
  rslts <- iterate_model(core = hc, params = param_values)
  expect_true(is.data.frame(rslts))

})

test_that("iterate model output has expected structure",  {

  rslts <- iterate_model(core = hc, params = param_values)

  # confirm the output is a data frame
  expect_true(is.data.frame(rslts))

  # confirm the proper columns stored in the output data frame
  expect_true(all(c("scenario", "year", "variable", "value", "units", "run_number") %in% names(rslts)))

  })

test_that("iterate model assigns run_number correctly even when NAs", {

  # force a bad param instance
  bad_params <- param_values
  bad_params$ECS[2] <- -555

  # run model with bad params to force NA
  rslts <- iterate_model(core = hc, params = bad_params, save_years = 2000)

  # sort the unique run_number in the output and expect them to be 1 and 2
  expect_equal(sort(unique(rslts$run_number)), 1:2)

  })

test_that("iterate model runs as expected with a single parameter column", {

  # make param data frame with only one column
  single_param <- data.frame("ECS" = c(3.0, 4.5))

  rslts <- iterate_model(core = hc, params = single_param, save_years = 2000)

  # confirm that all the results in the output match hector default
  expect_true(all(rslts$variable %in% c("CO2_concentration", "RF_tot", "RF_CO2", "global_tas")))

  # confirm that the length of the runs in the model output is equal to the length of the params (2)
  expect_true(length(unique(rslts$run_number)) == 2)

})
