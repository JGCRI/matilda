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
  expect_true(length(unique(rslts$run_number))  == 2)
  defualt_vars <- c("CO2_concentration", "RF_tot", "RF_CO2", "global_tas")
  expect_true(all(defualt_vars %in% rslts$variable))
  expect_true(sum(!rslts$variable %in% defualt_vars) == 0)

  # Confirm that changing default arguments is reflected in output
  yr <- 1900
  rslts <- iterate_model(core = hc, params = param_values, save_years = yr)
  expect_true(unique(rslts$year) == yr)

  var <- NPP()
  rslts <- iterate_model(core = hc, params = param_values,
                         save_years = yr, save_vars = var)
  expect_true(unique(rslts$variable) == var)

})

test_that("iterate model runs even with an error", {

  # Intentionally pass a parameter value that will cause Hector to crash.
  param_values$DIFFUSIVITY[2] <- -1

  # Function should run without error and return a data frame.
  rslts <- iterate_model(core = hc, params = param_values)
  expect_true(is.data.frame(rslts))

})
