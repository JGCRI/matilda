# data - values as characters
x <- c("BETA" = "1", "Q10_RH" = "2", "NPP_FLUX0" = "3", "AERO_SCALE" = "4")

# data - values as numeric
y <- c("BETA" = 1, "Q10_RH" = 2, "NPP_FLUX0" = 3, "AERO_SCALE" = 4)

# empty values
e <- c("BETA" = 0, "Q10_RH" = 0, "NPP_FLUX0" = 0, "AERO_SCALE" = 0)


# Error if param values are not numeric

test_that("error thrown when values passed to set_param are not numeric", {

  expect_error(set_params(core,
                          x))

})

# Message when setting parameters

test_that("message printed when parameter values are set", {

  expect_message(set_params(core,
                            y))

})

# Warn if 0 present

test_that("warning thrown when one or more parameters being set to 0", {

  expect_warning(set_params(core,
                            e),
                 regexp = "one or more parameters set to 0")

})



# tested some things within the function but don't know if these can only be tested
# when building or not.
# 1) test that the fn_names match names of Hector variable variable functions
#    use expect-name
#
# 2) test that var is a list of characters

