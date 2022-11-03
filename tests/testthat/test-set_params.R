# data - values as characters
x <- c("BETA" = "1", "Q10_RH" = "2", "NPP_FLUX0" = "3", "AERO_SCALE" = "4")

# data - values as numeric
y <- c("BETA" = 1, "Q10_RH" = 2, "NPP_FLUX0" = 3, "AERO_SCALE" = 4)

# empty values
e <- c("BETA" = 0, "Q10_RH" = 0, "NPP_FLUX0" = 0, "AERO_SCALE" = 0)

# core
core <- newcore(system.file("input/hector_ssp245.ini", package = "hector"))

# Error if param values are not numeric

test_that("error thrown when values passed to set_param are not numeric", {

  expect_error(set_params(core,
                          data.frame("x" = 1)),
               regexp = 'not numeric')

})

# Message when setting parameters - may not be necessary

test_that("message printed when parameter values are set", {

  expect_message(set_params(core,
                            y))

})

# Vector has names

test_that("numeric vector has names", {

  expect_error(set_params(core,
                          1:3),
               regexp = "no names")
})

# Vector empty

test_that("warns for no parameters", {

  expect_warning(set_params(core,
                            c()),
                 regexp = "no parameters")
})

# tested some things within the function but don't know if these can only be tested
# when building or not.
# 1) test that the fn_names match names of Hector variable variable functions
#    use expect-name
#
# 2) test that var is a list of characters

