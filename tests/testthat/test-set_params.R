# data - values as numeric
y <- c("BETA" = 1, "Q10_RH" = 2)

# empty values
e <- c("BETA" = 0, "Q10_RH" = 0)

# core
core <- newcore(system.file("input/hector_ssp245.ini", package = "hector"))

# Checking message printed - may not be necessary
test_that("function returns message", {

  expect_message(set_params(core,
                            y))

})


# Checking errors and warnings
test_that("invalid inputs returns proper error/warning", {

  # checking that vector has names
  expect_error(set_params(core,
                          1:3),
               regexp = "no names")

  # warning when vector has not values
  expect_warning(set_params(core,
                            c()),
               regexp = "no parameters")

  # error when param values not numeric
  expect_error(set_params(core,
                          data.frame("x" = 1)),
               regexp = 'not numeric')
})

test_that("set_param functions properly", {

  param_gen <- generate_params(1)

  param_values <- unlist(param_gen)

  params <- set_params(core, param_values)

  fetch <- fetchvars(core, NA, BETA())

  expect_equal(fetch$value,
               param_gen$BETA)

})
