# data - values as numeric
y <- c("BETA" = 1, "Q10_RH" = 2)

# core
core <- newcore(system.file("input/hector_ssp245.ini", package = "hector"))

# Checking message printed - may not be necessary
test_that("function returns message", {
  expect_message(set_params(
    core,
    y
  ))
})


# Checking errors and warnings
test_that("invalid inputs returns proper error/warning", {
  # checking that vector has names
  expect_error(
    set_params(
      core,
      1:3
    ),
    regexp = "no names"
  )

  # ensuring warning produced when vector has no values
  expect_warning(
    set_params(
      core,
      c()
    ),
    regexp = "no parameters"
  )

  # ensuring error when param values not numeric
  expect_error(
    set_params(
      core,
      data.frame("x" = 1)
    ),
    regexp = "not numeric"
  )
})

test_that("set_param functions properly", {
  default_beta <- fetchvars(core, NA, BETA())$value

  # ensure beta is different from default value
  new_beta <- default_beta + 0.1

  param_values <- c("BETA" = new_beta)

  params <- set_params(core, param_values)

  fetch <- fetchvars(core, NA, BETA())

  expect_equal(
    fetch$value,
    new_beta
  )
})
