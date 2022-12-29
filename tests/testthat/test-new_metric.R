# Goal: test code for new_metric function
#
# Tests:
## structure and class
##
## Do we need to test whether the variables are present in the core? or will this
## get picked-up by another test -- most other functions have an error for when
## the var is no present.
##
## Because this function just stores user information into a list

# creating new_metric object for tests
new_metric <- new_metric("global_tas", 2000:2050, mean)

# Testing attribute class

test_that("result inherits correct attribute class", {

  expect_s3_class(new_metric, "h_metric")

})

# Testing structure of result
test_that("output has correct data structure", {

  expect_true(is.character(c(new_metric$var, new_metric$op_name)))

  expect_true(is.integer(new_metric$years))

  expect_true(is.function(new_metric$op))

})

