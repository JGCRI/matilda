# Creating new_metric object for tests
metric <- new_metric("global_tas", 2000:2050, mean)

# Testing attribute class

test_that("result has correct attribute class", {

  expect_s3_class(metric, "h_metric")

})

# Testing structure of result
test_that("output has correct data structure", {

  expect_true(is.character(c(metric$var, metric$op_name)))

  expect_true(is.integer(metric$years))

  expect_true(is.function(metric$op))

})

# Testing message print is accurate

test_that("print.h_metric prints the new metric accurately", {

  expect_equal(capture.output(print.h_metric(metric)), "Probabilistic Hector Metric:  mean global_tas 2000  to  2050 ")

})

