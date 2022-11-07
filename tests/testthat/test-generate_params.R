# data - values as numeric
y <- data.frame("BETA" = 1:2, "Q10_RH" = 2:3)

# generating parameters
params <- generate_params(5)


test_that("output has proper class and structure", {

# test that the output is of class data.frame
  expect_s3_class(y, "data.frame")

# test that the data frame has names?
  expect_named(y,
               c("BETA", "Q10_RH"))

# test that the the values produced for the df are numeric?
  expect_true(is.numeric(params$BETA))

})
