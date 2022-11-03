# What are some tests that we want to complete for generate_params?

# data - values as numeric
y <- data.frame("BETA" = 1:2, "Q10_RH" = 2:3)

# generating parameters
params <- generate_params(5)

# test that the output is of class data.frame
test_that("output is a df", {

  expect_s3_class(y, "data.frame")

})

# test that the data frame has names?
test_that("params in the df have names", {

  expect_named(y,
               c("BETA", "Q10_RH"))

})

# test that the the values produced for the df are numeric?
test_that("param values in df are numeric", {

  expect_true(is.numeric(params$BETA))

})


# test that the output from the function has the same length as the input value?

# error is input is not a value?

## Don't forget to add the other parameters to the generate_params function.
