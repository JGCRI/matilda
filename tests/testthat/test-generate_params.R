# data - values as numeric
y <- data.frame("BETA" = 1:2, "Q10_RH" = 2:3)

# generating parameters
params <- generate_params(5)

test_that("output has proper class, structure, and length", {

# make sure output is data.frame
  expect_s3_class(y, "data.frame")

# make sure all values are numeric
  expect_true(all(sapply(y, is.numeric)))

# make sure the output is a proper length
  expect_equal(nrow(params),5)

})

test_that("check for NAs", {

  # make sure params output has no NAs
  expect_false(any(is.na(params)))

})
