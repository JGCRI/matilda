# data - values as numeric
y <- data.frame("BETA" = 1:2, "Q10_RH" = 2:3)

# core for testing generate_params
ini <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini)

# generating parameters
params <- generate_params(core, 5)

test_that("output has proper class, structure, and length", {
  # make sure output is data.frame
  expect_s3_class(y, "data.frame")

  # make sure all values are numeric
  expect_true(all(sapply(y, is.numeric)))

  # make sure the output is a proper length
  expect_equal(nrow(params), 5)
})

test_that("no NAs are produced", {
  # make sure params output has no NAs
  expect_false(any(is.na(params)))
})
