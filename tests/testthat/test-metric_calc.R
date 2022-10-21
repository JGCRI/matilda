### Ask Ben about this ###
ssp245 <- newcore(ini_file)
x <- iterative_hector(ssp245, "global_tas", 2000:2100, 10)
### can't figure out how to recognize object in testing code ###


test_that("metric_calc works", {
  r <- metric_calc(x, mean, "global_tas", 2000:2100)

  # return is a df
  expect_true(is.data.frame(r))

  # are df dimensions correct
  expect_equal(dim(r), c(10, 3))

  # is data str correct for cols
  expect_true(is.numeric(r$metric_result))
  expect_true(is.character(r$variable))
  expect_true(is.integer(r$run_number)) # could be character

})
