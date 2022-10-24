# new hector instance
ssp245 <- newcore(system.file("input/hector_ssp245.ini",
                              package = "hector"))

# iterative hector result
x <- iterative_hector(ssp245, "global_tas", 2000:2100, 10)

test_that("metric_calc result is df", {
  r <- metric_calc(x,
                   mean,
                   "global_tas",
                   2000:2050)

  # return is a df
  expect_true(is.data.frame(r))

})

test_that("metric_calc result has correct data structure", {
  r <- metric_calc(x,
                   mean,
                   "global_tas",
                   2000:2100)

  # is data str correct for cols
  expect_true(is.numeric(r$metric_result))
  expect_true(is.character(r$variable))
  expect_true(is.integer(r$run_number))

})



