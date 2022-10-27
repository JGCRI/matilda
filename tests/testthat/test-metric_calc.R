# data occupied df
x <- data.frame(year = 2000:2050,
                variable = rep(c("var"), each = 51),
                value = runif(51, 0, 3),
                run_number = rep(c(1:5), each = 51))

# empty df
df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(df) <- c("year", "variable", "value", "run_number")

# tests

test_that("metric_calc result is df with correct data structure", {
  r <- metric_calc(x,
                   mean,
                   "var",
                   2000:2050)

  # return is a df
  expect_true(is.data.frame(r))

  # is data str correct for cols
  expect_true(is.numeric(r$metric_result))
  expect_true(is.integer(r$run_number))

})

test_that("error message thrown when empty df passed to function", {

  expect_error(metric_calc(df,
                           mean,
                           "var",
                           2000:2050),
               regexp = 'x has no data')

})

test_that("error message thrown when year range exceeds years in df", {

  expect_error(metric_calc(x,
                           mean,
                           "var",
                           2000:2051),
               regexp = 'year range exceeds years in x')
})

test_that("error message thrown when variable not present in df", {

  expect_error(metric_calc(x,
                           mean,
                           "global_tas",
                           2000:2050),
               regexp = 'variable is not present in x')
})
