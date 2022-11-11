# data occupied df
x <- data.frame(year = 2000:2050,
                variable = rep(c("var"), each = 51),
                value = runif(51, 0, 3),
                run_number = rep(c(1:5), each = 51))

# empty df - no data
df <- data.frame(c())

# new_metric to pass into tests
new_metric <- new_metric("var", 2000:2050, mean)

# new_metric with too many years
year_range <- new_metric("var", 2000:2051, mean)

# new_metric with no var
no_var <- new_metric("var1", 2000:2050, mean)

# tests

test_that("result has proper class and structure", {

  r <- metric_calc(x,
                   new_metric)

  # return is a df
  expect_s3_class(r, "data.frame")

  # is data str correct for cols
  expect_true(is.numeric(r$metric_result))
  expect_true(is.integer(r$run_number))

})

test_that("error messages are thrown in proper cases", {

  # error when data frame has no data
  expect_error(metric_calc(df,
                           new_metric),
               regexp = 'x has no data')

  # error when year requested exceeds years in df
  expect_error(metric_calc(x,
                           year_range),
               regexp = 'year range exceeds years in x')

  # error when requested var is not in df
  expect_error(metric_calc(x,
                           no_var),
               regexp = 'variable is not present in x')
})
