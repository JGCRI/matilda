# creating testing df
y <- data.frame(year = 2000:2100,
                variable = rep(c("var1", "var2"), each = 101),
                value = runif(202, 0, 5))

# creating new metric
metric <- new_metric("var1", 2000:2100, mean)

# creating new metric for testing year length
year_length <- new_metric("var1", 2000:2101, mean)

# new metric with missing variable
no_var <- new_metric("var3", 2000:2100, mean)

# tests
test_that("metric_calc_1run returns a single numeric value", {

   r <- metric_calc_1run(y,
                         metric)

  # value length equal to one
  expect_equal(length(r), 1)

  # is value numeric
  expect_true(is.numeric(r))

})

test_that("correct error thrown when var and years not subset of df", {

  # check if error produced when year range provided exceeds years in df
  expect_error(metric_calc_1run(y,
                                year_length),
               regexp = 'year range must be subset of years in x')

  # check if error produced when variable provided is not present in df - results in NA return
  expect_error(metric_calc_1run(y,
                                no_var),
               regexp = 'variable not present in x, metric_value == NA')
})
