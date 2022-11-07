# Testing metric_calc_1run

# testing df
y <- data.frame(year = 2000:2100,
                variable = rep(c("var1", "var2"), each = 101),
                value = runif(202, 0, 5))

# empty df


# tests
test_that("metric_calc_1run returns a single numeric value", {

   r <- metric_calc_1run(y,
                        op = mean,
                        var = "var1",
                        years = 2000:2100)

  # is value length equal to one
  expect_equal(length(r), 1)
  # is value numeric
  expect_true(is.numeric(r))

})

test_that("correct error thrown when var and years not subset of df", {

  # is error produced if year range provided exceeds years in df
  expect_error(metric_calc_1run(y,
                                op = mean,
                                var = "var1",
                                years = 2000:2101),
               regexp = 'year range must be subset of years in x')

  # is error produced if variable provided is not present in df - results in NA return
  expect_error(metric_calc_1run(y,
                                mean,
                                "var3",
                                2000:2050),
               regexp = 'variable not present in x, metric_value == NA')
})


# Question to ask Ben at next meeting:
# If a function is not going to be exported, and only written to be used in another
# function how do you deal with similar error situations. For example, missing variable in input
# should be caught by the metric_calc function, should I also be adding error message for a
# function that will not be exported?
# Because one function is within another, do some error code not apply to both functions?
# Think of a better way to ask this or provide an example.
#
# Is it worth bothering to add error message in a function that won't be exported?
#
# Example - is metric_calc_1run is not exported and only used in metric_calc, is it worth
# running a test for what happens if the df passed to metric_calc_1run is empty, if we already have an
# error coded into the metric_calc function to prevent supplying an empty df?
