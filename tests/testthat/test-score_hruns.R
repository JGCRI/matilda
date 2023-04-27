# data occupied df
test <- data.frame(year = 2001:2003,
                   variable = rep(c("CO2_concentration"), each = 3),
                   value = c(630, 635, 700),
                   run_number = c(1:3))

# var in df does not match var for criterion used
wrong_var <- data.frame(year = 2001:2003,
                        variable = rep(c("global_tas"), each = 3),
                        value = c(3, 4, 5),
                        run_number = c(1:3))

# no years available in df
no_year <- data.frame(year = NA,
                      variable = rep(c("global_tas"), each = 3),
                      value = c(3, 4, 5),
                      run_number = c(1:3))

# criterion for testing
x <- criterion_co2_obs()

# df testing for use cases where df, crit, or score_function are not provided
# by the user
not_df <- NA
not_crit <- NA
not_function <- NA

# test structure and class

test_that("result has proper class and structure", {

  result <- score_hruns(test, x, score_ramp, w1 = 1, w2 = 3)

  # return is a df
  expect_s3_class(result, "data.frame")

  # structure is correct
  expect_true(is.numeric(result$scores))
  expect_true(is.integer(result$run_number))

})

# test error for variable not present

test_that("Error messages are thrown in proper cases", {

  # error when variable in data frame present, but years are not
  expect_error(score_hruns(no_year, x, score_ramp, w1 = 1, w2 = 2),
               regexp = 'criterion year and variable combination not represented in data')

  # error when years in data from are present, but var is not
  expect_error(score_hruns(wrong_var, x, score_ramp, w1 = 1, w2 = 2),
               regexp = 'criterion year and variable combination not represented in data')

  # error when there is no data frame provided
  expect_error(score_hruns(not_df, x, score_ramp, w1 = 1, w2 = 2),
               regexp = 'user supplied x is not a data frame')

  # error when there is no criterion provided
  expect_error(score_hruns(test, not_crit, score_ramp, w1 = 1, w2 = 2),
               regexp = 'user supplied crit is not a criterion')

  # error when there is no scor_function provided
  expect_error(score_hruns(test, x, not_function),
               regexp = 'user supplied score_function is not a function')

})
