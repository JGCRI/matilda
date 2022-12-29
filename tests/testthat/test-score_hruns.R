# testing score_hruns
# What should I test
# 1) structure and class etc.
# 2) different variations of hector results
## a) df with NAs -- tested in score_ramp
## b) empty df -- error for object year not found
## c) variable not in df

# data occupied df
test <- data.frame(year = 2001:2003,
                   variable = rep(c("atmos_co2"), each = 3),
                   value = c(630, 635, 700),
                   run_number = c(1:3))

wrong_var <- data.frame(year = 2001:2003,
                        variable = rep(c("global_tas"), each = 3),
                        value = c(3, 4, 5),
                        run_number = c(1:3))

x <- crit_co2_obs()

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

  # error when requested var is not in df
  expect_error(score_hruns(wrong_var, x, score_ramp, w1 = 1, w2 = 2),
               regexp = 'variable in x does not match criterion variable')

})
