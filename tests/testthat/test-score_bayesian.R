# # sample matrix
m <- matrix(c(rep(1, 6), rep(2, 3), rep(3, 3)), nrow = 3, ncol = 4)

## Error sample matrices
# m with only 2 cols
m_2row <- matrix(data = c(1:2), nrow = 2, ncol = 2)
# No non-NAs in obs col
m_NA_obs <- matrix(data = c(rep(NA, 5), 1:15), nrow = 5, ncol = 4)
# No non-NAs in any model data
m_NA_dat <- matrix(data = c(1:5, rep(NA, 15)), nrow = 5, ncol = 4)
# NA present in model data
m_NA_single_case <- matrix(data = c(1:3, 2, NA, 4, 3:5, 4:6), nrow = 3, ncol = 4)

score_bayesian(m_NA_single_case, F)

score_bayesian(m, T)
score_bayesian(m)
