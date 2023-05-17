#' Computing Model Scores Using Ramp Method
#'
#' @description This function uses observed data to compute scores that represent
#' how well modeled values reflect what is happening in reality. Scores that are
#' closer to 1 indicated skillful models, while scores closer to 0 indicate models
#' that produce values that deviate further from observed data.
#'
#' @param m Matrix of values. The first column of the matrix should be
#' a vector of observed data. Subsequent vectors are representative of modeled
#' values for a given variable.
#' @param w1 Difference at which score begins to ramp down from a 1.
#' @param w2 Difference at which score hits 0.
#' @param na.omit How should \code{score_ramp} deal with rows in matrix that
#' contain NAs? Defaults to \code{FALSE}, leaving NAs in the matrix.
#'
#' @return Returns a vector of scores with a length equal to the number of
#' model iterations in the input matrix. Or, if the input matrix has K columns,
#' the return vector will = K - 1.
#' @export
#'
#' @examples
#' # creating sample matrix
#' mat <- matrix(data = 1:15, nrow = 5, ncol = 3)
#'
#' # scoring columns where scores <= 5 will score 1 and scores >= 10 will score 0
#' score_ramp(mat, w1 = 5, w2 = 10)

score_ramp <- function(m, w1, w2, na.omit = FALSE) {

  # ensure that w1 argument is a value of at least 0 - no negative values
  if (w1 < 0) stop("w1 must be at least 0")

  # ensure that w2 is at least as big as w1 - w2 should never be less than w1
  if (w2 < w1) stop("w2 must be at least as big as w1")

  # Initialize a matrix that will be occupied by model scores. The first column
  # of the matrix must be the observed data and will be removed before returning
  # the final output.
  scores_matrix <- matrix(NA_real_, nrow(m), ncol(m))

  # Stop execution if number of columns in the matrix is less the 2
  # indicates that there is only one model result stored in matrix
  stopifnot(ncol(m) > 2)

  # indicate that observed data are in first column of matrix
  obs_data <- m[, 1]

  # throw and error if the modeled data is all NAs
  if (all(is.na(obs_data))) stop("No non-NA values in x")

  # loop across columns of the matrix. For each column (i) after col 2
  for (i in 2:ncol(m)) {

    # indicate modeled data are in subsequent columns
    model_data <- m[, i]

    # checks to ensure equal lengths between modeled and observed data
    if (length(obs_data) != length(model_data)) stop("Length of x must be equal to length of y")

    # throw and error if the modeled data is all NAs
    if (all(is.na(model_data))) stop("No non-NA values in y")

    # omit rows that have NA values in both obs_data and model_data
    if (na.omit) {
      obs_data <- na.omit(obs_data)
      model_data <- na.omit(model_data)}

    # Take absolute difference between obs_data and model_data value
    abs_diffs <- abs(obs_data - model_data)

    # Initializes vector that will be occupied with scores should be same length
    # as observed data
    scores <- rep(NA_real_, length(obs_data))

    # the order of the following two statements matters, because
    # we want a diff exactly equal to w1 to get a score of 1, even when w1 = w2
    scores [abs_diffs >= w2] <- 0
    scores [abs_diffs <= w1] <- 1

    # for abs_diffs between w1 and w2 - compute how far between w1 and w2 the
    # abs_diff value is
    between_w1_w2 <- abs_diffs > w1 & abs_diffs < w2
    w1_w2_frac <- (abs_diffs [between_w1_w2] - w1) / (w2 - w1)

    # for scores between w1 and w2 use (1 - computed distance) as score value
    scores [between_w1_w2] <- 1 - w1_w2_frac

    # store score values
    scores_matrix [, i] <- scores
  }

  # calculates means scores for each column (model iteration)
  agg_scores <- colMeans(scores_matrix [, -1], na.rm = T)

  return(agg_scores)
}
