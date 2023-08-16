#' Converting Hector Result to Matrix
#'
#' @param df Result data frame from \code{\link{iterate_model}}.
#' @param columns Column name to select for inclusion in the matrix. Defaults to
#' "value" which will use the values for each model iteration.
#'
#' @return A matrix with columns representing a vector of values for each model
#' iteration.
#' @export
#'
#' @examples
#' # Matrix hector result using model values
#' hector_matrix(matilda_result, columns = "value")
hector_matrix <- function(df, columns = "value") {
  split_list <- split(df, df$run_number)

  subset <-
    lapply(split_list, function(x) {
      subset(x, select = columns)
    })

  df_wide <- do.call(cbind, subset)
  colnames(df_wide) <- NULL

  return(as.matrix(df_wide))
}
