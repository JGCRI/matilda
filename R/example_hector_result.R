#' Example Hector result data
#'
#' @description This is a data set representing the results of an output from
#' \code{\link{iterate_hector}}. The data were produced by an analysis that
#' includes ten iterations of the Hector model for the \code{global_tas} variable
#' between the years 1960 and 2100. In addition, the output selects data from the
#' core that aligns with the scoring criterion (\code{\link{criterion_co2_obs}}).
#' This ensures that data being saved from the core includes the data needed to
#' score the Hector runs with the desired scoring criteria.
#'
#' @format \code{hector_data}
#' is a data frame with 2820 rows and 6 columns:
#' \describe{
#'   \item{scenario}{Scenario being used for the analysis}
#'   \item{year}{Years}
#'   \item{variable}{Variables}
#'   \item{value}{Value of variable}
#'   \item{units}{Units of the variable}
#'   \item{run_number}{Iteration number of each model output}
#' }
"hector_result"
