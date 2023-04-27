#' Example Hector result data
#'
#' @description This is a data set representing the results of an output from
#' \code{\link{iterate_hector}}. The data were produced by an analysis that
#' includes ten iterations of the Hector model. The model results include all
#' Hector variable outputs from the years 1745:2300.
#' This ensures that data being saved from the core includes the data needed to
#' score the Hector runs.
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
