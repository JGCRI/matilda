#' Example Hector result data
#'
#' @description This is a data set representing the results of an output from
#' \code{\link{iterate_hector}}. The data were produced by an analysis that
#' includes ten iterations of the Hector model for 'global_tas' and 'atmos_co2'
#' variables between the years 1960 and 2100.
#'
#' @format \code{hector_data}
#' is a data frame with 4040 rows and 6 columns:
#' \describe{
#'   \item{scenario}{Scenario being used for the analysis}
#'   \item{year}{Years}
#'   \item{variable}{Variables}
#'   \item{value}{Value of variable}
#'   \item{units}{Units of the variable}
#'   \item{run_number}{Iteration number of each model output}
#' }
"hector_result"
