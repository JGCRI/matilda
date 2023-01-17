#' Set Parameter Values to Hector Variables
#'
#' @description This function takes randomly generated parameter values produced by
#' \code{\link{generate_params}} and sets each value Hector variables for the
#' supplied Hector core.
#'
#' @param param_values An object of generated parameter values. Must be of
#' numeric class. Column names must match parameter functions in Hector.
#' @param core Hector core object
#'
#' @return Message indicating the values set to each parameter variable.
#' @export
#'
#' @examples
#' ssp245 <- system.file("input/hector_ssp245.ini", package = "hector")
#' core <- newcore(ssp245)
#' params <- generate_params(10)
#' param_vals <- unlist(params [1, ])
#' set_params(core, param_vals)

set_params <- function(core, param_values) {

  # If parameters values are not numeric - stop and send error
  if( length(param_values) == 0) {
    warning("no parameters")
    return()
  }

  if( !is.numeric(param_values)) stop("not numeric")

  if( is.null(names(param_values))) stop("no names")

  # for each parameter along the sequence of parameters in param_values do action
  for (i in seq_along(param_values)) {

    # use param_values names to create fn_name - storing the function names for
    # variables
    fn_name <- names(param_values)[i]

    # create var for setvar() - use do.call() to store variable names in list from
    # fn_name
    var <- do.call(fn_name, list())

    # create var_units for setvar() - use hector's getunits() to store units of var
    # in object
    var_units <- getunits(var)

    # send a message for what each var is being set to
    message("setting ", var, " to ", param_values[i])

    # set variables to be passed to
    setvar(core, NA, var = var, values = param_values[i], unit = var_units)

  }
}
