# Set an option to control whether messages and warning are suppressed.
# Defaults to verbose messaging.
options(matilda.verbose = TRUE)

# Quiet messages

#' Suppressing messages in Matilda functions
#'
#' @param ... Message to print if verbosity option is set to TRUE.
#'
#' @export
#'
matilda_message <- function(...) {

  # Get the current verbose option setting
  verbose <- getOption("matilda.verbose", default = TRUE)

  # Check if verbose
  if (verbose) {
    # If verbose, display a message
    message(...)
  }
  # If verbose FALSE, return nothing
  return()
}

# Quiet warnings

#' Suppressing warnings in Matilda functions
#'
#' @param ... Warning to print if verbosity option is set to TRUE.
#'
#' @export
#'
matilda_warning <- function(...) {

  # Get the current verbose option setting
  verbose <- getOption("matilda.verbose", default = TRUE)

  # Check if verbose
  if (verbose) {
    # If verbose, display warning
    warning(...)
  }
  # If verbose FALSE, return nothing
  return()
}
