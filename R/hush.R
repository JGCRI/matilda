# Set an option to control whether messages and warning are suppressed.
# Defaults to verbose messaging.
options("matilda.verbose" = FALSE)

# Quiet messages

#' Suppressing messages in Matilda functions
#'
#' @param ... Message to print if verbosity option is set to TRUE.
#'
#' @export
#'
matilda_message <- function(...) {

  # Get the current verbose option setting
  message.option <- getOption("matilda.verbose")

  # Check if the verbose option is set to FALSE
  if (message.option == FALSE) {
    # If FALSE, exit function without displaying message
    return()
  }
  # If verbose option is TRUE, display message
  message(...)
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
  warning.option <- getOption("matilda.verbose")

  # Check if the verbose option if set to FALSE
  if (warning.option == FALSE) {
    # If FALSE, exit the function without displaying message
    return()
  }
  # If verbose option is TRUE, display message
  warning(...)
}
