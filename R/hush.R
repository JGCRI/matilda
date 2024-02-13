# Quiet messages

# Set an option to control whether messages and warning are suppressed.
# Defaults to verbose messaging.
options("matilda.verbose" = FALSE)

# Function to suppress messages
matilda_message <- function(...) {
  # Check if the verbose option is set to FALSE
  if (getOption("matilda.verbose", default = FALSE)) {
    # If FALSE, exit function without displaying message
    return()
  }
  # If verbose option is TRUE, display message
  message(...)
}


# Quiet warnings

# Function to supress messages
matilda_warning <- function(...) {
  # Check if the vebise oiption if set to FALSE
  if (getOption("matilda.verbose", default = FALSE)) {
    # If FALSE, exit the function without displayign message
    return()
  }
  # If verbose option is TRUE, display message
  warning(...)
}
