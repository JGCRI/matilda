# Quiet messages

# Set an option to control whether messages and warning are suppressed.
# Defaults to verbose messaging.
options("matilda.verbose" = FALSE)

# Function to suppress messages
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

# Function to suppress messages
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
