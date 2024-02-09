# Quiet messages

options("matilda.verbose" = FALSE)

matilda_message <- function(...) {
  if (getOption("matilda.verbose", default = FALSE)) {
    return()
  }
  message(...)
}


# Quiet warnings

matilda_warning <- function(...) {
  if (getOption("matilda.verbose", default = FALSE)) {
    return()
  }
  warning(...)
}
