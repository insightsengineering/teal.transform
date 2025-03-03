delay <- function(x) {
  class(x) <- "delayed"
  x
}

is.delayed <- function(x) {
  inherits(x, "delayed")
}
