#' @export
transform <- function() {
  o <- list(dataset = NA, variables = NA, values = NA)
  class(o) <- c("delayed", "transform")
  o
}

is.transform <- function(x) {
  inherits(x, "transform")
}

#' @export
dataset <- function(x, select = first_choice) {
  o <- list(names = x, select = select)
  class(o) <- c("delayed", "dataset")
  o
}

is.dataset <- function(x) {
  inherits(x, "dataset")
}

#' @export
print.dataset <- function(x) {
  if (is.delayed(x)) {
    cat("Delayed dataset for:", x$names)
  } else {
    cat("Dataset for:", x$names)
  }
}

#' @export
variable <- function(x, select = first_choice) {
  o <- list(names = x, select = select)
  class(o) <- c("delayed", "variable")
  o
}

is.variable <- function(x) {
  inherits(x, "variable")
}

#' @export
print.variable <- function(x) {
  if (is.delayed(x)) {
    cat("Delayed variable for:", x$names)
  } else {
    cat("Variable for:", x$names)
  }
}

value <- function(x, select = first_choice) {
  o <- list(names = x, select = select)
  class(o) <- c("delayed", "value")
  o
}

is.value <- function(x) {
  inherits(x, "value")
}

#' @export
print.value <- function(x) {
  if (is.delayed(x)) {
    cat("Delayed value for:", x$names)
  } else {
    cat("Value for:", x$names)
  }
}
