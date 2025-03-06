delay <- function(x) {
  class(x) <- c("delayed", class(x))
  x
}

#' @export
#' @method is.delayed type
is.delayed.type <- function(x) {

  !all(is.character(x$names)) || !all(is.character(x$select))
}

#' @export is.delayed
#' @method is.delayed transform
is.delayed.transform <- function(x) {
  is.delayed(x$datasets) || is.delayed(x$variables) || is.delayed(x$values)
}

#' @export
#' @method is.delayed default
is.delayed.default <- function(x) {
  inherits(x, "delayed")
}

#' @export
is.delayed <- function(x) {
  UseMethod("is.delayed")
}

resolved <- function(x, variable){
  s <- all(is.character(x$names)) && all(is.character(x$select))

  if (!s && !all(x$select %in% x$names)) {
      stop("Selected ", variable, " not available")
  }

  cl <- class(x)
  class(x) <- setdiff(cl, "delayed")
  x
}

get_datanames <- function(x) {
  if (is.transform(x) && !is.delayed(x$datasets)) {
    x$datasets$names
  } else {
    NULL
  }
}

get_variables <- function(x) {
  if (is.transform(x) && !is.delayed(x$datasets) && !is.delayed(x$variables)) {
    x$variables$names
  } else {
    NULL
  }
}

get_values <- function(x) {
  if (is.transform(x) && !is.delayed(x)) {
    x$values$names
  } else {
    NULL
  }
}
