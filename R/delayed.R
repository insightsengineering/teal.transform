# Only delay if the type or object really needs it and is not already delayed
delay <- function(x) {
  if (is.delayed(x)) {
    attr(x, "delayed") <- TRUE
  }
  x
}

#' @export
#' @method is.delayed type
is.delayed.type <- function(x) {

  na <- length(x) == 1L && is.na(x)
  if (!na) {
    return(!all(is.character(x$names)) || !all(is.character(x$select)))
  }
  FALSE
}

#' @export
#' @method is.delayed transform
is.delayed.transform <- function(x) {
  is.delayed(x$datasets) || is.delayed(x$variables) || is.delayed(x$values)
}

#' @export
#' @method is.delayed default
is.delayed.default <- function(x) {
  FALSE
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
  attr(x, "delayed") <- NULL
  x
}

get_datasets <- function(x) {
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
