# Only delay if the type or object really needs it and is not already delayed
delay <- function(x) {
  if (is.delayed(x)) {
    attr(x, "delayed") <- TRUE
  }
  x
}

#' Is the specification resolved?
#'
#' Check that the specification is resolved against a given data source.
#' @param specification Object to be evaluated.
#' @returns A single logical value.
#' @examples
#' is.delayed(1)
#' is.delayed(variables("df", "df"))
#' is.delayed(variables("df")) # Unknown selection
#' @export
is.delayed <- function(specification) {
  UseMethod("is.delayed")
}

#' @export
#' @method is.delayed default
is.delayed.default <- function(specification) {
  # FIXME: A warning?
  FALSE
}

# Handling a list of transformers e1 | e2
#' @export
#' @method is.delayed list
is.delayed.list <- function(specification) {
  any(vapply(specification, is.delayed, logical(1L)))
}

#' @export
#' @method is.delayed transform
is.delayed.transform <- function(specification) {
  any(vapply(specification, is.delayed, logical(1L)))
}

#' @export
#' @method is.delayed type
is.delayed.type <- function(specification) {
  if (!is.na(specification)) {
    return(!all(is.character(specification$names)) || !all(is.character(specification$select)))
  }
  FALSE
}

resolved <- function(specification, type = is(specification)) {
  s <- all(is.character(specification$names)) && all(is.character(specification$select))

  if (!s && !all(specification$select %in% specification$names)) {
    stop("Selected ", type, " not resolved.")
  }
  attr(specification, "delayed") <- NULL
  specification
}
