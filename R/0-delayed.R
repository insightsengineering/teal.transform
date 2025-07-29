# Only delay if the type or object really needs it and is not already delayed
as.delayed <- function(x) {
  if (is.delayed(x)) {
    attr(x, "delayed") <- TRUE
  }
  x
}

#' Is the specification resolved?
#'
#' Check that the specification is resolved against a given data source.
#' @param x Object to be evaluated.
#' @returns A single logical value.
#' @examples
#' is.delayed(1)
#' is.delayed(variables("df", "df"))
#' is.delayed(variables("df")) # Unknown selection
#' @export
is.delayed <- function(x) {
  UseMethod("is.delayed")
}

#' @export
#' @method is.delayed default
is.delayed.default <- function(x) {
  # FIXME: A warning?
  FALSE
}

# Handling a list of transformers e1 | e2
#' @export
#' @method is.delayed list
is.delayed.list <- function(x) {
  any(vapply(x, is.delayed, logical(1L)))
}

#' @export
#' @method is.delayed specification
is.delayed.specification <- function(x) {
  any(vapply(x, is.delayed, logical(1L)))
}

#' @export
#' @method is.delayed type
is.delayed.type <- function(x) {
  if (!is.na(x)) {
    return(!all(is.character(x$choices)) || !all(is.character(x$selected)))
  }
  FALSE
}

resolved <- function(x) {
  s <- all(is.character(x$choices)) && all(is.character(x$selected))
  if (!s && !all(x$selected %in% x$choices)) {
    stop("Selected not resolved.")
  }
  attr(x, "delayed") <- NULL
  x
}
