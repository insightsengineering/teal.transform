#' Internal method to extract data from different objects
#'
#' Required to resolve a specification into something usable (by comparing with the existing data).
#' Required by merging data based on a resolved specification.
#' @param x Object from which a subset/element is required.
#' @param variable Name of the element to be extracted.
#' @param ... Other arguments passed to the specific method.
#' @export
#' @examples
#' extract(iris, "Sepal.Length")
extract <- function(x, variable, ...) {
  UseMethod("extract")
}

# Cases handled by the default method
# @export
# extract.MultiAssayExperiment <- function(x, variable) {
#   # if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
#   #   stop("Required to have MultiAssayExperiment's package.")
#   # }
#   x[, variable, drop = TRUE]
# }
#
# @export
# extract.DataFrame <- function(x, variable) {
#   # if (!requireNamespace("S4Vectors", quietly = TRUE)) {
#   #   stop("Required to have S4Vectors's package.")
#   # }
#   x[, variable, drop = TRUE]
# }
#
# @export
# extract.matrix <- function(x, variable) {
#   x[, variable, drop = TRUE]
# }

#' @export
extract.default <- function(x, variable, ..., drop = TRUE) {
  if (length(dim(x)) == 2L || length(variable) > 1L) {
    x[, variable, drop = drop]
  } else {
    x[[variable]]
  }
}

# @export
# @method extract data.frame
# extract.data.frame <- function(x, variable) {
#   # length(variable) == 1L
#   x[, variable, drop = TRUE]
# }

# @export
# extract.qenv <- function(x, variable) {
#   x[[variable]]
# }
