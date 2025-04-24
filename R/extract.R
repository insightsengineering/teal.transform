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

# Get code to be evaluated & displayed by modules
extract_srv <- function(id, input) {
  stopifnot(is.null(input$datasets))
  stopifnot(is.null(input$variables))
  moduleServer(
    id,
    function(input, output, session) {

      obj <- extract(data(), input$datasets)
      method <- paste0("extract.", class(obj))
      method <- dynGet(method, ifnotfound = "extract.default", inherits = TRUE)
      if (identical(method, "extract.default")) {
        b <- get("extract.default")
      } else {
        b <- get(method)
      }
      # Extract definition
      extract_f_def <- call("<-", x = as.name("extract"), value = b)
      q <- eval_code(data(), code = extract_f_def)

      # Extraction happening:
      # FIXME assumes only to variables used
      output <- call("<-", x = as.name(input$datasets), value =
                       substitute(
                         extract(obj, variables),
                         list(obj = as.name(input$datasets),
                              variables = input$variables)))
      q <- eval_code(q, code = output)
    })
}

