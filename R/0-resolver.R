#' Resolve the specification
#'
#' Given the specification of some data to extract find if they are available or not.
#' The specification for selecting a variable shouldn't depend on the data of said variable.
#' @param spec A object extraction specification.
#' @param data The qenv where the specification is evaluated.
#'
#' @returns A specification but resolved: the names and selection is the name of the objects (if possible).
#' @export
#'
#' @examples
#' dataset1 <- datasets(where(is.data.frame))
#' dataset2 <- datasets(where(is.matrix))
#' spec <- c(dataset1, variables("a", "a"))
#' td <- within(teal.data::teal_data(), {
#'   df <- data.frame(a = as.factor(LETTERS[1:5]), b = letters[1:5])
#'   df2 <- data.frame(a = LETTERS[1:5], b = 1:5)
#'   m <- matrix()
#' })
#' resolver(list(spec, dataset2), td)
#' resolver(dataset2, td)
#' resolver(spec, td)
#' spec <- c(dataset1, variables("a", where(is.character)))
#' resolver(spec, td)
resolver <- function(spec, data) {
  checkmate::assert_environment(data)
  if (!is.delayed(spec)) {
    return(spec)
  }

  stopifnot(is.list(spec) || .is.specification(spec))
  if (.is.type(spec)) {
    spec <- list(spec)
    names(spec) <- is(spec[[1]])
    class(spec) <- c("specification", class(spec))
  }

  det <- determine(spec, data)
  if (is.null(names(det))) {
    return(lapply(det, `[[`, 1))
  } else {
    det$x
  }
}

#' A method that should take a type and resolve it.
#'
#' Generic that makes the minimal check on spec.
#' Responsible of subsetting/extract the data received and check that the type matches
#' @param x The specification to resolve.
#' @param data The minimal data required.
#' @return A list with two elements, the `type` resolved and the data extracted.
#' @keywords internal
#' @export
determine <- function(x, data, ...) {
  stopifnot(.is.type(x) || is.list(x) || .is.specification(x))
  if (!is.delayed(x)) {
    return(list(x = x, data = extract(data, x$selected)))
  }
  UseMethod("determine")
}

#' @export
determine.default <- function(x, data, ...) {
  stop("There is not a specific method to pick choices.")
}

#' @export
determine.colData <- function(x, data, ...) {
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Requires SummarizedExperiment package from Bioconductor.")
  }
  data <- as.data.frame(colData(data))
  NextMethod("determine", x)
}

#' @export
determine.datasets <- function(x, data, ...) {
  if (is.null(data)) {
    return(list(x = x, data = NULL))
  } else if (!inherits(data, "qenv")) {
    stop("Please use qenv() or teal_data() objects.")
  }
  # Assumes the object has colnames method (true for major object classes: DataFrame, tibble, Matrix, array)
  # FIXME: What happens if colnames is null: colnames(array(dim = c(4, 2)))
  NextMethod("determine", x)
}

#' @export
determine.specification <- function(x, data, ...) {
  checkmate::assert_class(data, "qenv")

  # Adding some default specifications if they are missing
  if ("values" %in% names(x) && !"variables" %in% names(x)) {
    x <- append(x, list(variables = variables()), length(x) - 1)
  }

  if ("variables" %in% names(x) && !"datasets" %in% names(x)) {
    x <- append(x, list(variables = datasets()), length(x) - 1)
  }

  data_i <- data
  for (i in seq_along(x)) {
    determined_i <- determine(x[[i]], data_i)
    # overwrite so that next x in line receives the corresponding data and specification
    if (is.null(determined_i$x)) {
      next
    }
    x[[i]] <- determined_i$x
    data_i <- determined_i$data
  }

  list(x = x, data = data)
}

#' @export
determine.values <- function(x, data, ...) {
  if (!is.numeric(data)) {
    d <- data
    names(d) <- data
  } else {
    d <- data
  }

  # todo: replace with NextMethod?
  sel <- .eval_select(d, x$choices)
  x$choices <- data[sel]

  sel2 <- .eval_select(d[sel], x$selected)
  x$selected <- data[sel][sel2]

  # Not possible to know what is happening
  if (is.delayed(x)) {
    return(list(x = x, data = NULL))
  }

  list(x = x, data = data[sel])
}

#' @export
determine.variables <- function(x, data, ...) {
  if (is.null(data)) {
    return(list(x = x, data = NULL))
  } else if (length(dim(data)) != 2L) {
    stop(
      "Can't resolve variables from this object of class ",
      toString(sQuote(class(data)))
    )
  }

  if (ncol(data) <= 0L) {
    stop("Can't pull variable: No variable is available.")
  }

  NextMethod("determine", x)
}

#' @export
determine.type <- function(x, data) {
  x <- .determine_choices(x, data)
  x <- .determine_selected(x, data)
  list(x = x, data = extract(data, x$selected))
}

.determine_choices <- function(x, data) {
  if (is.delayed(x)) {
    new_choices <- unique(names(.eval_select(data, x$choices)))
    labels <- vapply(
      new_choices,
      FUN = function(choice) c(attr(data[[choice]], "label"), choice)[1],
      FUN.VALUE = character(1)
    )
    x$choices <- setNames(new_choices, labels)
  }
  x
}

.determine_selected <- function(x, data) {
  checkmate::assert_character(x$choices)
  if (!is(data, "qenv")) {
    data <- extract(data, x$choices)
  } else {
    # Do not extract; selection would be from the data extracted not from the names.
    data <- data[x$choices]
  }
  res <- try(unique(names(.eval_select(data, x$selected))), silent = TRUE)
  if (inherits(res, "try-error")) {
    warning("`selected` outside of possible `choices`. Emptying `selecting` field.", call. = FALSE)
    x$selected <- NULL
  } else {
    x$selected <- res
  }

  x
}
