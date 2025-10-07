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
resolver <- function(x, data) {
  checkmate::assert_class(x, "picks")
  checkmate::assert_environment(data)
  if (is.delayed(x)) {
    data_i <- data
    for (i in seq_along(x)) {
      x[[i]] <- if (is.null(data_i)) {
        # remove subsequent elements if nothing selected in the previous one
        NULL
      } else {
        determined_i <- determine(x[[i]], data = data_i)
        data_i <- determined_i$data
        determined_i$x
      }
    }
  }
  x
}

#' A method that should take a type and resolve it.
#'
#' Generic that makes the minimal check on spec.
#' Responsible of subsetting/extract the data received and check that the type matches
#' @param x The specification to resolve.
#' @param data The minimal data required.
#' @return A list with two elements, the `type` resolved and the data extracted.
#' @keywords internal
determine <- function(x, data, ...) {
  UseMethod("determine")
}

#' @export
determine.default <- function(x, data, ...) {
  stop("There is not a specific method to picks choices.")
}

#' @export
determine.colData <- function(x, data) {
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Requires SummarizedExperiment package from Bioconductor.")
  }
  data <- as.data.frame(colData(data))
  NextMethod("determine", x)
}

#' @export
determine.datasets <- function(x, data) {
  checkmate::assert_environment(data)
  if (is.null(data)) {
    return(list(x = x, data = NULL))
  } else if (!inherits(data, "qenv")) {
    stop("Please use qenv() or teal_data() objects.")
  }

  x$choices <- .determine_choices(x$choices, data = data)
  x$selected <- .determine_selected(
    x$selected,
    data = data[intersect(x$choices, names(data))],
    multiple = attr(x, "multiple")
  )
  list(x = x, data = .extract(x, data))
}

#' @export
determine.variables <- function(x, data) {
  checkmate::assert_multi_class(data, c("data.frame", "tbl_df", "data.table", "DataFrame"))

  if (is.null(data)) {
    return(list(x = x, data = NULL))
  }

  if (ncol(data) <= 0L) {
    stop("Can't pull variable: No variables is available.")
  }

  x$choices <- .determine_choices(x$choices, data = data)
  x$selected <- .determine_selected(
    x$selected,
    data = data[intersect(x$choices, colnames(data))],
    multiple = attr(x, "multiple")
  )

  list(x = x, data = .extract(x, data))
}

#' @export
determine.values <- function(x, data) {
  if (is.null(data) || ncol(data) == 0) {
    return(list(x = NULL))
  }
  data <- if (ncol(data) > 1) { # todo: to limit number of possible columns to concat
    apply(data, 1, toString)
  } else {
    data[[1]]
  }

  if (is.character(data) || is.factor(data)) {
    # todo: what to do with NA choices?
    d <- unique(data)
    x$choices <- .determine_choices(x$choices, data = setNames(d, d)) # .determine_* uses names
    x$selected <- if (length(x$choices)) {
      .determine_selected(x$selected, data = setNames(x$choices, x$choices), multiple = attr(x, "multiple"))
    }
    list(x = x) # nothing more after this (no need to pass data further)
  } else if (is.numeric(data) || inherits(data, c("Date", "POSIXct"))) {
    if (all(is.na(data))) {
      return(list(x = NULL))
    }
    x$choices <- range(data, na.rm = TRUE)
    x$selected <- if (is.numeric(x$selected) || inherits(data, c("Date", "POSIXct"))) x$selected else x$choices
    list(x = x)
  }
}

.determine_choices <- function(x, data) {
  if (is.character(x) && length(x)) {
    return(x)
  }

  idx <- .eval_select(data, x)
  choices <- unique(names(data)[idx])
  if (length(choices) == 0) {
    stop("Can't determine choices: ", rlang::as_label(x))
  }

  labels <- vapply(
    choices,
    FUN = function(choice) c(attr(data[[choice]], "label"), choice)[1],
    FUN.VALUE = character(1)
  )
  setNames(choices, labels)
}

.determine_selected <- function(x, data, multiple) {
  if (!is.null(x) && length(data)) {
    res <- try(.eval_select(data, x), silent = TRUE)
    x <- if (inherits(res, "try-error")) {
      warning("`selected` outside of possible `choices`. Emptying `selecting` field.", call. = FALSE)
      NULL
    } else {
      unique(names(res))
    }
    if (!isTRUE(multiple)) {
      x <- x[1]
    }
  }
  x
}

.extract <- function(x, data) {
  if (length(x$selected) == 1 && inherits(x, "datasets")) {
    data[[x$selected]]
  } else if (all(x$selected %in% names(data))) {
    data[x$selected]
  }
}
