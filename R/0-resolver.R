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
    join_keys_i <- teal.data::join_keys(data)
    for (i in seq_along(x)) {
      determined_i <- determine(x[[i]], data = data_i, join_keys = join_keys_i)
      # overwrite so that next x in line receives the corresponding data and specification
      if (is.null(determined_i$x)) {
        next
      }
      x[[i]] <- determined_i$x
      data_i <- determined_i$data
      join_keys_i <- determined_i$join_keys
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
determine <- function(x, data, join_keys, ...) {
  UseMethod("determine")
}

#' @export
determine.default <- function(x, data, join_keys, ...) {
  stop("There is not a specific method to picks choices.")
}

#' @export
determine.colData <- function(x, data, join_keys, ...) {
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Requires SummarizedExperiment package from Bioconductor.")
  }
  data <- as.data.frame(colData(data))
  NextMethod("determine", x)
}

#' @export
determine.datasets <- function(x, data, join_keys, ...) {
  checkmate::assert_environment(data)
  checkmate::assert_class(join_keys, "join_keys")
  if (is.null(data)) {
    return(list(x = x, data = NULL))
  } else if (!inherits(data, "qenv")) {
    stop("Please use qenv() or teal_data() objects.")
  }

  x <- .determine_choices(x, data)
  x <- .determine_selected(x, data)

  if (length(x$selected) != 1) {
    warning("`dataset` must be a single selection. Forcing to first possible choice.")
    x$selected <- x$choices[1]
  }

  list(x = x, data = data[[x$selected]], join_keys = join_keys[[x$selected]])
}

#' @export
determine.variables <- function(x, data, join_keys, ...) {
  checkmate::assert_multi_class(data, c("data.frame", "tbl_df", "data.table", "DataFrame"))
  checkmate::assert_list(join_keys, null.ok = TRUE)

  if (is.null(data)) {
    return(list(x = x, data = NULL))
  }

  if (ncol(data) <= 0L) {
    stop("Can't pull variable: No variable is available.")
  }

  # ↓ see ?tidyselectors
  for (join_keys_i in join_keys) {
    for (key_column in names(join_keys_i)) {
      attr(data[[key_column]], "join_key") <- TRUE
    }
  }

  x <- .determine_choices(x, data)
  x <- .determine_selected(x, data)

  list(x = x, data = if (length(x$selected) == 1) data[[x$selected]], join_keys = join_keys)
}

#' @export
determine.values <- function(x, data, join_keys, ...) {
  if (is.character(data) || is.factor(data)) {
    d <- data
    names(d) <- data
    # todo: replace with NextMethod?
    x$choices <- unique(names(.eval_select(d, x$choices)))
    names(x$choices) <- x$choices
    if (length(x$choices)) {
      x$selected <- unique(names(.eval_select(x$choices, x$selected)))
    } else {
      x$selected <- NULL
    }

    list(x = x) # nothing more after this (no need to pass data further)
  } else if (is.numeric(data)) {
    x$choices <- range(data)
    x$selected <- if (is.numeric(x$selected)) x$selected else x$choices
    list(x = x)
  }
}

.determine_choices <- function(x, data) {
  choices <- if (inherits(x$choices, "delayed_data")) {
    x$choices$subset(data)
  } else if (is.character(x$choices)) {
    x$choices
  } else {
    idx <- .eval_select(data, x$choices)
    unique(names(data)[idx])
  }
  if (length(choices) == 0) {
    stop("Can't determine choices: ", rlang::as_label(x$choices))
  }

  labels <- vapply(
    choices,
    FUN = function(choice) c(attr(data[[choice]], "label"), choice)[1],
    FUN.VALUE = character(1)
  )
  x$choices <- setNames(choices, labels)
  x
}

.determine_selected <- function(x, data) {
  if (!is.null(x$selected) && length(x$choices)) {
    data <- data[x$choices]
    res <- try(.eval_select(data, x$selected), silent = TRUE)
    x$selected <- if (inherits(res, "try-error")) {
      warning("`selected` outside of possible `choices`. Emptying `selecting` field.", call. = FALSE)
      NULL
    } else {
      unique(names(res))
    }
    if (!isTRUE(x$multiple)) {
      x$selected <- x$selected[1]
    }
  }
  x
}

.is_selected_equal <- function(x, y) {
  all(
    vapply(
      seq_along(x),
      FUN = function(i) {
        identical(as.vector(x[[i]]$selected), as.vector(y[[i]]$selected))
      },
      FUN.VALUE = logical(1)
    )
  )
}


#' Internal method to extract data from different objects
#'
#' Required to resolve a specification into something usable (by comparing with the existing data).
#' Required by merging data based on a resolved specification.
#' @param x Object from which a subset/element is required.
#' @param variable Name of the element to be extracted.
#' @param ... Other arguments passed to the specific method.
#' @keywords internal
.extract <- function(x, variable, ...) {
  UseMethod(".extract")
}


#' @export
.extract.default <- function(x, variable, ..., drop = FALSE) {
  if (length(dim(x)) == 2L || length(variable) > 1L) {
    x[, variable, drop = drop]
  } else {
    x[[variable]]
  }
}

#' @export
.extract.teal_data <- function(x, variable, ...) {
  if (length(variable) > 1L) {
    x[variable]
  } else {
    x[variable]
  }
}
