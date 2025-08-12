#' Resolve delayed inputs by evaluating the code within the provided datasets
#'
#' @note This is an internal function that is used by [resolve_delayed()].
#' All the methods are used internally only.
#'
#' @param x (`delayed_data`) object to resolve.
#' @param datasets (named `list` of `data.frame`) to use in evaluation.
#' @param keys (named `list` of `character`) to be used as the keys for each dataset.
#' The names of this list must be exactly the same as for datasets.
#'
#' @return Resolved object.
#'
#' @keywords internal
#'
resolve <- function(x, datasets, keys = NULL) {
  checkmate::assert_list(datasets, types = "reactive", min.len = 1, names = "named")
  checkmate::assert_list(keys, "character", names = "named", null.ok = TRUE)
  checkmate::assert(
    .var.name = "keys",
    checkmate::check_names(names(keys), subset.of = names(datasets)),
    checkmate::check_null(keys)
  )

  UseMethod("resolve")
}

#' @describeIn resolve Call [variable_choices()] on the delayed `variable_choices` object.
#' @export
resolve.delayed_variable_choices <- function(x, datasets, keys) {
  if (is.null(x$key)) {
    x$key <- `if`(is.null(keys), character(), keys[[x$data]])
  }
  x$data <- datasets[[x$data]]()
  if (inherits(x$subset, "function")) {
    x$subset <- resolve_delayed_expr(x$subset, ds = x$data, is_value_choices = FALSE)
  }

  do.call("variable_choices", x)
}

#' @describeIn resolve Call [value_choices()] on the delayed `value_choices` object.
#' @export
resolve.delayed_value_choices <- function(x, datasets, keys) {
  x$data <- datasets[[x$data]]()
  if (inherits(x$var_choices, "delayed_variable_choices")) {
    x$var_choices <- resolve(x$var_choices, datasets, keys)
  }
  if (is.function(x$subset)) {
    x$subset <- resolve_delayed_expr(x$subset, ds = x$data, is_value_choices = TRUE)
  }

  do.call("value_choices", x)
}

#' @describeIn resolve Call [select_spec()] on the delayed `choices_selected` object.
#' @export
resolve.delayed_choices_selected <- function(x, datasets, keys) {
  if (inherits(x$selected, "delayed_data")) {
    x$selected <- resolve(x$selected, datasets = datasets, keys)
  }
  x$choices <- resolve(x$choices, datasets = datasets, keys)

  if (!all(x$selected %in% x$choices)) {
    warning(paste(
      "Removing",
      paste(x$selected[which(!x$selected %in% x$choices)]),
      "from 'selected' as not in 'choices' when resolving delayed choices_selected"
    ))
    x$selected <- x$selected[which(x$selected %in% x$choices)]
  }

  do.call("choices_selected", x)
}

#' @describeIn resolve Call [select_spec()] on the delayed specification.
#' @export
resolve.delayed_select_spec <- function(x, datasets, keys) {
  x$choices <- resolve(x$choices, datasets = datasets, keys)
  if (inherits(x$selected, "delayed_data")) {
    x$selected <- resolve(x$selected, datasets = datasets, keys)
  }

  do.call("select_spec", x)
}

#' @describeIn resolve Call [filter_spec()] on the delayed specification.
#' @export
resolve.delayed_filter_spec <- function(x, datasets, keys) {
  if (inherits(x$vars_choices, "delayed_data")) {
    x$vars_choices <- resolve(x$vars_choices, datasets = datasets, keys)
  }
  if (inherits(x$vars_selected, "delayed_data")) {
    x$vars_selected <- resolve(x$vars_selected, datasets = datasets, keys)
  }
  if (inherits(x$choices, "delayed_data")) {
    x$choices <- resolve(x$choices, datasets = datasets, keys)
  }
  if (inherits(x$selected, "delayed_data")) {
    x$selected <- resolve(x$selected, datasets = datasets, keys)
  }

  do.call("filter_spec_internal", x[intersect(names(x), methods::formalArgs(filter_spec_internal))])
}

#' @describeIn resolve Call [data_extract_spec()] on the delayed specification.
#' @export
resolve.delayed_data_extract_spec <- function(x, datasets, keys) {
  x$select <- `if`(
    inherits(x$select, "delayed_data"),
    resolve(x$select, datasets = datasets, keys),
    x$select
  )

  if (any(vapply(x$filter, inherits, logical(1L), "delayed_data"))) {
    idx <- vapply(x$filter, inherits, logical(1), "delayed_data")
    x$filter[idx] <- lapply(x$filter[idx], resolve, datasets = datasets, keys = keys)
  }

  do.call("data_extract_spec", x)
}

#' @describeIn resolve Iterates over elements of the list and recursively calls
#' `resolve`.
#' @export
resolve.list <- function(x, datasets, keys) {
  # If specified explicitly, return it unchanged. Otherwise if delayed, resolve.
  lapply(x, resolve, datasets = datasets, keys = keys)
}

#' @describeIn resolve Default method that does nothing and returns `x` itself.
#' @export
resolve.default <- function(x, datasets, keys) {
  x
}

#' Resolve expression after delayed data are loaded
#'
#'
#' @param x (`function`) Function that is applied on dataset.
#' It must take only a single argument "data" and return character vector with columns / values.
#' @param ds (`data.frame`) Dataset.
#' @param is_value_choices (`logical`) Determines which check of the returned value will be applied.
#'
#' @return `character` vector - result of calling function `x` on dataset `ds`.
#'
#' @keywords internal
#'
resolve_delayed_expr <- function(x, ds, is_value_choices) {
  checkmate::assert_function(x, args = "data", nargs = 1)

  # evaluate function
  res <- do.call(x, list(data = ds))

  # check returned value
  if (is_value_choices) {
    if (!checkmate::test_atomic(res) || anyDuplicated(res)) {
      stop(paste(
        "The following function must return a vector with unique values",
        "from the respective columns of the dataset.\n\n",
        deparse1(bquote(.(x)), collapse = "\n")
      ))
    }
  } else {
    if (!checkmate::test_character(res, any.missing = FALSE) || length(res) > ncol(ds) || anyDuplicated(res)) {
      stop(paste(
        "The following function must return a character vector with unique",
        "names from the available columns of the dataset:\n\n",
        deparse1(bquote(.(x)), collapse = "\n")
      ))
    }
  }

  res
}

#' @export
#' @keywords internal
#'
print.delayed_variable_choices <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, paste("variable_choices with delayed data:", x$data)))
  cat("\n")
  print_delayed_list(x, indent)

  invisible(NULL)
}

#' @export
#' @keywords internal
#'
print.delayed_value_choices <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, paste("value_choices with delayed data: ", x$data)))
  cat("\n")
  print_delayed_list(x, indent)

  invisible(NULL)
}

#' @export
#' @keywords internal
#'
print.delayed_choices_selected <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, paste("choices_selected with delayed data: ", x$choices$data)))
  cat("\n")
  print_delayed_list(x, indent)

  invisible(NULL)
}

#' @export
#' @keywords internal
#'
print.delayed_select_spec <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, paste("select_spec with delayed data:", x$choices$data)))
  cat("\n")
  print_delayed_list(x, indent)

  invisible(NULL)
}

#' @export
#' @keywords internal
#'
print.filter_spec <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, "filter_spec with delayed data:"))
  cat("\n")
  print_delayed_list(x, indent)

  invisible(NULL)
}

#' @export
#' @keywords internal
#'
print.delayed_filter_spec <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, "filter_spec with delayed data:"))
  cat("\n")
  print_delayed_list(x, indent)

  invisible(NULL)
}

#' @export
#' @keywords internal
#'
print.delayed_data_extract_spec <- function(x, indent = 0L, ...) {
  cat(paste("data_extract_spec with delayed data:", x$dataname))
  cat("\n\n")
  print_delayed_list(x)

  invisible(NULL)
}

#' Create indented message
#' @keywords internal
#' @noRd
#'
indent_msg <- function(n, msg) {
  checkmate::assert_integer(n, len = 1, lower = 0, any.missing = FALSE)
  checkmate::assert_character(msg, min.len = 1, any.missing = FALSE)
  indent <- paste(rep("  ", n), collapse = "")

  paste0(indent, msg)
}

#' Common function to print a `delayed_data` object
#' @keywords internal
#' @noRd
#'
print_delayed_list <- function(obj, n = 0L) {
  checkmate::assert_integer(n, len = 1, lower = 0, any.missing = FALSE)
  stopifnot(is.list(obj))

  for (idx in seq_along(obj)) {
    cat(indent_msg(n, ifelse(is.null(names(obj)[[idx]]), paste0("[[", idx, "]]"), paste("$", names(obj)[[idx]]))))
    cat("\n")
    if (inherits(obj[[idx]], "delayed_data")) {
      print(obj[[idx]], n + 1L)
    } else if (is.list(obj[[idx]])) {
      print_delayed_list(obj[[idx]], n + 1L)
    } else {
      cat(indent_msg(n, paste(utils::capture.output(print(obj[[idx]])), collapse = "\n")))
      cat("\n")
    }
  }

  invisible(NULL)
}
