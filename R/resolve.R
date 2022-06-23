#' Resolve delayed inputs by evaluating the code within the provided datasets
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x Object of class `delayed_data` to resolve.
#' @param datasets A named list of type `data.frame` to use for evaluation.
#' @param join_keys A named list of type `character` to be used as the join keys for each dataset. The names of this
#'  list must be exactly the same as for datasets.
#'
#' @return Resolved object.
#'
#' @export
#'
#' @examples
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' attr(ADSL, "keys") <- teal.data::get_cdisc_keys("ADSL")
#' data_list <- list(ADSL = shiny::reactive(ADSL))
#' keys <- list(ADSL = attr(ADSL, "keys"))
#' shiny::isolate({
#'   # value_choices example
#'   v1 <- value_choices("ADSL", "SEX", "SEX")
#'   v1
#'   resolve(v1, data_list, keys)
#'
#'   # variable_choices example
#'   v2 <- variable_choices("ADSL", c("BMRKR1", "BMRKR2"))
#'   v2
#'   resolve(v2, data_list, keys)
#'
#'   # data_extract_spec example
#'   adsl_filter <- filter_spec(
#'     vars = variable_choices("ADSL", "SEX"),
#'     sep = "-",
#'     choices = value_choices("ADSL", "SEX", "SEX"),
#'     selected = "F",
#'     multiple = FALSE,
#'     label = "Choose endpoint and Censor"
#'   )
#'
#'   adsl_select <- select_spec(
#'     label = "Select variable:",
#'     choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
#'     selected = "BMRKR1",
#'     multiple = FALSE,
#'     fixed = FALSE
#'   )
#'
#'   adsl_de <- data_extract_spec(
#'     dataname = "ADSL",
#'     select = adsl_select,
#'     filter = adsl_filter
#'   )
#'
#'   resolve(adsl_filter, data_list, keys)
#'   resolve(adsl_select, data_list, keys)
#'   resolve(adsl_de, data_list, keys)
#'
#'   # nested list (arm_ref_comp)
#'   arm_ref_comp <- list(
#'     ARMCD = list(
#'       ref = variable_choices("ADSL"),
#'       comp = variable_choices("ADSL")
#'     )
#'   )
#'
#'   resolve(arm_ref_comp, data_list, keys)
#' })
resolve <- function(x, datasets, join_keys) {
  checkmate::assert_list(datasets, type = "reactive", min.len = 1, names = "named")
  checkmate::assert_list(join_keys, types = "character", len = length(datasets), names = "named")
  checkmate::assert_names(names(join_keys), identical.to = names(datasets))
  UseMethod("resolve")
}

#' @export
resolve.delayed_variable_choices <- function(x, datasets, join_keys) { # nolint
  if (is.null(x$key)) {
    x$key <- `if`(is.null(join_keys), character(), join_keys[[x$data]])
  }
  x$data <- datasets[[x$data]]()
  if (inherits(x$subset, "function")) {
    x$subset <- resolve_delayed_expr(x$subset, ds = x$data, is_value_choices = FALSE)
  }
  return(do.call("variable_choices", x))
}

#' @export
resolve.delayed_value_choices <- function(x, datasets, join_keys) { # nolint
  x$data <- datasets[[x$data]]()
  if (is.function(x$subset)) {
    x$subset <- resolve_delayed_expr(x$subset, ds = x$data, is_value_choices = TRUE)
  }
  return(do.call("value_choices", x))
}

#' @export
resolve.delayed_choices_selected <- function(x, datasets, join_keys) { # nolint
  if (inherits(x$selected, "delayed_data")) {
    x$selected <- resolve(x$selected, datasets = datasets, join_keys)
  }
  x$choices <- resolve(x$choices, datasets = datasets, join_keys)

  if (!all(x$selected %in% x$choices)) {
    logger::log_warn(paste(
      "Removing",
      paste(x$selected[which(!x$selected %in% x$choices)]),
      "from 'selected' as not in 'choices' when resolving delayed choices_selected"
    ))
    x$selected <- x$selected[which(x$selected %in% x$choices)]
  }

  return(do.call("choices_selected", x))
}

#' @export
resolve.delayed_select_spec <- function(x, datasets, join_keys) { # nolint
  x$choices <- resolve(x$choices, datasets = datasets, join_keys)
  if (inherits(x$selected, "delayed_data")) {
    x$selected <- resolve(x$selected, datasets = datasets, join_keys)
  }
  return(do.call("select_spec", x))
}

#' @export
resolve.delayed_filter_spec <- function(x, datasets, join_keys) { # nolint
  if (inherits(x$vars_choices, "delayed_data")) {
    x$vars_choices <- resolve(x$vars_choices, datasets = datasets, join_keys)
  }
  if (inherits(x$vars_selected, "delayed_data")) {
    x$vars_selected <- resolve(x$vars_selected, datasets = datasets, join_keys)
  }
  if (inherits(x$choices, "delayed_data")) {
    x$choices <- resolve(x$choices, datasets = datasets, join_keys)
  }
  if (inherits(x$selected, "delayed_data")) {
    x$selected <- resolve(x$selected, datasets = datasets, join_keys)
  }

  return(do.call("filter_spec_internal", x[intersect(names(x), methods::formalArgs(filter_spec_internal))]))
}

#' @export
resolve.delayed_data_extract_spec <- function(x, datasets, join_keys) { # nolint
  x$select <- `if`(
    inherits(x$select, "delayed_data"),
    resolve(x$select, datasets = datasets, join_keys),
    x$select
  )

  if (any(vapply(x$filter, inherits, logical(1L), "delayed_data"))) {
    idx <- vapply(x$filter, inherits, logical(1), "delayed_data")
    x$filter[idx] <- lapply(x$filter[idx], resolve, datasets = datasets, join_keys = join_keys)
  }

  return(do.call("data_extract_spec", x))
}

#' @export
resolve.list <- function(x, datasets, join_keys) { # nolint

  # If specified explicitly, return it unchanged. Otherwise if delayed, resolve.
  res <- lapply(x, resolve, datasets = datasets, join_keys = join_keys)
  return(res)
}

#' @export
resolve.default <- function(x, datasets, join_keys) {
  return(x)
}

#' Resolve expression after delayed data are loaded
#'
#'
#' @param x (`function`) Function that is applied on dataset.
#' It must take only a single argument "data" and return character vector with columns / values.
#' @param ds (`data.frame`) `TealDataset` on which the function is applied to.
#' @param is_value_choices (`logical`) Determines which check of the returned value will be applied.
#'
#' @return Character vector - result of calling function `x` on dataset `ds`.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # get only possible factor variables from mtcars dataset
#' resolve_delayed_expr(
#'   function(data) {
#'     idx <- vapply(data, function(x) is.numeric(x) && length(unique(x)) <= 6, logical(1))
#'     colnames(data)[idx]
#'   },
#'   ds = mtcars,
#'   is_value_choices = FALSE
#' )
#' }
resolve_delayed_expr <- function(x, ds, is_value_choices) {
  checkmate::assert_function(x, args = "data", nargs = 1)

  # evaluate function
  res <- do.call("x", list(data = ds))

  # check returned value
  if (is_value_choices) {
    if (!is.atomic(res) || anyDuplicated(res)) {
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

  return(res)
}

#' @keywords internal
#' @export
print.delayed_variable_choices <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, paste("variable_choices with delayed data:", x$data)))
  cat("\n")
  print_delayed_list(x, indent)
  return(invisible(NULL))
}

#' @keywords internal
#' @export
print.delayed_value_choices <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, paste("value_choices with delayed data: ", x$data)))
  cat("\n")
  print_delayed_list(x, indent)
  return(invisible(NULL))
}

#' @keywords internal
#' @export
print.delayed_choices_selected <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, paste("choices_selected with delayed data: ", x$choices$data)))
  cat("\n")
  print_delayed_list(x, indent)
  return(invisible(NULL))
}

#' @keywords internal
#' @export
print.delayed_select_spec <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, paste("select_spec with delayed data:", x$choices$data)))
  cat("\n")
  print_delayed_list(x, indent)
  return(invisible(NULL))
}

#' @keywords internal
#' @export
print.filter_spec <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, "filter_spec with delayed data:"))
  cat("\n")
  print_delayed_list(x, indent)
  return(invisible(NULL))
}

#' @keywords internal
#' @export
print.delayed_filter_spec <- function(x, indent = 0L, ...) {
  cat(indent_msg(indent, "filter_spec with delayed data:"))
  cat("\n")
  print_delayed_list(x, indent)
  return(invisible(NULL))
}

#' @keywords internal
#' @export
print.delayed_data_extract_spec <- function(x, indent = 0L, ...) {
  cat(paste("data_extract_spec with delayed data:", x$dataname))
  cat("\n\n")
  print_delayed_list(x)
  return(invisible(NULL))
}

indent_msg <- function(n, msg) {
  checkmate::assert_integer(n, len = 1, lower = 0, any.missing = FALSE)
  checkmate::assert_character(msg, min.len = 1, any.missing = FALSE)
  indent <- paste(rep("  ", n), collapse = "")
  return(paste0(indent, msg))
}

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
  return(invisible(NULL))
}
