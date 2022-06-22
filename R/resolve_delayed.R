#' Resolve delayed inputs by evaluating the code within the provided datasets
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x Object of class `delayed_data` to resolve.
#' @param datasets Object of class `FilteredData` to use for evaluation.
#'
#' @return Resolved object.
#'
#' @export
#'
#' @examples
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' shiny::isolate({
#'   ds <- teal.slice::init_filtered_data(
#'     list(ADSL = list(
#'       dataset = ADSL, keys = teal.data::get_cdisc_keys("ADSL"), parent = character(0)
#'     )),
#'     cdisc = TRUE
#'   )
#'
#'   # value_choices example
#'   v1 <- value_choices("ADSL", "SEX", "SEX")
#'   v1
#'   resolve_delayed(v1, ds)
#'
#'   # variable_choices example
#'   v2 <- variable_choices("ADSL", c("BMRKR1", "BMRKR2"))
#'   v2
#'   resolve_delayed(v2, ds)
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
#'   resolve_delayed(adsl_filter, ds)
#'   resolve_delayed(adsl_select, ds)
#'   resolve_delayed(adsl_de, ds)
#'
#'   # nested list (arm_ref_comp)
#'   arm_ref_comp <- list(
#'     ARMCD = list(
#'       ref = variable_choices("ADSL"),
#'       comp = variable_choices("ADSL")
#'     )
#'   )
#'
#'   resolve_delayed(arm_ref_comp, ds)
#' })
resolve_delayed <- function(x, datasets) {
  stopifnot(inherits(datasets, "FilteredData"))
  UseMethod("resolve_delayed")
}

#' @export
resolve_delayed.delayed_variable_choices <- function(x, datasets) { # nolint
  if (is.null(x$key)) {
    x$key <- datasets$get_keys(x$data)
  }
  x$data <- datasets$get_data(x$data)
  if (inherits(x$subset, "function")) {
    x$subset <- resolve_delayed_expr(x$subset, ds = x$data, is_value_choices = FALSE)
  }
  return(do.call("variable_choices", x))
}

#' @export
resolve_delayed.delayed_value_choices <- function(x, datasets) { # nolint
  x$data <- datasets$get_data(x$data)
  if (is.function(x$subset)) {
    x$subset <- resolve_delayed_expr(x$subset, ds = x$data, is_value_choices = TRUE)
  }
  return(do.call("value_choices", x))
}

#' @export
resolve_delayed.delayed_choices_selected <- function(x, datasets) { # nolint
  if (inherits(x$selected, "delayed_data")) {
    x$selected <- resolve_delayed(x$selected, datasets = datasets)
  }
  x$choices <- resolve_delayed(x$choices, datasets = datasets)

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
resolve_delayed.delayed_select_spec <- function(x, datasets) { # nolint
  x$choices <- resolve_delayed(x$choices, datasets = datasets)
  if (inherits(x$selected, "delayed_data")) {
    x$selected <- resolve_delayed(x$selected, datasets = datasets)
  }
  return(do.call("select_spec", x))
}

#' @export
resolve_delayed.delayed_filter_spec <- function(x, datasets) { # nolint
  if (inherits(x$vars_choices, "delayed_data")) {
    x$vars_choices <- resolve_delayed(x$vars_choices, datasets = datasets)
  }
  if (inherits(x$vars_selected, "delayed_data")) {
    x$vars_selected <- resolve_delayed(x$vars_selected, datasets = datasets)
  }
  if (inherits(x$choices, "delayed_data")) {
    x$choices <- resolve_delayed(x$choices, datasets = datasets)
  }
  if (inherits(x$selected, "delayed_data")) {
    x$selected <- resolve_delayed(x$selected, datasets = datasets)
  }

  return(do.call("filter_spec_internal", x[intersect(names(x), methods::formalArgs(filter_spec_internal))]))
}

#' @export
resolve_delayed.delayed_data_extract_spec <- function(x, datasets) { # nolint
  x$select <- `if`(inherits(x$select, "delayed_data"), resolve_delayed(x$select, datasets = datasets), x$select)

  if (any(vapply(x$filter, inherits, logical(1L), "delayed_data"))) {
    idx <- vapply(x$filter, inherits, logical(1), "delayed_data")
    x$filter[idx] <- lapply(x$filter[idx], resolve_delayed, datasets = datasets)
  }

  return(do.call("data_extract_spec", x))
}

#' @export
resolve_delayed.list <- function(x, datasets) { # nolint

  # If specified explicitly, return it unchanged. Otherwise if delayed, resolve.
  res <- lapply(x, resolve_delayed, datasets)
  return(res)
}

#' @export
resolve_delayed.default <- function(x, datasets) {
  return(x)
}
