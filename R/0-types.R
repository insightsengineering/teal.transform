#' @rdname types
#' @name Types
#' @title Type specification
#' @description
#' Define how to select and extract data
#' @param choices <[`tidy-select`][dplyr::dplyr_tidy_select]> One unquoted expression to be used to pick the choices.
#' @param selected <[`tidy-select`][dplyr::dplyr_tidy_select]> One unquoted expression to be used to pick from choices to be selected.
#' @returns An object of the same class as the function with two elements: names the content of x, and select.
#' @examples
#' datasets()
#' datasets("A")
#' c(datasets("A"), datasets("B"))
#' datasets(where(is.data.frame))
#' c(datasets("A"), variables(where(is.numeric)))
NULL

#' @describeIn types specify a selector.
#' @export
spec <- function(...) {
  spec <- list(...)
  names(spec) <- vapply(spec, FUN = is, FUN.VALUE = character(1))
  structure(spec, class = c("specification", "list"))
}

#' @describeIn types Specify datasets.
#' @export
datasets <- function(choices = tidyselect::everything(), selected = 1) {
  out <- .selected_choices(
    choices = if (.is_tidyselect(choices)) rlang::enquo(choices) else choices,
    selected = if (.is_tidyselect(selected)) rlang::enquo(selected) else selected,
    multiple = FALSE
  )
  class(out) <- c("datasets", class(out))
  out
}

#' @describeIn types Specify variables.
#' @export
variables <- function(choices = tidyselect::everything(), selected = 1, multiple = FALSE) {
  out <- .selected_choices(
    choices = if (.is_tidyselect(choices)) rlang::enquo(choices) else choices,
    selected = if (.is_tidyselect(selected)) rlang::enquo(selected) else selected,
    multiple = multiple
  )
  class(out) <- c("variables", class(out))
  out
}

#' @describeIn types Specify colData.
#' @export
mae_colData <- function(choices = tidyselect::everything(), selected = 1, multiple = FALSE) {
  out <- .selected_choices(
    choices = if (.is_tidyselect(choices)) rlang::enquo(choices) else choices,
    selected = if (.is_tidyselect(selected)) rlang::enquo(selected) else selected,
    multiple = multiple
  )
  class(out) <- c("colData", class(out))
  out
}

#' @describeIn types Specify values.
#' @export
values <- function(choices = tidyselect::everything(), selected = 1, multiple = FALSE) {
  out <- .selected_choices(
    choices = if (.is_tidyselect(choices)) rlang::enquo(choices) else choices,
    selected = if (.is_tidyselect(selected)) rlang::enquo(selected) else selected,
    multiple = multiple
  )
  class(out) <- c("values", class(out))
  out
}

#' @export
anyNA.type <- function(x, recursive = FALSE) {
  anyNA(unclass(x[c("choices", "selected")]), recursive = recursive)
}

#' @export
is.na.type <- function(x) anyNA(x)

#' @export
print.type <- function(x, ...) {
  if (is.na(x)) {
    cat("Nothing possible")
    return(x)
  }

  choices_fns <- .count_functions(x$choices)

  msg_values <- character()
  choices_values <- length(x$choices) - sum(choices_fns)
  if (any(choices_fns)) {
    msg_values <- paste0(msg_values, sum(choices_fns), " functions for possible choices.",
      collapse = "\n"
    )
  }
  if (choices_values) {
    msg_values <- paste0(msg_values, paste0(rlang::as_label(x$choices[!choices_fns]), collapse = ", "),
      " as possible choices.",
      collapse = "\n"
    )
  }

  selected_fns <- .count_functions(x$selected)

  msg_sel <- character()
  sel_values <- length(x$selected) - sum(selected_fns)
  if (any(selected_fns)) {
    msg_sel <- paste0(msg_sel, sum(selected_fns), " functions to select.",
      collapse = "\n"
    )
  }
  if (sel_values) {
    msg_sel <- paste0(msg_sel, paste0(rlang::as_label(x$selected[!selected_fns]), collapse = ", "),
      " selected.",
      collapse = "\n"
    )
  }

  cat(msg_values, msg_sel)
  return(x)
}


.count_functions <- function(x) {
  if (is.list(x)) {
    vapply(x, is.function, logical(1L))
  } else {
    FALSE
  }
}

.is.specification <- function(x) {
  inherits(x, "specification")
}

.is.tidyselect <- function(x) {
  err <- try(force(x), silent = TRUE)
  inherits(err, "error") && grepl("must be used within a *selecting*", err$message)
}

.is.type <- function(x) {
  inherits(x, "type")
}

.selected_choices <- function(choices,
                              selected,
                              multiple = length(selected) > 1,
                              keep_order = FALSE) {
  is_choices_delayed <- inherits(choices, "quosure")
  is_selected_delayed <- inherits(selected, "quosure")
  if (is_choices_delayed && !is_selected_delayed) {
    warning(
      deparse(sys.call(-1)),
      "\n - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) might lead to the", "situation where `selected` is not in dynamically obtained `choices`.",
      call. = FALSE
    )
  }

  out <- structure(
    list(choices = choices, selected = selected),
    multiple = multiple,
    keep_order = keep_order,
    class = "type"
  )
}

.valid_specification <- function(x) {
  !((.is.type(x) || .is.specification(x)))
}


#' Is an object created using tidyselect
#'
#' `choices` and `selected` can be provided using `tidyselect`, (e.g. [tidyselect::everything()]
#' [tidyselect::match()], [tidyselect::starts_with()]). These functions can't be called
#' independently but rather as an argument of function which consumes them.
#' `.is_tidyselect` safely determines if `x` can be evaluated with `tidyselect::eval_select()`
#' @param x `choices` or `selected`
#' @return `logical(1)`
#' @internal
.is_tidyselect <- function(x) {
  out <- tryCatch(x, error = function(e) e)
  inherits(out, "error") && # because tidyselect calls return error if not used in select
    grepl("must be used within a \\*selecting\\* function", paste(out$message, collapse = "\n")) ||
    checkmate::test_function(out, args = "x") || # because tidyselect::where(foo) returns a function(x, ...)
    checkmate::test_integerish(out) # integer is not a column/dataset name
}
