#' Choices/selected settings
#'
#' Define choices and default selection of variables from datasets.
#' @param choices <[`tidy-select`][dplyr::dplyr_tidy_select] or `character`>
#'  One unquoted expression to be used to picks the choices.
#' @param selected <[`tidy-select`][dplyr::dplyr_tidy_select] or `character`>
#'  One unquoted expression to be used to picks from choices to be selected.
#' @param multiple <`logical(1)`> if more than one selection is possible.
#' @param fixed <`logical(1)`> selection will be fixed and not possible to change interactively.
#' @param ... additional arguments delivered to `pickerInput`
#'
#' @returns `picks` object containing specified settings
#' @examples
#'
#' # Initialize selector for `iris` to select columns between `Sepal.Length` and `Petal.Width`
#' #  with first
#' picks(
#'   datasets(choices = "iris"),
#'   variables(choices = Sepal.Length:Petal.Width, selected = 1)
#' )
#' picks(
#'   datasets(choices = c("iris", "mtcars"), selected = "iris"),
#'   variables(choices = tidyselect::everything(), selected = 1)
#' )
#' picks(
#'   datasets(choices = c("iris", "mtcars"), selected = 1),
#'   variables(choices = tidyselect::where(is.numeric), selected = 1)
#' )
#' picks(
#'   datasets(choices = tidyselect::everything(), selected = 1),
#'   variables(choices = is_categorical(min.len = 2, max.len = 15), selected = 1:2)
#' )
#' @rdname types
#' @name Types
NULL

#' @describeIn types specify a selector.
#' @export
picks <- function(...) {
  # todo: assert that datasets is on the first place?
  picks <- list(...)
  checkmate::assert_list(picks, types = "type")
  names(picks) <- vapply(picks, FUN = is, FUN.VALUE = character(1))
  for (i in seq_along(picks)) {
    if (isTRUE(attr(picks[[i]], "multiple")) && i < length(picks)) {
      stop(
        names(picks)[i], " has a property `multiple = TRUE` which is forbidden if there are any following elements",
        " depending on its selection."
      )
    }
  }
  structure(picks, class = c("picks", "list"))
}

#' @export
datanames <- function(x) {
  if (inherits(x, "picks")) {
    x <- list(x)
  }
  checkmate::assert_list(x, c("picks", "NULL"))
  unique(unlist(lapply(x, function(x) {
    if (is.character(x$datasets$choices)) x$datasets$choices
  })))
}

#' @describeIn types Specify datasets.
#' @export
datasets <- function(choices = tidyselect::everything(),
                     selected = 1,
                     fixed = !.is_tidyselect(choices) && length(choices) == 1,
                     ...) {
  # todo: implement ... in pickerInput like `max-options`, `allow-clear`
  out <- .selected_choices(
    choices = if (.is_tidyselect(choices)) rlang::enquo(choices) else choices,
    selected = if (.is_tidyselect(selected)) rlang::enquo(selected) else selected,
    multiple = FALSE,
    fixed = fixed,
    ...
  )
  class(out) <- c("datasets", class(out))
  out
}

#' @describeIn types Specify variables.
#' @export
variables <- function(choices = tidyselect::everything(),
                      selected = 1,
                      multiple = !.is_tidyselect(selected) && length(selected) > 1,
                      fixed = !.is_tidyselect(choices) && length(choices) == 1,
                      ...) {
  out <- .selected_choices(
    choices = if (.is_tidyselect(choices)) rlang::enquo(choices) else choices,
    selected = if (.is_tidyselect(selected)) rlang::enquo(selected) else selected,
    multiple = multiple,
    fixed = fixed,
    ...
  )
  class(out) <- c("variables", class(out))
  out
}

#' @describeIn types Specify variables.
#' @export
values <- function(choices = tidyselect::everything(),
                   selected = tidyselect::everything(),
                   multiple = TRUE,
                   fixed = !.is_tidyselect(choices) && length(choices) == 1,
                   ...) {
  out <- .selected_choices(
    choices = if (.is_tidyselect(choices)) rlang::enquo(choices) else choices,
    selected = if (.is_tidyselect(selected)) rlang::enquo(selected) else selected,
    multiple = multiple,
    fixed = fixed,
    ...
  )
  class(out) <- c("values", class(out))
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

#' @export
anyNA.type <- function(x, recursive = FALSE) {
  anyNA(unclass(x[c("choices", "selected")]), recursive = recursive)
}

#' @export
is.na.type <- function(x) anyNA(x)

#' @export
print.type <- function(x, ...) {
  cat(
    "choices :", .toString(x$choices),
    "\nselected:", .toString(x$selected)
  )
  return(x)
}

.toString <- function(x) {
  if (inherits(x, "quosure")) {
    rlang::as_label(x)
  } else if (is.vector(x)) {
    toString(x, width = 30)
  } else if (is.null(x)) {
    "~"
  }
}


.is.picks <- function(x) {
  inherits(x, "picks")
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
                              keep_order = FALSE,
                              fixed = FALSE,
                              ...) {
  is_choices_delayed <- inherits(choices, "quosure") ||
    checkmate::test_multi_class(choices, c("variable_choices", "value_choices"))
  is_selected_eager <- is.character(selected)

  if (is_choices_delayed && is_selected_eager) {
    warning(
      deparse(sys.call(-1)),
      "\n - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) might lead to the", "situation where `selected` is not in dynamically obtained `choices`.",
      call. = FALSE
    )
  }

  if (inherits(choices, "choices_labeled")) {
    choices <- setNames(as.vector(choices), names(choices))
  }

  if (inherits(selected, "choices_labeled")) {
    selected <- setNames(as.vector(selected), names(selected))
  }

  out <- structure(
    list(choices = choices, selected = selected),
    multiple = multiple,
    keep_order = keep_order,
    fixed = fixed,
    ...,
    class = "type"
  )
}

.valid_picks <- function(x) {
  !((.is.type(x) || .is.picks(x)))
}


#' Is an object created using tidyselect
#'
#' @description
#' `choices` and `selected` can be provided using `tidyselect`, (e.g. [tidyselect::everything()]
#' [tidyselect::where()], [tidyselect::starts_with()]). These functions can't be called
#' independently but rather as an argument of function which consumes them.
#' `.is_tidyselect` safely determines if `x` can be evaluated with `tidyselect::eval_select()`
#' @param x `choices` or `selected`
#' @return `logical(1)`
#' @keywords internal
.is_tidyselect <- function(x) {
  out <- tryCatch(x, error = function(e) e)
  !is.character(out) && !is.null(out) && !inherits(out, "delayed_data")
}
