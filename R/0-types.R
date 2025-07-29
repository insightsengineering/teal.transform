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

#' @describeIn types Specify datasets.
#' @export
datasets <- function(choices = tidyselect::everything(), selected = 1) {
  out <- .selected_choices(choices = rlang::enquo(choices), selected = rlang::enquo(selected))
  class(out) <- c("datasets", class(out))
  out
}

#' @describeIn types Specify variables.
#' @export
variables <- function(choices = tidyselect::everything(), selected = 1) {
  out <- .selected_choices(choices = rlang::enquo(choices), selected = rlang::enquo(selected))
  class(out) <- c("variables", class(out))
  out
}

#' @describeIn types Specify colData.
#' @export
mae_colData <- function(choices = tidyselect::everything(), selected = 1) {
  out <- .selected_choices(choices = rlang::enquo(choices), selected = rlang::enquo(selected))
  class(out) <- c("colData", class(out))
  out
}

#' @describeIn types Specify values.
#' @export
values <- function(choices = tidyselect::everything(), selected = 1) {
  out <- .selected_choices(choices = rlang::enquo(choices), selected = rlang::enquo(selected))
  class(out) <- c("values", class(out))
  out
}

#' @export
anyNA.type <- function(x, recursive = FALSE) {
  anyNA(unclass(x[c("choices", "selected")]), recursive = recursive)
}

#' @export
c.specification <- function(...) {
  l <- list(...)
  types <- lapply(l, names)
  typesc <- vapply(l, .is.specification, logical(1L))
  if (!all(typesc)) {
    stop("An object in position ", which(!typesc), " is not a specification.")
  }
  utypes <- unique(unlist(types, FALSE, FALSE))
  vector <- vector("list", length(utypes))
  names(vector) <- utypes
  for (t in utypes) {
    new_type <- vector("list", length = 2)
    names(new_type) <- c("choices", "selected")
    class(new_type) <- c("type", "list")
    for (i in seq_along(l)) {
      if (!t %in% names(l[[i]])) {
        next
      }
      # Slower but less code duplication:
      # new_type <- c(new_type, l[[i]][[t]])
      # then we need class(new_type) <- c(t, "type", "list") outside the loop
      old_choices <- new_type$choices
      old_selected <- new_type$selected
      new_type$choices <- c(old_choices, l[[i]][[t]][["choices"]])
      attr(new_type$choices, "original") <- c(orig(
        old_choices
      ), orig(l[[i]][[t]][["names"]]))
      new_type$selected <- c(old_selected, l[[i]][[t]][["selected"]])
      attr(new_type$selected, "original") <- c(orig(old_selected), orig(l[[i]][[t]][["selected"]]))
      attr(new_type, "delayed") <- any(attr(new_type, "delayed"), attr(l[[i]], "delayed"))
    }
    orig_choices <- unique(orig(new_type$choices))
    new_type$choices <- unique(new_type$choices)
    attr(new_type$choices, "original") <- orig_choices

    orig_selected <- unique(orig(new_type$selected))
    new_type$selected <- unique(new_type$selected)
    attr(new_type$selected, "original") <- orig_selected
    class(new_type) <- c(t, "type", "list")
    vector[[t]] <- new_type
  }
  class(vector) <- c("specification", "list")
  vector
}

#' @export
c.type <- function(...) {
  l <- list(...)
  types <- lapply(l, is)
  typesc <- vapply(l, .is.type, logical(1L))
  if (!all(typesc)) {
    stop("An object in position ", which(!typesc), " is not a type.")
  }
  utypes <- unique(unlist(types, FALSE, FALSE))
  vector <- vector("list", length(utypes))
  names(vector) <- utypes
  for (t in utypes) {
    new_type <- vector("list", length = 2)
    names(new_type) <- c("choices", "selected")
    for (i in seq_along(l)) {
      if (!is(l[[i]], t)) {
        next
      }
      old_choices <- new_type$choices
      old_selected <- new_type$selected
      new_type$choices <- c(old_choices, l[[i]][["choices"]])
      attr(new_type$choices, "original") <- c(orig(
        old_choices
      ), orig(l[[i]][["choices"]]))
      new_type$selected <- unique(c(old_selected, l[[i]][["selected"]]))
      attr(new_type$selected, "original") <- c(orig(old_selected), orig(l[[i]][["selected"]]))
    }
    orig_choices <- unique(orig(new_type$choices))
    orig_selected <- unique(orig(new_type$selected))

    new_type$choices <- unique(new_type$choices)
    if (length(new_type$choices) == 1) {
      new_type$choices <- new_type$choices[[1]]
    }
    attr(new_type$choices, "original") <- orig_choices

    if (length(new_type$selected) == 1) {
      new_type$selected <- new_type$selected[[1]]
    }
    attr(new_type$selected, "original") <- orig_selected

    class(new_type) <- c(t, "type", "list")
    attr(new_type, "delayed") <- is.delayed(new_type)
    vector[[t]] <- new_type
  }
  if (length(vector) == 1) {
    return(vector[[1]])
  }
  class(vector) <- c("specification", "list")
  vector
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

.selected_choices <- function(choices, selected, keep_order = FALSE, fixed = FALSE) {
  out <- structure(
    list(choices = choices, selected = selected),
    keep_order = keep_order,
    fixed = fixed,
    class = "type"
  )
  as.delayed(out)
}

.simplity_c <- function(x) {
  unique(unlist(x, FALSE, FALSE))
}

.valid_specification <- function(x) {
  !((.is.type(x) || .is.specification(x)))
}
