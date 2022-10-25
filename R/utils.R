# also returns a list if only a single element
#' Split by separator
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x (`character`) Character (single)
#' @param sep (`character`) Separator
#' @export
split_by_sep <- function(x, sep) {
  stopifnot(is.atomic(x))
  if (is.character(x)) {
    strsplit(x, sep, fixed = TRUE)
  } else {
    x
  }
}

#' Extract labels from choices basing on attributes and names
#'
#' @param choices (`list` or `vector`) select choices
#' @param values optional, choices subset for which labels should be extracted, `NULL` for all choices
#'
#' @return (`character`) vector with labels
#' @keywords internal
extract_choices_labels <- function(choices, values = NULL) {
  res <- if (inherits(choices, "choices_labeled")) {
    attr(choices, "raw_labels")
  } else if (!is.null(names(choices)) && !setequal(names(choices), unlist(unname(choices)))) {
    names(choices)
  } else {
    NULL
  }

  if (!is.null(values) && !is.null(res)) {
    stopifnot(all(values %in% choices))
    res <- res[vapply(values, function(val) which(val == choices), numeric(1))]
  }

  return(res)
}