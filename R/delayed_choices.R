#' Bare constructor for `delayed_choices` object
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Special S3 structures that delay selection of possible choices in a
#' `filter_spec`, `select_spec` or `choices_selected` object.
#'
#' @return
#' Object of class `delayed_choices`, which is a function that returns
#' the appropriate subset of its argument. The `all_choices` structure
#' also has an additional class for internal use.
#'
#' @examples
#' # These pairs of structures represent semantically identical specifications:
#' choices_selected(choices = letters, selected = letters)
#' choices_selected(choices = letters, selected = all_choices())
#'
#' choices_selected(choices = letters, selected = letters[1])
#' choices_selected(choices = letters, selected = first_choice())
#'
#' filter_spec(
#'   vars = c("selected_variable"),
#'   choices = c("value1", "value2", "value3"),
#'   selected = "value3"
#' )
#' filter_spec(
#'   vars = c("selected_variable"),
#'   choices = c("value1", "value2", "value3"),
#'   selected = last_choice()
#' )
#'
#' @name delayed_choices

#' @export
#' @rdname delayed_choices
all_choices <- function() {
  structure(
    function(x) {
      x
    },
    class = c("all_choices", "delayed_choices")
  )
}

#' @export
#' @rdname delayed_choices
first_choice <- function() {
  structure(
    function(x) {
      if (length(x) == 0L) {
        x
      } else if (is.atomic(x)) {
        x[1L]
      } else if (inherits(x, "delayed_data")) {
        x$subset <- function(data) {
          modifier <- function(x) x[1L]
          Reduce(function(f, ...) f(...), c(x$subset, modifier), init = data, right = TRUE)
          x
        }
      }
    },
    class = c("delayed_choices")
  )
}

#' @export
#' @rdname delayed_choices
last_choice <- function() {
  structure(
    function(x) {
      if (length(x) == 0L) {
        x
      } else if (is.atomic(x)) {
        x[length(x)]
      } else if (inherits(x, "delayed_data")) {
        x$subset <- function(data) {
          modifier <- function(x) x[length(x)]
          Reduce(function(f, ...) f(...), c(x$subset, modifier), init = data, right = TRUE)
          x
        }
      }
    },
    class = c("delayed_choices")
  )
}
