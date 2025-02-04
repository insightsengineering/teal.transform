#' Bare constructor for `delayed_choices` object
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Special S3 structures that delay selection of possible choices in a
#' `filter_spec`, `select_spec` or `choices_selected` object.
#'
#' @param n (`integer`-like) number of first/last items to subset to
#'
#' @return
#' Object of class `delayed_data, delayed_choices`, which is a function
#' that returns the appropriate subset of its argument.
#' `all_choices`, `first_choices`, and `last_choices` structures
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
#' choices_selected(choices = letters, selected = letters[length(letters)])
#' choices_selected(choices = letters, selected = last_choice())
#'
#' choices_selected(choices = letters, selected = head(letters, 4))
#' choices_selected(choices = letters, selected = first_choices(4))
#'
#' choices_selected(choices = letters, selected = tail(letters, 4))
#' choices_selected(choices = letters, selected = last_choices(4))
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
    class = c("multiple_choices", "delayed_choices", "delayed_data")
  )
}

#' @export
#' @rdname delayed_choices
first_choice <- function() {
  structure(
    function(x) {
      if (inherits(x, "delayed_choices")) {
        x
      } else if (length(x) == 0L) {
        x
      } else if (is.atomic(x)) {
        x[1L]
      } else if (inherits(x, "delayed_data")) {
        if (is.null(x$subset)) return(x)
        original_fun <- x$subset
        added_fun <- function(x) x[1L]
        x$subset <- function(data) {
          added_fun(original_fun(data))
        }
        x
      }
    },
    class = c("delayed_choices", "delayed_data")
  )
}

#' @export
#' @rdname delayed_choices
last_choice <- function() {
  structure(
    function(x) {
      if (inherits(x, "delayed_choices")) {
        x
      } else if (length(x) == 0L) {
        x
      } else if (is.atomic(x)) {
        x[length(x)]
      } else if (inherits(x, "delayed_data")) {
        if (is.null(x$subset)) return(x)
        original_fun <- x$subset
        added_fun <- function(x) x[length(x)]
        x$subset <- function(data) {
          added_fun(original_fun(data))
        }
        x
      }
    },
    class = c("delayed_choices", "delayed_data")
  )
}

#' @export
#' @rdname delayed_choices
first_choices <- function(n) {
  checkmate::assert_count(n, positive = TRUE)
  structure(
    function(x) {
      if (inherits(x, "delayed_choices")) {
        x
      } else if (length(x) == 0L) {
        x
      } else if (is.atomic(x)) {
        utils::head(x, n = n)
      } else if (inherits(x, "delayed_data")) {
        if (is.null(x$subset)) return(x)
        original_fun <- x$subset
        added_fun <- function(x) utils::head(x, n = n)
        x$subset <- function(data) {
          added_fun(original_fun(data))
        }
        x
      }
    },
    class = c("multiple_choices", "delayed_choices", "delayed_data")
  )
}

#' @export
#' @rdname delayed_choices
last_choices <- function(n) {
  checkmate::assert_count(n, positive = TRUE)
  structure(
    function(x) {
      if (inherits(x, "delayed_choices")) {
        x
      } else if (length(x) == 0L) {
        x
      } else if (is.atomic(x)) {
        utils::tail(x, n = n)
      } else if (inherits(x, "delayed_data")) {
        if (is.null(x$subset)) return(x)
        original_fun <- x$subset
        added_fun <- function(x) utils::tail(x, n = n)
        x$subset <- function(data) {
          added_fun(original_fun(data))
        }
        x
      }
    },
    class = c("multiple_choices", "delayed_choices", "delayed_data")
  )
}
