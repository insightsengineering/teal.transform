#' Bare constructor for `delayed_choices` object
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' An S3 structure that delays selection of possible choices in a
#' `filter_spec`, `select_spec` or `choices_selected` object.
#'
#' @param which `character(1)` string speficying which choices to select
#' @return
#' Object of class `delayed_choices`, which is a function that returns the appropriate subset of its argument.
#' `delayed_choices` also have additional classes, which depend on `which`, for internal use.
#'
#' @examples
#' # Both structures are semantically identical
#' filter_spec(
#'   vars = c("selected_variable"),
#'   choices = c("value1", "value2"),
#'   selected = c("value1", "value2")
#' )
#'
#' filter_spec(
#'   vars = c("selected_variable"),
#'   choices = c("value1", "value2"),
#'   selected = delayed_choices()
#' )
#'
#' choices_selected(choices = letters, selected = letters)
#' choices_selected(choices = letters, selected = delayed_choices())
#' @export
#'
delayed_choices <- function(which = c("all", "first", "last")) {
  which <- match.arg(which)
  ans <-
    switch(which,
      "all" = function(x) x,
      "first" = function(x) {
        if (length(x) == 0L) {
          return(x)
        }
        if (is.atomic(x)) {
          return(x[1L])
        }
        x$subset <- function(data) {
          modifier <- function(x) x[1L]
          Reduce(function(f, ...) f(...), c(x$subset, modifier), init = data, right = TRUE)
        }
        x
      },
      "last" = function(x) {
        if (length(x) == 0L) {
          return(x)
        }
        if (is.atomic(x)) {
          return(x[length(x)])
        }
        x$subset <- function(data) {
          modifier <- function(x) x[length(x)]
          Reduce(function(f, ...) f(...), c(x$subset, modifier), init = data, right = TRUE)
        }
        x
      }
    )
  extra_class <- switch(which,
    "all" = "all_choices",
    "first" = "first_choice",
    "last" = "last_choice"
  )
  structure(ans, class = c(extra_class, "delayed_choices"))
}
