#' `tidyselect` helpers
#'
#'
#' @examples
#' # select keys (primary and foreign)
#' variables(choices = is_key())
#'
#' # select factor column but exclude foreign keys
#' variables(choices = where(~ is.factor(.x) & !is_foreign_key()))
#' @name tidyselectors

# developer notes:
#  in determine join_keys are handed over and in determine.variables attributes are assigned to
#  the data columns. It is internally controlled process and it is designed like this because:
#   - tidyselect functions don't accept arguments from outside so we can't add join_keys of selected dataset
#     during eval_select.
#   - having predicates to be utilized by `tidyselect::where` is `tidyselect` compatible and more predictable

#' @rdname tidyselectors
#' @param min.len (`integer(1)`) minimal number of unique values
#' @param max.len (`integer(1)`) maximal number of unique values
#' @export
is_categorical <- function(max.len, min.len) {
  # todo: consider making a function which can exit earlier when max.len > length(unique(x)) < min.len
  #       without a need to compute unique on the whole vector.
  if (missing(max.len) && missing(min.len)) {
    function(x) is.factor(x) || is.character(x)
  } else if (!missing(max.len) && missing(min.len)) {
    checkmate::assert_int(max.len, lower = 0)
    function(x) (is.factor(x) || is.character(x)) && length(unique(x)) <= max.len
  } else if (!missing(min.len) && missing(max.len)) {
    checkmate::assert_int(min.len, lower = 0)
    function(x) (is.factor(x) || is.character(x)) && length(unique(x)) >= min.len
  } else {
    checkmate::assert_int(min.len, lower = 0)
    checkmate::assert_int(max.len, lower = 0)
    checkmate::assert_true(max.len >= min.len)
    function(x) {
      (is.factor(x) || is.character(x)) && {
        n <- length(unique(x))
        n >= min.len && n <= max.len
      }
    }
  }
}

#' @rdname tidyselectors
#' @export
no_more_choices_than <- function(max.len) {
  # todo: consider making a function which can exit earlier when max.len > length(unique(x)) < min.len
  #       without a need to compute unique on the whole vector.
  function(x) length(unique(x)) <= max.len
}
