# Contains modules to check the input provided to the `tm_*` functions is correct.
# In general, they are checking functions, in the sense that they call `stopifnot`
# if the conditions are not met.

#' Make sure that the extract spec has list form
#'
#' @md
#' @description `r lifecycle::badge("stable")`
#'
#' @param x `data_extract_spec` a single `data_extract_spec` or list of these
#' @param allow_null `logical` whether x can be `NULL`
#'
#' @return x as a list if it is not already
#'
#' @export
list_extract_spec <- function(x, allow_null = FALSE) {
  if (is.null(x)) {
    stopifnot(allow_null)
    return(NULL)
  }
  if (!checkmate::test_list(x, types = "data_extract_spec")) {
    x <- list(x)
  }
  stopifnot(checkmate::test_list(x, types = "data_extract_spec"))
  x
}

#' Checks that the extract_input specification does not allow multiple
#' selection
#'
#' @md
#' @description `r lifecycle::badge("stable")`
#'
#' @param extract_input `data_extract_spec` a list of `data_extract_spec` or NULL
#'
#' Stops if condition not met
#'
#' @export
check_no_multiple_selection <- function(extract_input) {
  # bug in is_class_list when NULL
  checkmate::assert_list(extract_input, types = "data_extract_spec", null.ok = TRUE)
  all(vapply(extract_input, function(elem) !isTRUE(elem$select$multiple), logical(1))) ||
    stop("extract_input variable should not allow multiple selection")
  invisible(NULL)
}
