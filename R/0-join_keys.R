#' @importFrom dplyr rename
#' @export
rename.join_keys <- function(.data, dataname, ...) {
  checkmate::assert_string(dataname)
  dots <- list(...)
  checkmate::assert_list(dots, types = "character", names = "named")
  column <- unlist(dots)
  for (other_name in names(.data[[dataname]])) {
    keys <- .data[dataname, other_name]
    matched_idx <- match(column, names(keys))
    names(keys)[matched_idx] <- names(column)
    if (other_name == dataname) {
      keys[matched_idx] <- names(column)
    }
    .data[dataname, other_name] <- keys
  }
  .data
}
