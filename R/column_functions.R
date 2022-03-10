#' Returns non-key column names from data
#'
#' @description `r lifecycle::badge("stable")`
#' @export
#'
#' @param data (\code{data.frame}) Data with attribute \code{filter_and_columns}. This can only be
#'  created by \code{\link{data_extract_srv}}. which returns a shiny \code{\link[shiny]{reactive}}.
#'
#' @return A named character vector with the non-key columns of the \code{data}..
#'
#' @references \link{data_extract_srv}
get_dataset_prefixed_col_names <- function(data) {
  if (!is.null(attr(data, "filter_and_columns")$columns) && attr(data, "filter_and_columns")$columns != "") {
    paste(attr(data, "dataname"), attr(data, "filter_and_columns")$columns, sep = ".")
  } else {
    NULL
  }
}
