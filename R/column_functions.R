#' Returns non-key column names from data
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param data (`data.frame`) Data with attribute `filter_and_columns`. This can only be
#' created by [data_extract_srv()], which returns a shiny [shiny::reactive()].
#'
#' @return A named `character` vector with the non-key columns of the `data`.
#'
#' @references [data_extract_srv()]
#'
#' @export
#'
get_dataset_prefixed_col_names <- function(data) {
  if (!is.null(attr(data, "filter_and_columns")$columns) && attr(data, "filter_and_columns")$columns != "") {
    paste(attr(data, "dataname"), attr(data, "filter_and_columns")$columns, sep = ".")
  } else {
    NULL
  }
}
