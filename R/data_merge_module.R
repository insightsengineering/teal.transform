#' Data merge module
#'
#' @description `r lifecycle::badge("deprecated")`
#' @details This function was a convenient wrapper to combine `data_extract_multiple_srv()` and
#' `data_merge_srv()` when no additional processing is required.
#'
#' @inheritParams shiny::moduleServer
#' @param datasets (`FilteredData`)\cr
#'  object containing data, see [teal.slice::FilteredData] for more.
#' @param data_extract (named `list` of `data_extract_spec`)\cr
#' @param merge_function (`character(1)`)\cr
#'  A character string of a function that
#'  accepts the arguments `x`, `y` and `by` to perform the merging of datasets.
#' @param anl_name (`character(1)`)\cr
#'  Name of the analysis dataset.
#'
#' @return reactive expression with output from [data_merge_srv()].
#'
#' @seealso [data_merge_srv()]
#'
#' @export
data_merge_module <- function(datasets,
                              data_extract,
                              merge_function = "dplyr::full_join",
                              anl_name = "ANL",
                              id = "merge_id") {
  lifecycle::deprecate_stop("0.3.1", "data_merge_module()")
}


#' Data merge module server
#'
#' @description `r lifecycle::badge("deprecated")`
#' @details When additional processing of the `data_extract` list input was required, `data_merge_srv()` could be
#'   combined with `data_extract_multiple_srv()` or `data_extract_srv()` to influence the `selector_list` input.
#'
#' @inheritParams shiny::moduleServer
#' @param selector_list (`reactive`)\cr
#'   output from [data_extract_multiple_srv()] or a reactive named list of outputs from [data_extract_srv()].
#'   When using a reactive named list, the names must be identical to the shiny ids of the
#'   respective [data_extract_ui()].
#' @param datasets (`FilteredData`)\cr
#'  object containing data (see `teal.slice::FilteredData`).
#' @param merge_function (`character(1)` or `reactive`)\cr
#'  A character string of a function that accepts the arguments
#'  `x`, `y` and `by` to perform the merging of datasets.
#' @param anl_name (`character(1)`)\cr
#'  Name of the analysis dataset.
#'
#' @return reactive expression with output from [merge_datasets].
#'
#' @seealso [data_extract_srv()]
#'
#' @export
data_merge_srv <- function(id = "merge_id",
                           selector_list,
                           datasets,
                           merge_function = "dplyr::full_join",
                           anl_name = "ANL") {
  lifecycle::deprecate_stop("0.3.1", "data_merge_srv()")
}
