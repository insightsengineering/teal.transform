#' Delayed datasets
#'
#' Generate `delayed_data_extract_spec` without prior knowledge of the data.
#'
#' `delayed_datanames` is a character string with class `delayed_datanames`
#' and attribute `datasets`, which is set to `x`. The attribute specifies
#' a wishlist of datasets for which `delayed_data_extract_spec`s are to be created,
#' maintaining the same specification for `select`, `filter`, and `reshape`.
#'
#' `delayed_data_extract_spec` that have `delayed_datanames` for `dataname` are resolved internally.
#'
#' It is forbidden to use different `delayed_datanames` within one `delayed_data_extract_spec`
#' as well as to mix `delayed_datanames` with specific dataset specification within one `delayed_data_extract_spec`.
#' This is enforced when creating `data_extract_spec`s.
#'
#' @inheritSection resolve_delayed_datanames Resolution
#'
#' @param x (`character`) set of dataset names for wchich `delayed_data_extract_spec`s will be created;
#'                        set to `"all"` to use all available datasets
#'
#' @return Character string with `class` `delayed_datanames` and attribute `datasets`.
#'
#' @examples
#' # resolve into delayed_data_extract_specs for all available datasets
#' data_extract_spec(
#'   dataname = delayed_datanames()
#' )
#'
#' # resolve into delayed_data_extract_specs for available datasets from among ADSL and ADAE
#' data_extract_spec(
#'   dataname = delayed_datanames(c("ADSL", "ADAE"))
#' )
#'
#' # use the same delayed_datanames() in child elements of a des
#' data_extract_spec(
#'   dataname = delayed_datanames(),
#'   select = select_spec(
#'     choices = variable_choices(
#'       data = delayed_datanames(),
#'       subset = function(data) names(Filter(is.numeric, data))
#'     ),
#'     selected = last_choice()
#'   )
#' )
#'
#' @export
#'
delayed_datanames <- function(x = "all") {
  structure(
    "delayed_datanames",
    class = c("delayed_datanames", "delayed_data", "character"),
    datasets = x
  )
}
