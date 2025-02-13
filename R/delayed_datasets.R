#' Delayed datasets
#'
#' Generate `delayed_data_extract_spec` without prior knowledge of the data.
#'
#' `delayed_datasets` is a character string with class `delayed_datasets`
#' and attribute `datasets`, which is set to `x`. The attribute specifies
#' a wishlist of datasets for which `delayed_data_extract_spec`s are to be created,
#' maintaining the same specification for `select`, `filter`, and `reshape`.
#'
#' `delayed_data_extract_spec` that have `delayed_datasets` for `dataname` are resolved internally.
#'
#' It is forbidden to use different `delayed_datasets` within one `delayed_data_extract_spec`
#' as well as to mix `delayed_datasets` with specific dataset specification within one `delayed_data_extract_spec`.
#' This is enforced when creating `data_extract_spec`s.
#'
#' @inheritSection resolve_delayed_datasets Resolution
#'
#' @param x (`character`) set of dataset names for wchich `delayed_data_extract_spec`s will be created;
#'                        set to `"all"` to use all available datasets
#'
#' @return Character string with `class` `delayed_datasets` and attribute `datasets`.
#'
#' @examples
#' # resolve into delayed_data_extract_specs for all available datasets
#' data_extract_spec(
#'   dataname = delayed_datasets()
#' )
#'
#' # resolve into delayed_data_extract_specs for available datasets from among ADSL and ADAE
#' data_extract_spec(
#'   dataname = delayed_datasets(c("ADSL", "ADAE"))
#' )
#'
#' # use the same delayed_datasets() in child elements of a des
#' data_extract_spec(
#'   dataname = delayed_datasets(),
#'   select = select_spec(
#'     choices = variable_choices(
#'       data = delayed_datasets(),
#'       subset = function(data) names(Filter(is.numeric, data))
#'     ),
#'     selected = last_choice()
#'   )
#' )
#'
#' @export
#'
delayed_datasets <- function(x = "all") {
  structure(
    "delayed_datasets",
    class = c("delayed_datasets", "delayed_data", "character"),
    datasets = x
  )
}
