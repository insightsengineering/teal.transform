#' Resolve delayed inputs by evaluating the code within the provided datasets
#'
#' `r lifecycle::badge("stable")`
#'
#' @param x (`delayed_data`, `list`) to resolve.
#' @param datasets (`FilteredData` or named `list`) to use as a reference to resolve `x`.
#' @param join_keys (`join_keys`) used to resolve `key` in [variable_choices()].
#'
#' @return Resolved object.
#'
#' @examples
#' library(shiny)
#'
#' ADSL <- teal.data::rADSL
#' isolate({
#'   data_list <- list(ADSL = reactive(ADSL))
#'
#'   # value_choices example
#'   v1 <- value_choices("ADSL", "SEX", "SEX")
#'   v1
#'   resolve_delayed(v1, data_list)
#'
#'   # variable_choices example
#'   v2 <- variable_choices("ADSL", c("BMRKR1", "BMRKR2"))
#'   v2
#'   resolve_delayed(v2, data_list)
#'
#'   # data_extract_spec example
#'   adsl_filter <- filter_spec(
#'     vars = variable_choices("ADSL", "SEX"),
#'     sep = "-",
#'     choices = value_choices("ADSL", "SEX", "SEX"),
#'     selected = "F",
#'     multiple = FALSE,
#'     label = "Choose endpoint and Censor"
#'   )
#'
#'   adsl_select <- select_spec(
#'     label = "Select variable:",
#'     choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
#'     selected = "BMRKR1",
#'     multiple = FALSE,
#'     fixed = FALSE
#'   )
#'
#'   adsl_de <- data_extract_spec(
#'     dataname = "ADSL",
#'     select = adsl_select,
#'     filter = adsl_filter
#'   )
#'
#'   resolve_delayed(adsl_filter, datasets = data_list)
#'   resolve_delayed(adsl_select, datasets = data_list)
#'   resolve_delayed(adsl_de, datasets = data_list)
#'
#'   # nested list (arm_ref_comp)
#'   arm_ref_comp <- list(
#'     ARMCD = list(
#'       ref = variable_choices("ADSL"),
#'       comp = variable_choices("ADSL")
#'     )
#'   )
#'
#'   resolve_delayed(arm_ref_comp, datasets = data_list)
#' })
#' @export
#'
resolve_delayed <- function(x, datasets, join_keys = teal.data::join_keys()) {
  checkmate::assert_class(join_keys, "join_keys")
  UseMethod("resolve_delayed", datasets)
}

#' @describeIn resolve_delayed Default values for `join_keys` parameters is extracted from `datasets`.
#' @export
resolve_delayed.FilteredData <- function(x,
                                         datasets,
                                         join_keys = datasets$get_join_keys()) {
  datasets_list <- sapply(X = datasets$datanames(), simplify = FALSE, FUN = function(x) {
    datasets$get_data(dataname = x, filtered = TRUE)
  })
  resolve(x, datasets_list, join_keys)
}

#' @describeIn resolve_delayed Generic method when `datasets` argument is a named list.
#' @export
resolve_delayed.list <- function(x, datasets, join_keys = teal.data::join_keys()) {
  checkmate::assert_list(datasets, types = c("reactive", "data.frame"), min.len = 1, names = "named")
  datasets_list <- sapply(X = datasets, simplify = FALSE, FUN = function(x) {
    if (is.reactive(x)) x() else x
  })
  resolve(x, datasets = datasets_list, join_keys = join_keys)
}
