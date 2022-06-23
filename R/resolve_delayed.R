#' Resolve delayed inputs by evaluating the code within the provided datasets
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x Object of class `delayed_data` to resolve.
#' @param datasets Object of class `FilteredData` to use for evaluation.
#'
#' @return Resolved object.
#'
#' @export
#'
#' @examples
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' shiny::isolate({
#'   ds <- teal.slice::init_filtered_data(
#'     list(ADSL = list(
#'       dataset = ADSL, keys = teal.data::get_cdisc_keys("ADSL"), parent = character(0)
#'     )),
#'     cdisc = TRUE
#'   )
#'
#'   # value_choices example
#'   v1 <- value_choices("ADSL", "SEX", "SEX")
#'   v1
#'   resolve_delayed(v1, ds)
#'
#'   # variable_choices example
#'   v2 <- variable_choices("ADSL", c("BMRKR1", "BMRKR2"))
#'   v2
#'   resolve_delayed(v2, ds)
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
#'   resolve_delayed(adsl_filter, ds)
#'   resolve_delayed(adsl_select, ds)
#'   resolve_delayed(adsl_de, ds)
#'
#'   # nested list (arm_ref_comp)
#'   arm_ref_comp <- list(
#'     ARMCD = list(
#'       ref = variable_choices("ADSL"),
#'       comp = variable_choices("ADSL")
#'     )
#'   )
#'
#'   resolve_delayed(arm_ref_comp, ds)
#' })
resolve_delayed <- function(x, datasets) {
  stopifnot(inherits(datasets, "FilteredData"))
  datasets_list <- sapply(X = datasets$datanames(), simplify = FALSE, FUN = function(x) {
    reactive(datasets$get_data(dataname = x, filtered = TRUE))
  })
  key_list <- sapply(X = datasets$datanames(), simplify = FALSE, FUN = function(x) {
    datasets$get_keys(dataname = x)
  })
  resolve(x, datasets_list, key_list)
}
