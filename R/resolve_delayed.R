#' Resolve delayed inputs by evaluating the code within the provided datasets
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x (`delayed_data`, `list`) to resolve.
#' @param datasets (`FilteredData` or named `list`) to use as a reference to resolve `x`.
#' @param keys (named `list`) with primary keys for each dataset from `datasets`. `names(keys)`
#'   should match `names(datasets)`
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
resolve_delayed <- function(x, datasets, keys) {
  UseMethod("resolve_delayed", datasets)
}


#' @export
resolve_delayed.FilteredData <- function(x,
                                         datasets,
                                         keys = sapply(datasets$datanames(), datasets$get_keys, simplify = FALSE)) {
  datasets_list <- sapply(X = datasets$datanames(), simplify = FALSE, FUN = function(x) {
    reactive(datasets$get_data(dataname = x, filtered = TRUE))
  })
  resolve(x, datasets_list, keys)
}

#' @export
resolve_delayed.list <- function(x, datasets, keys = NULL) {
  checkmate::assert_list(datasets, type = c("reactive", "data.frame"), min.len = 1, names = "named")
  checkmate::assert_list(keys, "character", names = "named", null.ok = TRUE)
  checkmate::assert(
    .var.name = "keys",
    checkmate::check_names(names(keys), subset.of = names(datasets)),
    checkmate::check_null(keys)
  )
  # convert to list of reactives
  datasets_list <- sapply(X = datasets, simplify = FALSE, FUN = function(x) {
    if (is.reactive(x)) x else reactive(x)
  })
  resolve(x, datasets_list, keys)
}
