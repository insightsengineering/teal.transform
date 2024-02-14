#' Data extract input for `teal` modules
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' The Data extract input can be used to filter and select columns from a data set.
#' This function enables such an input in `teal`.
#' Please use the constructor function [data_extract_spec] to set it up.
#'
#' @note No checks based on columns can be done because the data is only referred to by name.
#'
#' @rdname data_extract_spec
#'
#' @section Module Development:
#' `teal.transform` uses this object to construct a UI element in a module.
#'
#' @param dataname (`character`)
#' The name of the dataset to be extracted.
#' @param select (`NULL` or `select_spec`-S3 class or `delayed_select_spec`)
#' Columns to be selected from the input dataset mentioned in `dataname`.
#' The setup can be created using [select_spec] function.
#' @param filter (`NULL` or `filter_spec` or its respective delayed version)
#' Setup of the filtering of key columns inside the dataset.
#' This setup can be created using the [filter_spec] function.
#' Please note that if both select and filter are set to `NULL`, then the result
#' will be a filter spec UI with all variables as possible choices and a select
#' spec with multiple set to `TRUE`.
#' @param reshape (`logical`)
#' whether reshape long to wide.
#' Note that it will be used only in case of long dataset with multiple
#' keys selected in filter part.
#'
#' @return `data_extract_spec` object.
#'
#' @references [select_spec] [filter_spec]
#'
#' @examples
#' adtte_filters <- filter_spec(
#'   vars = c("PARAMCD", "CNSR"),
#'   sep = "-",
#'   choices = c("OS-1" = "OS-1", "OS-0" = "OS-0", "PFS-1" = "PFS-1"),
#'   selected = "OS-1",
#'   multiple = FALSE,
#'   label = "Choose endpoint and Censor"
#' )
#'
#' data_extract_spec(
#'   dataname = "ADTTE",
#'   filter = adtte_filters,
#'   select = select_spec(
#'     choices = c("AVAL", "BMRKR1", "AGE"),
#'     selected = c("AVAL", "BMRKR1"),
#'     multiple = TRUE,
#'     fixed = FALSE,
#'     label = "Column"
#'   )
#' )
#'
#' data_extract_spec(
#'   dataname = "ADSL",
#'   filter = NULL,
#'   select = select_spec(
#'     choices = c("AGE", "SEX", "USUBJID"),
#'     selected = c("SEX"),
#'     multiple = FALSE,
#'     fixed = FALSE
#'   )
#' )
#' data_extract_spec(
#'   dataname = "ADSL",
#'   filter = filter_spec(
#'     vars = variable_choices("ADSL", subset = c("AGE"))
#'   )
#' )
#'
#' dynamic_filter <- filter_spec(
#'   vars = choices_selected(variable_choices("ADSL"), "COUNTRY"),
#'   multiple = TRUE
#' )
#' data_extract_spec(
#'   dataname = "ADSL",
#'   filter = dynamic_filter
#' )
#'
#' @export
#'
data_extract_spec <- function(dataname, select = NULL, filter = NULL, reshape = FALSE) {
  checkmate::assert_string(dataname)
  stopifnot(
    is.null(select) ||
      (inherits(select, "select_spec") && length(select) >= 1)
  )
  checkmate::assert(
    checkmate::check_null(filter),
    checkmate::check_class(filter, "filter_spec"),
    checkmate::check_list(filter, "filter_spec")
  )
  checkmate::assert_flag(reshape)

  if (is.null(select) && is.null(filter)) {
    select <- select_spec(
      choices = variable_choices(dataname),
      multiple = TRUE
    )
    filter <- filter_spec(
      vars = choices_selected(variable_choices(dataname)),
      selected = all_choices()
    )
  }

  if (inherits(filter, "filter_spec")) filter <- list(filter)

  for (idx in seq_along(filter)) filter[[idx]]$dataname <- dataname

  if (
    inherits(select, "delayed_select_spec") ||
      any(vapply(filter, inherits, logical(1), "delayed_filter_spec"))
  ) {
    structure(
      list(dataname = dataname, select = select, filter = filter, reshape = reshape),
      class = c("delayed_data_extract_spec", "delayed_data", "data_extract_spec")
    )
  } else {
    structure(
      list(dataname = dataname, select = select, filter = filter, reshape = reshape),
      class = "data_extract_spec"
    )
  }
}
