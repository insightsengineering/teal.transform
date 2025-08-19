#' Help text with available datasets input
#'
#' @description
#'
#' Creates [shiny::helpText()] with the names of available datasets for the
#' current module.
#'
#' @param data_extracts (`list`) of data extracts for single variable.
#'
#' @return `shiny.tag` defining help-text element that can be added to a UI element.
#'
#' @export
#'
datanames_input <- function(data_extracts) {
  datanames <- get_extract_datanames(data_extracts)
  helpText(
    paste0("Dataset", `if`(length(datanames) > 1, "s", ""), ":"),
    tags$code(paste(datanames, collapse = ", "))
  )
}

#' Gets names of the datasets from a list of `data_extract_spec` objects
#'
#' @description
#'
#' Fetches `dataname` slot per `data_extract_spec` from a list of
#' `data_extract_spec`.
#'
#' @param data_extracts (`data_extract_spec(1)`) object or a list (of lists)
#' of `data_extract_spec`.
#'
#' @return `character` vector with the unique `dataname` set.
#'
#' @export
#'
get_extract_datanames <- function(data_extracts) {
  data_extracts <- if (inherits(data_extracts, "data_extract_spec")) {
    list(data_extracts)
  } else {
    data_extracts
  }
  checkmate::assert_list(data_extracts)

  data_extracts <- Filter(Negate(is.null), data_extracts)
  data_extracts <- Filter(Negate(is.logical), data_extracts)
  data_extracts <- Filter(Negate(is.choices_selected), data_extracts)

  stopifnot(length(data_extracts) > 0)
  stopifnot(
    checkmate::test_list(data_extracts, types = "data_extract_spec") ||
      all(vapply(data_extracts, function(x) checkmate::test_list(x, types = "data_extract_spec"), logical(1)))
  )

  datanames <- lapply(data_extracts, function(x) {
    if (inherits(x, "data_extract_spec")) {
      x[["dataname"]]
    } else if (checkmate::test_list(x, types = "data_extract_spec")) {
      lapply(x, `[[`, "dataname")
    }
  })

  unique(unlist(datanames))
}

#' Verify uniform dataset source across data extract specification
#'
#' @description
#'
#' Checks if the input `data_extract_spec` objects all come from the same dataset.
#'
#' @param ... either `data_extract_spec` objects or lists of `data_extract_spec`
#' objects that do not contain `NULL`
#'
#' @return `TRUE` if all `data_extract_spec` objects come from the same dataset,
#' `FALSE` otherwise.
#'
#' @export
#'
is_single_dataset <- function(...) {
  data_extract_spec <- list(...)
  dataset_names <- get_extract_datanames(data_extract_spec)
  length(dataset_names) == 1
}
