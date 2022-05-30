#' Available datasets input
#'
#' @description `r lifecycle::badge("stable")`
#' Creates \link[shiny]{helpText} with the names of datasets
#' available for current module.
#' @param data_extracts list of data extracts for single variable
#' @export
datanames_input <- function(data_extracts) {
  datanames <- get_extract_datanames(data_extracts)
  helpText(
    paste0("Dataset", `if`(length(datanames) > 1, "s", ""), ":"),
    tags$code(paste(datanames, collapse = ", "))
  )
}

#' Gets names of the datasets from a list of `data_extract_spec` objects
#'
#' @description `r lifecycle::badge("stable")`
#' Fetches dataname slot per \code{data_extract_spec} from a list of \code{data_extract_spec}
#' and returns the unique dataname set.
#'
#' @param data_extracts A single \code{data_extract_spec} object or a list (of lists) of \code{data_extract_spec}
#'
#' @export
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
      all(
        vapply(data_extracts, function(x) checkmate::test_list(x, types = "data_extract_spec"), logical(1))
      )
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

#' Checks if the input `data_extract_spec` objects all come from the same dataset
#'
#' @description `r lifecycle::badge("stable")`
#' @param ... either \code{data_extract_spec} objects or lists of \code{data_extract_spec} objects that do not contain
#'   NULL
#' @return logical
#' @export
is_single_dataset <- function(...) {
  data_extract_spec <- list(...)
  dataset_names <- get_extract_datanames(data_extract_spec)
  return(length(dataset_names) == 1)
}