#' Resolve `delayed_datasets`
#'
#' Convert `delayed_data_extract_spec`s containing `delayed_datasets` into normal ones.
#'
#' This function is used internally.
#'
#' @param des (`data_extract_spec` or `list` thereof) see `Details`
#' @param datasets (`character`) vector of dataset for which to resolve
#'
#' @section Resolution:
#' `delayed_data_extract_specs` are resolved as follows:
#' - `data_extract_specs` are returned as is
#' - `delayed_data_extract_specs` where `dataname` is `character` are returned as is
#' - `delayed_data_extract_specs` where `dataname` is `delayed_datasets` is first confronted
#'   with names of datasets in the app and has its `datasets` attribute updated,
#'   and then is converted to a list of `delayed_data_extract_spec`s of the same length as
#'   the updated `datasets` attribute.
#'
#' @return List of `delayed_data_extract_spec`s.
#'
#' @keywords internal
resolve_delayed_datasets <- function(des, datasets) {
  # When used on a ddes with delayed_dataset that is in a list
  # .unfold_delayed_datasets creates a list(list(ddes, ddes)) structure
  # where list(ddes, ddes) is expected. One list level has to be collapsed.
  .integrate <- function(x) {
    if (inherits(x, "data_extract_spec")) {
      x
    } else if (checkmate::test_list(x, "list", len = 1L) && checkmate::test_list(x[[1L]], "data_extract_spec")) {
      x[[1L]]
    } else {
      lapply(x, .integrate)
    }
  }

  .unfold_delayed_datasets(des, datasets) |>
    .resolve_delayed_datasets() |>
    .integrate()
}

#' @keywords internal
#' @noRd
.unfold_delayed_datasets <- function(des, datasets) {
  .horse <- function(des, datasets) {
    delayed <- attr(des, "datasets", exact = TRUE)
    delayed <-
      if (identical(delayed, "all")) {
        datasets
      } else {
        intersect(delayed, datasets)
      }
    attr(des, "datasets") <- delayed
    des
  }

  rapply(des, .horse, "delayed_datasets", how = "replace", datasets = datasets)
}

#' @keywords internal
#' @noRd
.resolve_delayed_datasets <- function(des) {
  .horse <- function(des) {
    if (!inherits(des$dataname, "delayed_datasets")) {
      des
    } else {
      lapply(attr(des$dataname, "datasets", exact = TRUE), function(dataset) {
        rapply(des, f = function(...) dataset, "delayed_datasets", how = "replace")
      })
    }
  }

  if (inherits(des, "data_extract_spec")) {
    .horse(des)
  } else {
    lapply(des, .resolve_delayed_datasets)
  }
}

#' Assert delayed_datasets are used properly:
#' - no mixing with specific dataset specification
#' - no mixing different delayed_datasets
#' @keywords internal
#' @noRd
assert_delayed_datesets <- function(x) {
  checkmate::assert_class(x, "data_extract_spec")
  if (inherits(x, "delayed_data_extract_spec")) {
    # STEP 1: check that all places that could be delayed_datasets are actually datasets
    error_msg <- paste0(deparse1(sys.call(-1)), ": delayed_datasets must not be mixed with specific datanames")
    .extract <- function(x) {
      if (is.null(x) || is.logical(x) || is.function(x) || is.character(x)) {
        NULL
      } else if (is.list(x) && is.character(x[["data"]]) && !inherits(x[["data"]], "delayed_datasets")) {
        x[["data"]]
      } else if (is.list(x) && is.character(x[["dataname"]]) && !inherits(x[["dataname"]], "delayed_datasets")) {
        x[["dataname"]]
      } else {
        lapply(x, .extract)
      }
    }
    datanames <- unlist(.extract(x))
    delayed <- vapply(datanames, inherits, logical(1L), what = "delayed_datasets")
    if (!(all(delayed) || all(!delayed))) stop(error_msg, call. = FALSE)

    # STEP 2: check that all delayed_datasets in this ddes are the same
    master <- x$dataname
    if (inherits(master, "delayed_datasets")) {
      error_msg <- paste0(deparse1(sys.call(-1)), ": delayed_datasets used must be identical")
      slaves <- rapply(x, function(xx) xx, "delayed_datasets", how = "unlist")
      .extract_datasets <- function(xx) paste(sort(attr(xx, "datasets")), collapse = "--")
      slaves_datasets <- rapply(x, .extract_datasets, "delayed_datasets", how = "unlist")
      Reduce(
        function(x1, x2) {
          if (identical(x1, x2)) x2 else stop(error_msg, call. = FALSE)
        },
        slaves,
        init = as.vector(master)
      )
      Reduce(
        function(x1, x2) {
          if (identical(x1, x2)) x2 else stop(error_msg, call. = FALSE)
        },
        slaves_datasets,
        init = .extract_datasets(master)
      )
    } else {
      error_msg <- paste0(deparse1(sys.call(-1)), ": delayed_datasets must not be mixed with specific datanames")
      rapply(x, function(...) stop(error_msg, call. = FALSE), "delayed_datasets")
    }
  }
  x
}
