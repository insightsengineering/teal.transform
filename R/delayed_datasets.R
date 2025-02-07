#' Delayed datasets
#'
#' Generate `delayed_data_extract_spec` without prior knowledge of the data.
#'
#' documentation WIP
#'
#' `delayed_datasets` is a character string with class `delayed_datasets`
#' and attribute `datasets` which is set to `x`.
#' The attribute specifies a wishlist of datasets for which `delayed_des` are to be created.
#'
#' `delayed_data_extract_specs` are resolved as follows:
#' - `data_extract_specs` are returned as is
#' - `delayed_data_extract_specs` where `dataname` is `character` are returned as is
#' - `delayed_data_extract_specs` where `dataname` is `delayed_datasets` is first confronted
#'   with names of datasets in the app and has its `datasets` attribute updated,
#'   and then is converted to a list of `delayed_data_extract_spec`s of the same length as
#'   the updated `datasets` attribute.
#'
#' @param x (`character`) set of dataset names for wchich `delayed_data_extract_spec`s will be created;
#'                        set to `"all"` to use all available datasets
#' @param des (`data_extract_spec` or `list` thereof) see `Details`
#' @param datasets (`character`) vector of dataset for which to resolve

#' @name delayed_datasets
NULL

#' @rdname delayed_datasets
#' @export
delayed_datasets <- function(x = "all") {
  structure(
    "delayed_datasets",
    class = c("delayed_datasets", "delayed_data", "character"),
    datasets = x
  )
}

#' @rdname delayed_datasets
#' @export
resolve_delayed_datasets <- function(des, datasets) {
  .integrate <- function(x) {
    if (inherits(x, "delayed_data_extract_spec")) return(x)
    if (checkmate::test_list(x, "list", len = 1L) &&
          checkmate::test_list(x[[1L]], "delayed_data_extract_spec")) {
      return(x[[1L]])
    }
    lapply(x, .integrate)
  }

  .resolve_delayed_datasets(.update_delayed_datasets(des, datasets)) |> .integrate()
}

#' @keywords internal
#' @noRd
.update_delayed_datasets <- function(des, datasets) {
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
    if (!inherits(des$dataname, "delayed_datasets")) return(des)
    lapply(attr(des$dataname, "datasets", exact = TRUE), function(dataset) {
      rapply(des, f = function(...) dataset, "delayed_datasets", how = "replace")
    })
  }

  if (inherits(des, "delayed_data_extract_spec")) return(.horse(des))
  lapply(des, .resolve_delayed_datasets)
}

#' ensure that all delayed_datasets in a delayed_des are the same
assert_delayed_datesets_identical <- function(x) {
  checkmate::assert_class(x, "data_extract_spec")
  if (inherits(x, "delayed_data_extract_spec")) {
    master <- x$dataname
    if (inherits(master, "delayed_datasets")) {
      error_msg <- paste0(deparse1(match.call()), ": delayed_datasets identity violated")
      slaves <- rapply(x, function(xx) xx, "delayed_datasets", how = "unlist")
      slaves_datasets <- rapply(x, function(xx) attr(xx, "datasets"), "delayed_datasets", how = "unlist")
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
        init = attr(master, "datasets")
      )
    }
  }
  x
}
