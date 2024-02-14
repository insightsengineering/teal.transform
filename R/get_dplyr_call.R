#' Aggregates data extract selectors
#'
#' Simplifies `selector_list` into aggregated list with one element per
#' same selector - same dataset, same filter configuration and same reshape status.
#'
#' @inheritParams get_merge_call
#'
#' @return (`list`) simplified selectors with aggregated set of filters,
#' selections, reshapes etc. All necessary data for merging.
#'
#' @keywords internal
#'
get_dplyr_call_data <- function(selector_list, join_keys = teal.data::join_keys()) {
  logger::log_trace("get_dplyr_call_data called with: { paste(names(selector_list), collapse = ', ') } selectors.")
  checkmate::assert_class(join_keys, "join_keys")
  lapply(selector_list, check_selector)

  all_merge_key_list <- get_merge_key_grid(selector_list, join_keys)
  res <- lapply(
    seq_along(selector_list),
    function(idx) {
      internal_id <- selector_list[[idx]]$internal_id

      merge_keys_list <- all_merge_key_list[[idx]]

      merge_keys <- if (length(merge_keys_list) > 1) {
        unique(unlist(lapply(merge_keys_list[-idx], names)))
      } else {
        names(merge_keys_list[[1]])
      }

      if (isFALSE(selector_list[[idx]]$reshape)) {
        unite_cols <- character(0)
        pivot_longer_cols <- character(0)
        unite_vals <- character(0)
      } else {
        unite_cols <- get_reshape_unite_col(selector_list[[idx]])
        pivot_longer_cols <- get_pivot_longer_col(selector_list[[idx]])
        unite_vals <- get_reshape_unite_vals(selector_list[[idx]])
      }

      selector_cols <- c(selector_list[[idx]]$select)
      init_select_cols <- unique(c(pivot_longer_cols, selector_cols))
      init_select_cols_with_keys <- unique(c(merge_keys, unite_cols, pivot_longer_cols, selector_cols))
      # can change order of keys

      list(
        internal_id = internal_id,
        merge_keys_list = merge_keys_list,
        unite_cols = unite_cols,
        unite_vals = unite_vals,
        pivot_longer_cols = pivot_longer_cols,
        selector_cols = selector_cols,
        init_select_cols_with_keys = init_select_cols_with_keys,
        init_select_cols = init_select_cols
      )
    }
  )

  # rename duplicated non-key columns
  all_cols <- unlist(lapply(res, `[[`, "init_select_cols"))
  for (idx1 in seq_along(res)) {
    init_select_cols <- res[[idx1]]$init_select_cols
    internal_id <- res[[idx1]]$internal_id
    selector_cols <- res[[idx1]]$selector_cols
    unite_cols <- res[[idx1]]$unite_cols
    unite_vals <- res[[idx1]]$unite_vals
    pivot_longer_cols <- res[[idx1]]$pivot_longer_cols
    merge_keys <- unique(unlist(res[[idx1]]$merge_keys_list))

    init_select_cols_renamed <- rename_duplicated_cols(
      setdiff(init_select_cols, merge_keys),
      internal_id,
      setdiff(selector_cols, unite_cols),
      all_cols
    )

    pivot_longer_cols_renamed <- rename_duplicated_cols(
      pivot_longer_cols,
      internal_id,
      setdiff(selector_cols, unite_cols),
      all_cols
    )

    pivot_longer_unite_cols_renamed <- if (rlang::is_empty(unite_vals)) { # nolint: object_length_linter.
      pivot_longer_cols_renamed
    } else {
      Reduce(
        append,
        mapply(
          function(x1, name) {
            stats::setNames(paste(x1, unite_vals, sep = "_"), rep(name, length(unite_vals)))
          },
          x1 = pivot_longer_cols_renamed,
          name = pivot_longer_cols,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      )
    }

    selector_cols_renamed <- rename_duplicated_cols(
      init_select_cols,
      internal_id,
      setdiff(selector_cols, unite_cols),
      all_cols[!all_cols %in% merge_keys]
    )

    out_cols_renamed <- if (!rlang::is_empty(pivot_longer_unite_cols_renamed)) {
      pivot_longer_unite_cols_renamed
    } else {
      selector_cols_renamed
    }

    res[[idx1]]$init_select_cols_renamed <- init_select_cols_renamed
    res[[idx1]]$pivot_longer_cols_renamed <- pivot_longer_cols_renamed
    res[[idx1]]$out_cols_renamed <- out_cols_renamed
  }
  res
}

#' Parses filter, select, rename and reshape call
#'
#' @inheritParams get_dplyr_call_data
#'
#' @param idx optional (`integer`) current selector index in all selectors list.
#' @param dplyr_call_data (`list`) simplified selectors with aggregated set of filters,
#' selections, reshapes etc. All necessary data for merging.
#' @param data (`NULL` or named `list`) of datasets.
#'
#' @return (`call`) filter, select, rename and reshape call.
#'
#' @keywords internal
#'
get_dplyr_call <- function(selector_list,
                           idx = 1L,
                           join_keys = teal.data::join_keys(),
                           dplyr_call_data = get_dplyr_call_data(selector_list, join_keys = join_keys),
                           datasets = NULL) {
  logger::log_trace(
    paste(
      "get_dplyr_call called with:",
      "{ paste(names(datasets), collapse = ', ') } datasets;",
      "{ paste(names(selector_list), collapse = ', ') } selectors."
    )
  )
  lapply(selector_list, check_selector)
  checkmate::assert_class(join_keys, "join_keys", null.ok = TRUE)
  checkmate::assert_integer(idx, len = 1, any.missing = FALSE)

  n_selectors <- length(selector_list)

  dataname_filtered <- as.name(selector_list[[idx]]$dataname)

  filter_call <- get_filter_call(selector_list[[idx]]$filters, selector_list[[idx]]$dataname, datasets)

  select_call <- get_select_call(dplyr_call_data[[idx]]$init_select_cols_with_keys)

  rename_call <- if (n_selectors > 1) {
    get_rename_call(dplyr_call_data = dplyr_call_data, idx = idx)
  } else {
    NULL
  }

  reshape_call <- if (isTRUE(selector_list[[idx]]$reshape)) {
    get_reshape_call(dplyr_call_data = dplyr_call_data, idx = idx)
  } else {
    NULL
  }

  Reduce(
    function(x, y) call("%>%", x, y),
    Filter(function(x) !is.null(x), c(dataname_filtered, filter_call, select_call, rename_call, reshape_call))
  )
}

#' Parse `dplyr` select call
#'
#' @param select (`character`) vector of selected column names.
#'
#' @return `dplyr` select `call`.
#'
#' @keywords internal
#'
get_select_call <- function(select) {
  logger::log_trace("get_select_call called with: { paste(select, collapse = ', ') } columns.")
  if (is.null(select) || length(select) == 0) {
    return(NULL)
  }

  select <- unique(select)

  as.call(c(list(quote(dplyr::select)), lapply(select, as.name)))
}

#' Build a `dplyr` filter call
#'
#' @param filter (`list`) Either list of lists or list with `select` and `selected` items.
#' @param dataname (`NULL` or `character`) name of dataset.
#' @param datasets (`NULL` or named `list`).
#'
#' @return `dplyr` filter `call`.
#'
#' @keywords internal
#'
get_filter_call <- function(filter, dataname = NULL, datasets = NULL) {
  logger::log_trace(
    paste(
      "get_filter_call called with:",
      "{ dataname } dataset;",
      "{ paste(sapply(filter, function(x) x$columns), collapse = ', ') } filters."
    )
  )
  checkmate::assert_list(datasets, types = "reactive", names = "named", null.ok = TRUE)
  if (is.null(filter)) {
    return(NULL)
  }

  stopifnot(
    (!is.null(dataname) && is.null(datasets)) ||
      (is.null(dataname) && is.null(datasets)) ||
      (!is.null(datasets) && isTRUE(dataname %in% names(datasets)))
  )

  get_filter_call_internal <- function(filter, dataname, datasets) {
    if (rlang::is_empty(filter$selected)) {
      return(FALSE)
    }

    keys <- filter$columns
    datas_vars <- if (!is.null(datasets)) datasets[[dataname]]() else NULL

    if (!is.null(datas_vars)) {
      u_variables <- unique(apply(datas_vars[, keys, drop = FALSE], 1, function(x) paste(x, collapse = "-")))
      selected <- if (length(keys) == 1) {
        selected_single <- unlist(filter$selected)
        # We need character NA as for rest vars the NA is translated to "NA" by paste function
        selected_single[is.na(selected_single)] <- "NA"
        selected_single
      } else {
        unlist(lapply(filter$selected, function(x) paste(x, collapse = "-")))
      }
      # we don't want to process the key which all values are selected
      # this means that call for this key is redundant and will be skipped
      if (all(u_variables %in% selected)) {
        keys <- NULL
      }
    }

    if (length(keys) == 1) {
      key_name <- unlist(keys)
      key_value <- unlist(filter$selected)
      varname <- if (isTRUE(inherits(datas_vars[[key_name]], c("POSIXct", "POSIXlt", "POSIXt")))) {
        bquote(trunc(.(as.name(key_name))))
      } else {
        as.name(key_name)
      }

      if (length(key_value) == 1 && is.na(key_value)) {
        call("is.na", as.name(key_name))
      } else {
        call_condition_choice(varname = varname, choices = key_value)
      }
    } else if (length(keys) > 1) {
      calls_combine_by(
        "|",
        lapply(
          filter$selected,
          function(keys_values) {
            res <- calls_combine_by(
              "&",
              Map(
                keys,
                keys_values,
                f = function(key_name, key_value) {
                  if (is.na(key_value)) {
                    call("is.na", as.name(key_name))
                  } else {
                    varname <- if (isTRUE(inherits(datas_vars[[key_name]], c("POSIXct", "POSIXlt", "POSIXt")))) {
                      bquote(trunc(.(as.name(key_name))))
                    } else {
                      as.name(key_name)
                    }

                    call_condition_choice(
                      varname = varname,
                      key_value
                    )
                  }
                }
              )
            )
            call("(", res)
          }
        )
      )
    }
  }

  internal <- if (length(filter) == 1) {
    get_filter_call_internal(filter[[1]], dataname, datasets)
  } else {
    res <- Filter(Negate(is.null), Map(function(x) get_filter_call_internal(x, dataname, datasets), filter))
    calls_combine_by("&", res)
  }


  if (!is.null(internal)) {
    as.call(c(quote(dplyr::filter), internal))
  } else {
    NULL
  }
}

#' Remove duplicated columns
#' @keywords internal
#' @noRd
#'
rename_duplicated_cols <- function(x, internal_id, selected_cols, all_cols) {
  all_cols_dups <- all_cols[duplicated(all_cols)]
  vapply(
    x,
    function(y) {
      ifelse(y %in% selected_cols && y %in% all_cols_dups, paste0(internal_id, ".", y), y)
    },
    character(1)
  )
}

#' Returns `dplyr` rename call
#'
#' Rename is used only if there are duplicated columns.
#'
#' @inheritParams get_dplyr_call
#'
#' @return (`call`) `dplyr` rename call.
#'
#' @keywords internal
#'
get_rename_call <- function(selector_list = list(),
                            idx = 1L,
                            join_keys = teal.data::join_keys(),
                            dplyr_call_data = get_dplyr_call_data(selector_list, join_keys = join_keys)) {
  checkmate::assert_integer(idx, len = 1, any.missing = FALSE)
  stopifnot(length(dplyr_call_data) >= idx)
  logger::log_trace(
    paste(
      "get_rename_call called with:",
      "{ dplyr_call_data[[idx]]$internal_id } selector;",
      "{ paste(dplyr_call_data[[idx]]$init_select_cols_renamed, collapse = ', ') } renamed columns."
    )
  )

  lapply(selector_list, check_selector)

  rename_dict <- dplyr_call_data[[idx]]$init_select_cols_renamed
  rename_dict <- rename_dict[names(rename_dict) != rename_dict]

  if (is.null(rename_dict) || length(rename_dict) == 0) {
    return(NULL)
  }

  internal <- stats::setNames(lapply(names(rename_dict), as.name), rename_dict)

  as.call(append(quote(dplyr::rename), internal))
}

#' Returns `dplyr` reshape call
#'
#' @inheritParams get_dplyr_call
#'
#' @return List of multiple `dplyr` calls that reshape data.
#'
#' @keywords internal
#'
get_reshape_call <- function(selector_list = list(),
                             idx = 1L,
                             join_keys = teal.data::join_keys(),
                             dplyr_call_data = get_dplyr_call_data(selector_list, join_keys = join_keys)) {
  checkmate::assert_integer(idx, len = 1, any.missing = FALSE)
  stopifnot(length(dplyr_call_data) >= idx)
  logger::log_trace(
    paste(
      "get_reshape_call called with:",
      "{ dplyr_call_data[[idx]]$internal_id } selector;",
      "{ paste(dplyr_call_data[[idx]]$unite_cols, collapse = ', ') } reshaping columns;",
      "{ paste(dplyr_call_data[[idx]]$pivot_longer_cols, collapse = ', ') } reshaped columns."
    )
  )
  lapply(selector_list, check_selector)

  pl_cols <- unname(dplyr_call_data[[idx]]$pivot_longer_cols_renamed)

  pivot_longer_call <- as.call(list(
    quote(tidyr::pivot_longer),
    cols = if (length(pl_cols)) pl_cols else quote(tidyselect::everything()),
    names_to = "MEASURE",
    values_to = "VALUE"
  ))

  unite_call <- as.call(c(
    list(quote(tidyr::unite)),
    quote(KEY),
    quote(MEASURE),
    lapply(
      dplyr_call_data[[idx]]$unite_cols,
      function(x) {
        as.name(x)
      }
    )
  ))

  pivot_wider_call <- as.call(list(
    quote(tidyr::pivot_wider),
    names_from = "KEY",
    values_from = "VALUE"
  ))

  c(pivot_longer_call, unite_call, pivot_wider_call)
}


#' Get pivot longer  columns
#'
#' Get values names which are spread into columns.
#'
#' @param selector one element of selector_list obtained by `get_dplyr_call_data`.
#'
#' @return A `character` vector of all the selected columns that are not a `keys` element.
#'
#' @keywords internal
#'
get_pivot_longer_col <- function(selector) {
  logger::log_trace("get_reshape_unite_col called with: { selector$internal_id } selector.")
  setdiff(selector$select, selector$keys)
}

#' Get unite columns
#'
#' Get key names which spreads values into columns. Reshape is done only
#' on keys which are in `filter_spec`.
#'
#' @inheritParams get_pivot_longer_col
#'
#' @return A `character` vector of all the selector's keys that are defined in the filters.
#'
#' @keywords internal
#'
get_reshape_unite_col <- function(selector) {
  logger::log_trace("get_reshape_unite_col called with: { selector$internal_id } selector.")
  intersect(
    selector$keys,
    unlist(lapply(selector$filters, `[[`, "columns"))
  )
}

#' Get unite columns values
#'
#' Get key values (levels) of the unite columns.
#'
#' @inheritParams get_pivot_longer_col
#'
#' @return A `character` vector of keys of the unite columns.
#'
#' @keywords internal
#'
get_reshape_unite_vals <- function(selector) {
  logger::log_trace("get_reshape_unite_vals called with: { selector$internal_id } selector.")
  unite_cols <- get_reshape_unite_col(selector)
  filters <- selector$filters
  filters_columns <- lapply(filters, `[[`, "columns")

  # first check if combined filter exists then check one by one
  filters_idx <- which(vapply(filters_columns, function(x) identical(unite_cols, x), logical(1)))
  if (length(filters_idx) == 0) {
    filters_idx <- which(filters_columns %in% unite_cols)
  }

  unite_cols_vals <- lapply(
    filters[filters_idx],
    function(x) {
      vapply(x$selected, paste, character(1), collapse = "_")
    }
  )
  unite_cols_vals <- unite_cols_vals[vapply(unite_cols_vals, length, integer(1)) > 0]

  if (length(unite_cols_vals) > 0) {
    grid <- do.call(expand.grid, args = list(unite_cols_vals, stringsAsFactors = FALSE))
    apply(grid, 1, paste, collapse = "_")
  } else {
    character(0)
  }
}
