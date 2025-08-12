#' Get merge call from a list of selectors
#'
#' @description
#'
#' Creates list of calls depending on selector(s) and type of the merge.
#' The merge order is the same as in selectors passed to the function.
#'
#' @inheritParams merge_datasets
#' @param join_keys (`join_keys`) nested list of keys used for joining.
#' @param dplyr_call_data (`list`) simplified selectors with aggregated set of filters.
#'
#' @return List with merge `call` elements.
#'
#' @export
#'
get_merge_call <- function(selector_list,
                           join_keys = teal.data::join_keys(),
                           dplyr_call_data = get_dplyr_call_data(selector_list, join_keys = join_keys),
                           merge_function = "dplyr::full_join",
                           anl_name = "ANL") {
  if (!missing(selector_list)) {
    checkmate::assert_list(selector_list, min.len = 1)
    lapply(selector_list, check_selector)
    logger::log_debug(
      paste(
        "get_merge_call called with: { paste(names(selector_list), collapse = ', ') } selectors;",
        "{ merge_function } merge function."
      )
    )
  } else {
    logger::log_debug(
      paste(
        "get_merge_call called with:",
        "{ paste(sapply(dplyr_call_data, `[[`, 'internal_id'), collapse = ', ') } selectors;",
        "{ merge_function } merge function."
      )
    )
  }

  checkmate::assert_string(anl_name)
  stopifnot(attr(regexec("[A-Za-z0-9\\_]*", anl_name)[[1]], "match.length") == nchar(anl_name))
  check_merge_function(merge_function)


  n_selectors <- if (!missing(selector_list)) {
    length(selector_list)
  } else {
    length(dplyr_call_data)
  }

  anl_merge_calls <- list(
    call("<-", as.name(anl_name), as.name(paste0(anl_name, "_", 1)))
  )

  for (idx in seq_len(n_selectors)[-1]) {
    anl_merge_call_i <- call(
      "<-",
      as.name(anl_name),
      {
        merge_key_i <- get_merge_key_i(idx = idx, dplyr_call_data = dplyr_call_data)
        is_merge_key_pair <- vapply(merge_key_i, function(x) length(names(x)) == 1, logical(1))

        join_call <- as.call(
          c(
            rlang::parse_expr(merge_function),
            list(
              as.name(anl_name),
              as.name(paste0(anl_name, "_", idx))
            ),
            if (!rlang::is_empty(merge_key_i)) {
              list(
                by = parse_merge_key_i(merge_key = merge_key_i)
              )
            }
          )
        )

        # mutate call to get second key if any pair key
        # e.g. full_join(dt1, dt2, by = c("key1" = "key2")) %>% mutate(key2 = key1)
        # it's because dplyr joins preserve only key from LHS data
        mutate_call <- if (any(is_merge_key_pair)) {
          merge_key_pairs <- merge_key_i[is_merge_key_pair]
          # drop duplicates ignoring names
          idx <- vapply(unique(unlist(merge_key_pairs)), function(x1) {
            which.min(vapply(merge_key_pairs, function(x2) x2 == x1, logical(1)))
          }, integer(1))

          merge_key_pairs <- merge_key_pairs[idx]
          as.call(
            append(
              quote(dplyr::mutate),
              stats::setNames(
                lapply(merge_key_pairs, function(x) as.name(names(x))),
                merge_key_pairs
              )
            )
          )
        } else {
          NULL
        }

        Reduce(
          function(x, y) call("%>%", x, y),
          c(join_call, mutate_call)
        )
      }
    )

    anl_merge_calls <- append(
      anl_merge_calls,
      anl_merge_call_i
    )
  }

  anl_merge_calls
}

#' Gets merge key pair list from keys list
#'
#' @inheritParams get_merge_call
#'
#' @return List of merge key pairs between all datasets.
#'
#' @keywords internal
#'
get_merge_key_grid <- function(selector_list, join_keys = teal.data::join_keys()) {
  logger::log_debug(
    "get_merge_key_grid called with: { paste(names(selector_list), collapse = ', ') } selectors."
  )

  lapply(
    selector_list,
    function(selector_from) {
      lapply(
        selector_list,
        function(selector_to) {
          get_merge_key_pair(
            selector_from,
            selector_to,
            join_keys[selector_from$dataname, selector_to$dataname]
          )
        }
      )
    }
  )
}

#' Gets keys vector from keys list
#'
#' @details
#' This function covers up to now 4 cases:
#'
#' * Dataset without parent: Primary keys are returned;
#' * Dataset source = dataset target:
#' The primary keys subtracted of all key columns that get purely filtered.
#' This means just one value would be left after filtering inside this column
#' Then it can be taken out;
#' * Target `dataname` is parent foreign keys;
#' * Any other case foreign keys;
#'
#' @param selector_from (`list`) of `data_extract_srv` objects.
#' @param selector_to (`list`) of `data_extract_srv` objects.
#' @param key_from (`character`) keys used in the first selector while joining.
#'
#' @return `character` vector of selector keys.
#'
#' @keywords internal
#'
get_merge_key_pair <- function(selector_from, selector_to, key_from) {
  logger::log_debug(
    paste(
      "get_merge_key_pair called with:",
      "{ paste(selector_from$internal_id, selector_to$internal_id, sep = ', ') } selectors;",
      "{ paste(key_from, collapse = ', ') } keys."
    )
  )
  check_selector(selector_from)
  check_selector(selector_to)
  checkmate::test_character(key_from, min.len = 0, any.missing = FALSE)

  res <- if (identical(selector_from$dataname, selector_to$dataname)) {
    # key is dropped if reshape or if filtered out (only one level selected)
    keys_dropped <- if (isTRUE(selector_from$reshape)) {
      get_reshape_unite_col(selector_from)
    } else {
      get_dropped_filters(selector_from)
    }
    res <- setdiff(
      key_from,
      keys_dropped
    )
    if (!rlang::is_empty(res)) res <- rlang::set_names(res)
    res
  } else {
    key_from
  }
  logger::log_debug("get_merge_key_pair returns { paste(res, collapse = ', ') } merge keys.")
  res
}

#' Gets keys needed for join call of two selectors
#'
#' @inheritParams get_merge_call
#' @param idx (`integer`) optional, current selector index in all selectors list.
#'
#' @return `character` list of keys.
#'
#' @keywords internal
#'
get_merge_key_i <- function(selector_list, idx, dplyr_call_data = get_dplyr_call_data(selector_list)) {
  checkmate::assert_integer(idx, len = 1, any.missing = FALSE, lower = 2L)

  if (!missing(selector_list)) {
    checkmate::assert_list(selector_list, min.len = 1)
    lapply(selector_list, check_selector)

    logger::log_debug(
      paste(
        "get_merge_key_i called with:",
        "{ paste(names(selector_list), collapse = ', ') } selectors;",
        "idx = { idx }."
      )
    )
  } else {
    logger::log_debug(
      paste(
        "get_merge_key_i called with",
        "{ paste(sapply(dplyr_call_data, `[[`, 'internal_id'), collapse = ', ') } selectors;",
        "idx = { idx }."
      )
    )
  }

  merge_keys_list <- lapply(dplyr_call_data, `[[`, "merge_keys_list")

  # keys x - get from all selectors up to the current one
  keys_x <- lapply(merge_keys_list[seq_len(idx - 1)], `[[`, idx)

  # keys y - get from the current selector
  keys_y <- merge_keys_list[[idx]][seq_len(idx - 1)]

  keys_map <- lapply(
    seq_len(idx - 1),
    function(idx2) {
      keys_x_idx2 <- keys_x[[idx2]]
      keys_y_idx2 <- keys_y[[idx2]]
      min_length <- min(length(keys_x_idx2), length(keys_y_idx2))

      # In case the keys might be wrongly sorted, sort them
      if (!identical(keys_x_idx2[seq_len(min_length)], keys_y_idx2[seq_len(min_length)])) {
        keys_x_idx2 <- c(
          intersect(keys_x_idx2, keys_y_idx2),
          setdiff(keys_x_idx2, keys_y_idx2)
        )

        keys_y_idx2 <- c(
          intersect(keys_y_idx2, keys_x_idx2),
          setdiff(keys_y_idx2, keys_x_idx2)
        )
      }
      # cut keys case of different length
      keys_x_idx2 <- keys_x_idx2[seq_len(min_length)]
      keys_y_idx2 <- keys_y_idx2[seq_len(min_length)]

      mapply(
        function(x, y) {
          if (identical(x, y)) {
            x
          } else {
            stats::setNames(nm = y, x)
          }
        },
        keys_x_idx2,
        keys_y_idx2,
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )
    }
  )

  keys_map <- if (length(keys_map) > 1) {
    Reduce(append, keys_map)
  } else {
    keys_map[[1]]
  }

  keys_map <- unique(keys_map)
  logger::log_debug("get_merge_key_i returns { paste(keys_map, collapse = ' ') } unique keys.")
  keys_map
}

#' Parses merge keys
#'
#' @inheritParams get_merge_call
#' @param merge_key keys obtained from `get_merge_key_i`.
#' @param idx (`integer`) optional, current selector index in all selectors list.
#'
#' @return `call` with merge keys.
#'
#' @keywords internal
#'
parse_merge_key_i <- function(selector_list,
                              idx,
                              dplyr_call_data = get_dplyr_call_data(selector_list),
                              merge_key = get_merge_key_i(selector_list, idx, dplyr_call_data)) {
  logger::log_debug("parse_merge_key_i called with { paste(merge_key, collapse = ' ') } keys.")
  as.call(
    append(
      quote(c),
      unlist(merge_key)
    )
  )
}

#' Names of filtered-out filters dropped from selection
#'
#' @details
#' Names of filtered-out filters dropped from automatic selection
#' (key vars are automatically included in select).
#' Dropped filter is filter which became not unique for all observations.
#' This means that if variable is filtered to just one level,
#' it's not a key anymore.
#'
#' Other variables used in filter should also be dropped from automatic
#' selection, unless they have been selected.
#'
#' @inheritParams get_pivot_longer_col
#'
#' @return Vector of `character` names of the filters which should be dropped from select call.
#'
#' @keywords internal
#'
get_dropped_filters <- function(selector) {
  logger::log_debug("get_dropped_filters called with { selector$internal_id } selector.")
  unlist(
    lapply(selector$filters, function(x) {
      if (isFALSE(x$drop_keys)) {
        NULL
      } else if (length(x$columns) > 1) {
        # concatenated filters
        single_selection <- sapply(seq_along(x$columns), function(i) length(unique(sapply(x$selected, `[[`, i))) == 1)
        x$columns[single_selection]
      } else {
        # one filter in one input
        if (isFALSE(x$multiple) || length(x$selected) == 1) x$columns
      }
    })
  )
}


#' Gets the relabel call
#'
#' @inheritParams merge_datasets
#' @param columns_source (named `list`)
#' where names are column names, values are labels + additional attribute `dataname`
#'
#' @return (`call`) to relabel `dataset` and assign to `anl_name`.
#'
#' @export
get_anl_relabel_call <- function(columns_source, datasets, anl_name = "ANL") {
  logger::log_debug(
    paste(
      "get_anl_relabel_call called with:",
      "{ paste(names(columns_source), collapse = ', ') } columns_source;",
      "{ anl_name } merged dataset."
    )
  )
  checkmate::assert_string(anl_name)
  stopifnot(attr(regexec("[A-Za-z0-9\\_]*", anl_name)[[1]], "match.length") == nchar(anl_name))
  labels_vector <- Reduce(
    function(x, y) append(x, y),
    lapply(
      columns_source,
      function(selector) {
        column_names <- names(selector)
        if (rlang::is_empty(column_names)) {
          return(NULL)
        }

        data_used <- datasets[[attr(selector, "dataname")]]
        labels <- teal.data::col_labels(data_used(), fill = FALSE)
        column_labels <- labels[intersect(colnames(data_used()), column_names)]

        # NULL for no labels at all, character(0) for no labels for a given columns
        return(
          if (rlang::is_empty(column_labels)) {
            column_labels
          } else {
            stats::setNames(
              column_labels,
              selector[names(column_labels)]
            )
          }
        )
      }
    )
  )

  if (length(labels_vector) == 0 || all(is.na(labels_vector))) {
    return(NULL)
  }

  relabel_call <- call(
    "%>%",
    as.name(anl_name),
    get_relabel_call(labels_vector)
  )

  relabel_and_assign_call <- call(
    "<-",
    as.name(anl_name),
    relabel_call
  )

  relabel_and_assign_call
}

#' Create relabel call from named character
#'
#' @description
#' Function creates relabel call from named character.
#'
#' @param labels (named `character`)
#' where name is name is function argument name and value is a function argument value.
#'
#' @return `call` object with relabel step.
#'
#' @examples
#' get_relabel_call(
#'   labels = c(
#'     x = as.name("ANL"),
#'     AGE = "Age",
#'     AVAL = "Continuous variable"
#'   )
#' )
#'
#' get_relabel_call(
#'   labels = c(
#'     AGE = "Age",
#'     AVAL = "Continuous variable"
#'   )
#' )
#' @export
get_relabel_call <- function(labels) {
  logger::log_debug("get_relabel_call called with: { paste(labels, collapse = ' ' ) } labels.")
  if (length(stats::na.omit(labels)) == 0 || is.null(names(labels))) {
    return(NULL)
  }
  labels <- labels[!duplicated(names(labels))]
  labels <- labels[!is.na(labels)]

  as.call(
    append(
      quote(teal.data::col_relabel),
      labels
    )
  )
}

#' Get columns to relabel
#'
#' Get columns to relabel excluding these which has been reshaped (pivot_wider).
#'
#' @param columns_source (`list`)
#' @param dplyr_call_data (`list`)
#'
#' @return `columns_source` list without columns that have been reshaped.
#'
#' @keywords internal
#'
get_relabel_cols <- function(columns_source, dplyr_call_data) {
  logger::log_debug(
    "get_relabel_cols called with: { paste(names(columns_source), collapse = ', ') } columns_source."
  )
  pivot_longer_cols <- unlist(unname(lapply(dplyr_call_data, function(x) x[["pivot_longer_cols_renamed"]])))
  lapply(
    columns_source,
    function(column_source) {
      dataname <- attr(column_source, "dataname")
      column_source <- column_source[!names(column_source) %in% pivot_longer_cols]
      if (length(column_source) == 0) {
        return(NULL)
      }
      attr(column_source, "dataname") <- dataname
      column_source
    }
  )
}
