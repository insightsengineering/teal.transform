#' Merge the datasets on the keys
#'
#' @description `r lifecycle::badge("stable")`
#' It combines/merges multiple datasets with specified keys attribute.
#'
#'
#' @details Internally this function uses calls to allow reproducibility.
#'
#' @inheritParams data_merge_srv
#' @return merged_dataset (`list`) containing:
#' \itemize{
#'  \item data (`data.frame`) after filtering and reshaping containing selected columns.
#'  \item `expr` (`character`) code needed to replicate merged dataset.
#'  \item columns_source (`list`) of column selected for particular selector.
#'  \item keys (`list`) the keys of the merged dataset.
#'  \item filter_info (`list`) The information given by the user. This information
#'    defines the filters that are applied on the data. Additionally it defines
#'    the variables that are selected from the data sets.
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # inside teal module server function
#' response <- data_extract_srv(
#'   id = "reponse",
#'   data_extract_spec = response_spec,
#'   datasets = datasets
#' )
#' regressor <- data_extract_srv(
#'   id = "regressor",
#'   data_extract_spec = regressor_spec,
#'   datasets = datasets
#' )
#' merged_data <- merge_datasets(list(regressor(), response()))
#' }
merge_datasets <- function(selector_list, datasets, merge_function = "dplyr::full_join", anl_name = "ANL") {
  logger::log_trace(
    paste(
      "merge_datasets called with:",
      "{ paste(datasets$datanames(), collapse = ', ') } datasets;",
      "{ paste(names(selector_list), collapse = ', ') } selectors;",
      "{ merge_function } merge function."
    )
  )
  checkmate::assert_list(selector_list, min.len = 1)
  checkmate::assert_string(anl_name)
  stopifnot(attr(regexec("[A-Za-z0-9\\_]*", anl_name)[[1]], "match.length") == nchar(anl_name))
  lapply(selector_list, check_selector)
  merge_selectors_out <- merge_selectors(selector_list)
  merged_selector_list <- merge_selectors_out[[1]]
  merged_selector_map_id <- merge_selectors_out[[2]]
  check_data_merge_selectors(merged_selector_list)

  join_keys <- datasets$get_join_keys()
  dplyr_call_data <- get_dplyr_call_data(merged_selector_list, join_keys)

  validate_keys_sufficient(join_keys, merged_selector_list)

  columns_source <- mapply(
    function(id_from, id_to) {
      id_data <- vapply(dplyr_call_data, `[[`, character(1), "internal_id")
      out_cols <- dplyr_call_data[[which(id_to == id_data)]][["out_cols_renamed"]]
      id_selector <- vapply(selector_list, `[[`, character(1), "internal_id")
      res <- out_cols[names(out_cols) %in% selector_list[[which(id_from == id_selector)]][["select"]]]
      attr(res, "dataname") <- selector_list[[which(id_from == id_selector)]]$dataname
      always_selected <- selector_list[[which(id_from == id_selector)]]$always_selected
      if (is.null(always_selected)) {
        attr(res, "always_selected") <- character(0)
      } else {
        attr(res, "always_selected") <- always_selected
      }
      res
    },
    id_from = names(merged_selector_map_id),
    id_to = merged_selector_map_id,
    SIMPLIFY = FALSE
  )

  chunks_stack <- teal.code::chunks$new()

  new_env <- new.env()

  selector_datanames <- unique(vapply(merged_selector_list, `[[`, character(1), "dataname"))
  for (i in selector_datanames) {
    logger::log_trace("merge_datasets { paste0(i, \"_FILTERED\") } assigned in chunks environment.")
    assign(
      paste0(i, "_FILTERED"),
      datasets$get_data(i, filtered = TRUE),
      envir = new_env
    )
  }
  chunks_stack$reset(envir = new_env)

  for (idx in seq_along(merged_selector_list)) {
    dplyr_call <- get_dplyr_call(
      selector_list = merged_selector_list,
      idx = idx,
      dplyr_call_data = dplyr_call_data,
      datasets = datasets
    )
    anl_i_call <- call("<-", as.name(paste0(anl_name, "_", idx)), dplyr_call)
    chunks_stack$push(anl_i_call, id = paste0("ANL_dplyr_call_", idx))
  }

  anl_merge_calls <- get_merge_call(
    selector_list = merged_selector_list,
    dplyr_call_data = dplyr_call_data,
    merge_function = merge_function,
    anl_name = anl_name
  )
  for (idx in seq_along(anl_merge_calls)) {
    chunks_stack$push(anl_merge_calls[[idx]], id = paste0("get_merge_call_", idx))
  }

  anl_relabel_call <- get_anl_relabel_call(
    columns_source = get_relabel_cols(columns_source, dplyr_call_data), # don't relabel reshaped cols
    datasets = datasets,
    anl_name = anl_name
  )

  if (!is.null(anl_relabel_call)) {
    chunks_stack$push(anl_relabel_call, id = "ANL_relabel_call")
  }

  all_call_string <- paste0(chunks_stack$get_rcode(), collapse = "\n")

  # keys in each merged_selector_list element shoul be identical
  # so take first one
  keys <- merged_selector_list[[1]]$keys

  filter_info <- lapply(merged_selector_list, "[[", "filters")

  # Merge the datasets, ignore errors and warnings (kept in chunks)
  suppressWarnings(chunks_stack$eval())

  res <- list(
    # put data into function to delay chunk code validation
    # This allows the code from chunks to be accessible in "Show R Code"
    # by either "expr" or "chunks" part of the returned list
    data = function() {
      logger::log_trace("merge_datasets { anl_name } dataset returned from chunks.")
      chunks_stack$validate_is_ok()
      chunks_stack$get(anl_name)
    },
    expr = all_call_string,
    chunks = chunks_stack,
    columns_source = columns_source,
    keys = keys,
    filter_info = filter_info
  )
  logger::log_trace("merge_datasets merge code executed resulting in { anl_name } dataset.")
  return(res)
}

#' Merge selectors - select item if all of dataname, reshape, filters and keys items are identical
#'
#' @inheritParams merge_datasets
#'
#' @return error or nothing
#' @keywords internal
#'
#' @examples
#' selector_list <- list(
#'   # ADSL - SEX
#'   list(
#'     filters = NULL, select = "AGE", always_selected = NULL,
#'     reshape = FALSE, dataname = "ADSL",
#'     internal_id = "adsl_var", keys = c("STUDYID", "USUBJID")
#'   ),
#'
#'   # ADSL - AGE
#'   list(
#'     filters = NULL, select = "SEX", always_selected = NULL,
#'     reshape = FALSE, dataname = "ADSL",
#'     internal_id = "adsl_var2", keys = c("STUDYID", "USUBJID")
#'   ),
#'
#'   # ADLB - AVAL
#'   list(
#'     filters = NULL, select = "AVAL", always_selected = NULL,
#'     reshape = FALSE, dataname = "ADLB",
#'     internal_id = "adlb_var", keys = c(
#'       "STUDYID", "USUBJID",
#'       "PARAMCD", "AVISIT"
#'     )
#'   )
#' )
#' merged_selectors <- teal.transform:::merge_selectors(selector_list)
#' merged_selectors
merge_selectors <- function(selector_list) {
  logger::log_trace("merge_selectors called with: { paste(names(selector_list), collapse = ', ') } selectors.")
  checkmate::assert_list(selector_list, min.len = 1)
  lapply(selector_list, check_selector)

  # merge map - idx to value
  # e.g. 1 2 1 means that 3rd selector is merged to 1st selector
  res_map_idx <- seq_along(selector_list)
  for (idx1 in res_map_idx) {
    selector_idx1 <- selector_list[[idx1]]
    for (idx2 in utils::tail(seq_along(res_map_idx), -idx1)) {
      if (res_map_idx[idx2] != idx2) {
        next
      }
      selector_idx2 <- selector_list[[idx2]]
      if (identical(selector_idx1$dataname, selector_idx2$dataname) &&
        identical(selector_idx1$reshape, selector_idx2$reshape) &&
        identical(selector_idx1$filters, selector_idx2$filters) &&
        identical(selector_idx1$keys, selector_idx2$keys)) {
        res_map_idx[idx2] <- idx1
      }
    }
  }

  res_map_id <- stats::setNames(
    vapply(selector_list[res_map_idx], `[[`, character(1), "internal_id"),
    vapply(selector_list, `[[`, character(1), "internal_id")
  )


  res_list <- selector_list
  for (idx in seq_along(res_map_idx)) {
    idx_val <- res_map_idx[[idx]]
    if (idx != idx_val) {
      # merge selector to the "first" identical subset
      res_list[[idx_val]]$select <- union(res_list[[idx_val]]$select, selector_list[[idx]]$select)
    }
  }
  for (idx in rev(seq_along(res_map_idx))) {
    idx_val <- res_map_idx[[idx]]
    if (idx != idx_val) {
      res_list[[idx]] <- NULL
    }
  }

  return(list(res_list, res_map_id))
}


#' Validate data_extracts in merge_datasets
#'
#' Validate selected inputs from data_extract before passing to data_merge to avoid
#' \code{dplyr} errors or unexpected results
#' @inheritParams merge_datasets
#' @return \code{NULL} if check is successful
#' @keywords internal
check_data_merge_selectors <- function(selector_list) {
  # check if reshape n empt select or just primary keys
  lapply(selector_list, function(x) {
    if (x$reshape & length(setdiff(x$select, x$keys)) == 0) {
      validate(need(
        FALSE,
        "Error in data_extract_spec setup:\
        \tPlease select non-key column to be reshaped from long to wide format."
      ))
    }
  })
  NULL
}

#' Validates whether the provided keys are sufficient to merge the datasets slices
#'
#' @note
#' The keys are not sufficient if the datasets slices described in
#' `merged_selector_list` come from datasets, which don't have the
#' appropriate join keys in `join_keys`.
#'
#' @param join_keys (`list`) the provided join keys
#' @param merged_selector_list (`list`) the specification of datasets' slices to merge
#'
#' @return `TRUE` if the provided keys meet the requirements; the `shiny`
#' validate error otherwise
#' @keywords internal
validate_keys_sufficient <- function(join_keys, merged_selector_list) {
  validate(
    need(
      are_needed_keys_provided(join_keys, merged_selector_list),
      message = paste(
        "Cannot merge at least two dataset extracts.",
        "Make sure all datasets used for merging have appropriate keys."
      )
    )
  )

  TRUE
}

#' Checks whether the provided slices have the corresponding join keys
#'
#' @note
#' `merged_selector_list` contains a list of descriptions of data frame slices;
#' each coming from a single dataset. This function checks whether all pairs
#' of the datasets have the join keys needed to merge the slices.
#'
#' @inheritParams validate_keys_sufficient
#'
#' @return `TRUE` if all pairs of the slices have the corresponding keys;
#' `FALSE` otherwise
#' @keywords internal
are_needed_keys_provided <- function(join_keys, merged_selector_list) {
  # because one slice doesn't have to be merged with anything
  if (length(merged_selector_list) <= 1) {
    return(TRUE)
  }

  do_join_keys_exist <- function(dataset_name1, dataset_name2, join_keys) {
    !rlang::is_empty(join_keys[[dataset_name1]][[dataset_name2]])
  }

  datasets_names <- vapply(merged_selector_list, function(slice) slice[["dataname"]], FUN.VALUE = character(1))
  datasets_names_pairs <- utils::combn(datasets_names, m = 2)
  datasets_names_pairs <- datasets_names_pairs[, !duplicated(t(datasets_names_pairs)), drop = FALSE]

  datasets_pairs_keys_present <- apply(
    datasets_names_pairs,
    MARGIN = 2,
    FUN = function(names_pair) do_join_keys_exist(names_pair[1], names_pair[2], join_keys)
  )

  all(datasets_pairs_keys_present)
}