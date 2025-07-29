#' @param data (`teal_data`)
#' @param selectors (`list` of `specification`)
#' @param join_fun (`character(1)`) name of the merge function.
#' @param ignore_disconnected (`logical(1)`)
#' @export
merge_expr <- function(data, selectors, join_fun = "dplyr::left_join", ignore_disconnected = TRUE) {
  checkmate::assert_class(data, "teal_data")
  checkmate::assert_list(selectors, "specification")
  checkmate::assert_string(join_fun)
  checkmate::assert_flag(ignore_disconnected)

  # IMPORTANT! Merge just need a dispatch on the class of the first object
  #           as merge (left_join) of data.frame and MAE is still a data.frame
  #           it is just a matter of "select" which is dataset specific
  datanames <- unique(unlist(lapply(selectors, function(x) x$datasets$selected)))

  calls <- expression()
  anl_datanames <- character(0) # to follow what anl is composed of (to determine keys)
  anl_variables <- character(0)
  for (i in seq_along(datanames)) {
    dataname <- datanames[i]
    this_selectors <- Filter(function(x) identical(x$datasets$selected, dataname), selectors)
    this_keys <- teal.data::join_keys(data)[[dataname]]
    this_variables <- union(
      unlist(this_keys), # todo: all keys or only those which connects `datanames`?
      unlist(lapply(this_selectors, function(x) x$variables$selected))
    )

    # todo: extract call is datasets (class, class) specific
    this_call <- call_extract_matrix(dataname = dataname, column = this_variables)
    if (i > 1) {
      # merge by cumulated keys
      merge_keys <- unique(unlist(teal.data::join_keys(data)[[dataname]][names(this_keys) %in% anl_datanames]))
      if (!length(merge_keys)) {
        msg <- sprintf(
          "Merge is not possible. No join_keys between %s and any of %s",
          dataname,
          sQuote(toString(anl_datanames))
        )
        if (ignore_disconnected) warning(msg, call. = FALSE) else stop(msg, call. = FALSE)
        next
      }
      this_call <- as.call(
        list(
          str2lang(join_fun),
          y = this_call,
          by = merge_keys
        )
      )
    }

    anl_datanames <- c(anl_datanames, dataname)
    anl_variables <- c(anl_variables, this_variables)
    calls <- c(calls, this_call)
  }

  call("<-", str2lang("anl"), calls_combine_by("%>%", calls))
}
