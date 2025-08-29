#' Merge expression for selectors
#' @param selectors (`list` of `picks`)
#' @param output_name (`character(1)`)
#' @param join_fun (`character(1)`) name of the merge function.
#' @param join_keys (`join_keys`)
#' @export
merge_expr <- function(selectors,
                       output_name = "merged",
                       join_fun = "dplyr::left_join",
                       join_keys,
                       allow_cartesian = FALSE) {
  checkmate::assert_list(selectors, c("picks", "reactive"))
  checkmate::assert_string(output_name)
  checkmate::assert_string(join_fun)
  checkmate::assert_class(join_keys, "join_keys")

  merge_summary <- .merge_summary_list(selectors, join_keys = join_keys)
  join_keys <- merge_summary$join_keys
  mapping <- merge_summary$mapping
  mapping <- lapply(mapping, function(x) {
    # because we need `$new_name = $old_name` to rename in select call
    x$variables <- stats::setNames(names(x$variables), unname(x$variables))
    x
  })

  datanames <- unique(unlist(lapply(mapping, `[[`, "datasets")))
  calls <- expression()
  anl_datanames <- character(0) # to follow what anl is composed of (to determine keys)
  anl_primary_keys <- character(0) # to determine accumulated keys of a merged dataset
  for (i in seq_along(datanames)) {
    dataname <- datanames[i]
    this_mapping <- Filter(function(x) x$datasets == dataname, mapping)
    this_foreign_keys <- .fk(join_keys, dataname)
    this_primary_keys <- join_keys[dataname, dataname]
    this_variables <- c(
      this_foreign_keys,
      unlist(lapply(unname(this_mapping), `[[`, "variables"))
    )
    this_variables <- this_variables[!duplicated(unname(this_variables))] # because unique drops names

    # todo: extract call is datasets (class, class) specific
    this_call <- .call_dplyr_select(dataname = dataname, variables = this_variables)
    if (i > 1) {
      merge_keys <- join_keys["anl", dataname]
      if (!length(merge_keys)) {
        msg <- sprintf(
          "Merge is not possible. No join_keys between %s and any of %s",
          sQuote(dataname),
          sQuote(toString(anl_datanames))
        )
        stop(msg, call. = FALSE)
        next
      }
      anl_vs_this <- setdiff(anl_primary_keys, this_primary_keys)
      this_vs_anl <- setdiff(this_primary_keys, anl_primary_keys)
      if (length(anl_vs_this) && length(this_vs_anl) && !allow_cartesian) {
        validate(need(FALSE, "cartesian join")) # todo: add more info
      }
      this_call <- as.call(
        list(
          str2lang(join_fun),
          y = this_call,
          by = merge_keys,
          suffix = c("", sprintf("_%s", dataname))
        )
      )
    }

    anl_datanames <- c(anl_datanames, dataname)
    anl_primary_keys <- union(anl_primary_keys, this_primary_keys)
    calls <- c(calls, this_call)
  }

  call("<-", str2lang(output_name), calls_combine_by("%>%", calls))
}


merge_srv <- function(data, selectors, join_fun = "dplyr::left_join", output_name = "merged") {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_list(selectors, "specification")
  checkmate::assert_string(join_fun)
  session <- shiny::getDefaultReactiveDomain()

  inputs_out <- sapply(names(selectors), USE.NAMES = TRUE, function(id) {
    module_input_srv(id, spec = selectors[[id]], data = data)
  })

  selectors_r <- reactive(lapply(inputs_out, function(x) x()))


  merged_data <- reactive({
    req(data(), selectors_r())
    expr <- merge_expr(selectors = selectors_r(), join_keys = teal.data::join_keys(data()), output_name = output_name)
    teal.code::eval_code(data(), expr)
  })

  merged_data
}

#' @export
qenv_merge_selectors <- function(x,
                                 selectors,
                                 output_name = "merged",
                                 join_fun = "dplyr::left_join",
                                 allow_cartesian = TRUE) {
  expr <- merge_expr(
    selectors = selectors,
    output_name = output_name,
    join_fun = join_fun,
    join_keys = teal.data::join_keys(x),
    allow_cartesian = allow_cartesian
  )
  eval_code(x, expr)
}

map_merged <- function(selectors, join_keys) {
  .merge_summary_list(selectors, join_keys = join_keys)$mapping
}


#' Analyse selectors and guess merge consequences
#'
#' @return list containing:
#' - mapping (`named list`) containing selected values in each selector. This `mapping`
#'   is sorted according to correct datasets merge order.
#' - join_keys (`join_keys`) updated `join_keys` containing keys of `ANL`
#'
#' @keywords internal
.merge_summary_list <- function(selectors, join_keys) {
  checkmate::assert_list(selectors, c("picks", "reactive"))
  if (missing(join_keys)) {
    join_keys <- Reduce(
      function(all, this) c(all, attr(this, "join_keys")),
      x = selectors,
      init = teal.data::join_keys()
    )
  }

  mapping <- lapply( # what has been selected in each selector
    selectors,
    function(x) {
      obj <- if (is.reactive(x)) x() else x
      selected <- lapply(obj, function(x) stats::setNames(x$selected, x$selected))
    }
  )
  mapped_datanames <- unlist(lapply(mapping, `[[`, "datasets"), use.names = FALSE)
  mapping_by_dataset <- split(mapping, mapped_datanames)

  datanames <- unique(mapped_datanames)
  if (length(datanames) > 1) {
    # datanames are handed over in order of selectors but
    # they must be in topological order - otherwise join might not be possible
    datanames <- c(
      intersect(names(join_keys), datanames),
      setdiff(datanames, names(join_keys)) # non-joinable datasets at the end
    )

    # mapping will be reused so needs to be reordered as well
    mapping <- mapping[order(match(mapped_datanames, datanames))]
  }
  remaining_datanames <- datanames
  join_keys <- join_keys[datanames]
  anl_colnames <- character(0)
  for (dataname in datanames) {
    #   glossary:
    #     dataset/dataname: dataset (or its name) in the current iteration (datasets are merged in a loop)
    #     anl datasets/datanames: datasets (or names) which anl is composed of (this is a cumulative process)
    #     remaining datasets/datanames: datasets (or names) which are about to be merged
    #
    # Rules:
    #   1. selected variables are added to anl.
    #   2. duplicated variables added to anl should be renamed
    #   3. anl "inherits" foreign keys from anl datasets to remaining datasets
    #   4. foreign keys of current dataset are added to anl join_keys but only if no relation from anl already.
    #   5. foreign keys should be renamed if duplicated with anl colnames
    #   6. (for later) selected datasets might not be directly mergable, we need to find the "path" which
    #     will probably involve add intermediate datasets in between to perform merge
    remaining_datanames <- setdiff(remaining_datanames, dataname)
    mapping_ds <- mapping_by_dataset[[dataname]]
    mapping_ds <- lapply(mapping_ds, function(x) {
      new_vars <- .suffix_duplicated_vars(
        #       is dropped by merge call. We should refer this selected foreign-key-variable
        #       to equivalent key variable added in previous iteration (existing anl foreign key)
        # 4. duplicated variables added to anl should be renamed
        vars = x$variables,
        all_vars = anl_colnames,
        suffix = dataname
      )

      # if foreign key of this dataset is selected and if this foreign key took a part in the merge
      #  then this key is dropped and we need to refer to the first variable
      existing_fk <- join_keys[dataname, "anl"] # keys that are already in anl
      existing_fk_selected <- intersect(names(existing_fk), x$variables)
      new_vars[existing_fk_selected] <- existing_fk[existing_fk_selected]
      x$variables <- new_vars
      x
    })
    mapping[names(mapping_ds)] <- mapping_ds

    this_colnames <- unique(unlist(lapply(mapping_ds, `[[`, "variables")))
    anl_colnames <- c(anl_colnames, this_colnames)

    # todo: if this dataset has no join_keys to anl (anl_datasets) then error saying
    #       can't merge {dataset} with merged dataset composed of {anl_datasets}

    # ↓  3. anl "inherits" foreign keys from anl datasets to remaining datasets
    this_join_keys <- do.call(
      teal.data::join_keys,
      lapply(
        remaining_datanames,
        function(dataset_2) {
          new_keys <- join_keys[dataname, dataset_2]
          # ↓ 4. foreign keys of current dataset are added to anl join_keys but only if no relation from anl already
          if (length(new_keys) && !dataset_2 %in% names(join_keys[["anl"]])) {
            # ↓ 5. foreign keys should be renamed if duplicated with anl colnames
            new_key_names <- .suffix_duplicated_vars(
              vars = names(new_keys), # names because we change the key of dataset_1 (not dataset_2)
              all_vars = anl_colnames,
              suffix = dataname
            )
            names(new_keys) <- new_key_names
            join_key(dataset_1 = "anl", dataset_2 = dataset_2, keys = new_keys)
          }
        }
      )
    )
    join_keys <- c(this_join_keys, join_keys)
    anl_colnames <- union(anl_colnames, .fk(join_keys, "anl"))
  }


  list(mapping = mapping, join_keys = join_keys)
}

.fk <- function(x, dataname) {
  this_jk <- x[[dataname]]
  unique(unlist(lapply(this_jk[!names(this_jk) %in% dataname], names)))
}

.suffix_duplicated_vars <- function(vars, all_vars, suffix) {
  idx_duplicated <- vars %in% all_vars
  if (any(idx_duplicated)) {
    vars[idx_duplicated] <- sprintf("%s_%s", vars[idx_duplicated], suffix)
  }
  vars
}
