#' Merge module
#'
#' Example module
tm_merge <- function(label = "merge-module", inputs, transformators = list()) {
  module(
    label = label,
    ui = function(id, inputs) {
      ns <- NS(id)
      tags$div(
        tags$div(
          class = "row g-2",
          lapply(names(inputs), function(id) {
            tags$div(
              class = "col-auto",
              tags$strong(tags$label(id)),
              teal.transform::module_input_ui(
                id = ns(id),
                spec = inputs[[id]]
              )
            )
          })
        ),
        shiny::div(
          reactable::reactableOutput(ns("table_merged")),
          shiny::verbatimTextOutput(ns("join_keys")),
          shiny::verbatimTextOutput(ns("mapped")),
          shiny::verbatimTextOutput(ns("src"))
        )
      )
    },
    server = function(id, data, inputs) {
      moduleServer(id, function(input, output, session) {
        selectors <- module_input_srv(id, spec = inputs, data = data)

        merged_q <- reactive({
          req(data())
          lapply(selectors, function(x) req(x()))
          teal.transform::qenv_merge_selectors(x = data(), selectors = selectors)
        })

        table_q <- reactive({
          req(merged_q())
          within(merged_q(), reactable::reactable(merged), selectors = selectors)
        })

        output$table_merged <- reactable::renderReactable({
          req(table_q())
          teal.code::get_outputs(table_q())[[1]]
        })

        output$src <- renderPrint({
          styler::style_text(
            teal.code::get_code(req(table_q()))
          )
        })

        output$join_keys <- renderPrint(teal.data::join_keys(merged_q()))

        output$mapped <- renderText(yaml::as.yaml(map_merged(selectors)))
      })
    },
    ui_args = list(inputs = inputs),
    server_args = list(inputs = inputs),
    transformators = transformators
  )
}



#' @export
merge_srv <- function(id, selectors, data, output_name = "merged", join_fun = "dplyr::left_join") {
  moduleServer(id, function(input, output, session) {
    merged_q <- reactive({
      req(data())
      qenv_merge_selectors(data(), selectors = selectors, output_name = output_name, join_fun = join_fun)
    })
    merge_vars <- reactive(lapply(map_merged(selectors), function(x) x$variables))

    vars_origins <- reactive(
      lapply(selectors, function(x) {
        lapply(x(), `[[`, "selected")
      })
    )
    list(
      data = merged_q,
      merge_vars = merge_vars,
      vars_origins = vars_origins
    )
  })
}


#' Merge expression for selectors
#' @param x ([teal.data::teal_data])
#' @param selectors (`named list` of `picks`)
#' @param output_name (`character(1)`) name of the merged dataset.
#' @param join_fun (`character(1)`) name of the merge function.
#' @export
qenv_merge_selectors <- function(x,
                                 selectors,
                                 output_name = "merged",
                                 join_fun = "dplyr::left_join") {
  checkmate::assert_class(x, "teal_data")
  checkmate::assert_list(selectors, c("picks", "reactive"), names = "named")
  checkmate::assert_string(join_fun)

  merge_summary <- .merge_summary_list(selectors, join_keys = teal.data::join_keys(x))
  expr <- .merge_expr(
    merge_summary = merge_summary,
    output_name = output_name,
    join_fun = join_fun
  )
  merged_q <- eval_code(x, expr)
  teal.data::join_keys(merged_q) <- merge_summary$join_keys
  merged_q
}

#' @export
map_merged <- function(selectors, join_keys) {
  .merge_summary_list(selectors, join_keys = join_keys)$mapping
}

#'
.merge_expr <- function(merge_summary,
                        output_name = "merged",
                        join_fun = "dplyr::left_join") {
  checkmate::assert_list(merge_summary)
  checkmate::assert_string(output_name)
  checkmate::assert_string(join_fun)

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
    this_filter_mapping <- Filter(function(x) {
      "values" %in% names(x)
    }, this_mapping)
    this_foreign_keys <- .fk(join_keys, dataname)
    this_primary_keys <- join_keys[dataname, dataname]
    this_variables <- c(
      this_foreign_keys,
      unlist(lapply(unname(this_mapping), `[[`, "variables"))
    )
    this_variables <- this_variables[!duplicated(unname(this_variables))] # because unique drops names

    # todo: extract call is datasets (class, class) specific
    this_call <- .call_dplyr_select(dataname = dataname, variables = this_variables)
    if (length(this_filter_mapping)) {
      this_call <- calls_combine_by("%>%", c(this_call, .call_dplyr_filter(this_filter_mapping)))
    }

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
      if (length(anl_vs_this) && length(this_vs_anl)) {
        # validate(need(FALSE, "cartesian join")) # todo: add more info
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
    #   1. anl "inherits" foreign keys from anl datasets to remaining datasets
    #   2. foreign keys of current dataset are added to anl join_keys but only if no relation from anl already.
    #   3. foreign keys should be renamed if duplicated with anl colnames
    #   4. (for later) selected datasets might not be directly mergable, we need to find the "path" which
    #     will probably involve add intermediate datasets in between to perform merge
    #   5. selected variables are added to anl.
    #   6. duplicated variables added to anl should be renamed
    remaining_datanames <- setdiff(remaining_datanames, dataname)

    # todo: if this dataset has no join_keys to anl (anl_datasets) then error saying
    #       can't merge {dataset} with merged dataset composed of {anl_datasets}

    # ↓  1. anl "inherits" foreign keys from anl datasets to remaining datasets
    this_join_keys <- do.call(
      teal.data::join_keys,
      lapply(
        remaining_datanames,
        function(dataset_2) {
          new_keys <- join_keys[dataname, dataset_2]
          # ↓ 2. foreign keys of current dataset are added to anl join_keys but only if no relation from anl already
          if (length(new_keys) && !dataset_2 %in% names(join_keys[["anl"]])) {
            # ↓ 3. foreign keys should be renamed if duplicated with anl colnames
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


    mapping_ds <- mapping_by_dataset[[dataname]]
    mapping_ds <- lapply(mapping_ds, function(x) {
      new_vars <- .suffix_duplicated_vars(
        #       is dropped by merge call. We should refer this selected foreign-key-variable
        #       to equivalent key variable added in previous iteration (existing anl foreign key)
        # 6. duplicated variables added to anl should be renamed
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
