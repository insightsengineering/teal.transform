#' Returns a reactive list with values read from the inputs of `data_extract_spec`
#'
#' @details
#' Reads the UI inputs of a single `data_extract_spec` object in a running
#' `teal` application.
#' Returns a reactive list of reactive values read from the input.
#'
#' The returned list has keys corresponding to the UI inputs:
#' `select`, `filters`, `always_selected`, `reshape`.
#'
#' @inheritParams data_extract_single_srv
#'
#' @return `shiny::reactive` the reactive list with reactive values read from the UI.
#'
#' @keywords internal
#'
data_extract_read_srv <- function(id, datasets, single_data_extract_spec, iv, select_validation_rule = NULL,
                                  filter_validation_rule = NULL) {
  checkmate::assert_class(single_data_extract_spec, "data_extract_spec")
  checkmate::assert_list(datasets, types = "reactive", names = "named")
  moduleServer(
    id,
    function(input, output, session) {
      logger::log_debug(
        "data_extract_read_srv initialized with: { single_data_extract_spec$dataname } dataset."
      )
      filter_idx <- seq_along(single_data_extract_spec$filter)
      extract_n_process_inputs <- function(idx) {
        x <- single_data_extract_spec$filter[[idx]]
        input_col <- input[[paste0("filter", idx, ns.sep, "col")]]
        input_vals <- input[[paste0("filter", idx, ns.sep, "vals")]]
        # convert to numeric for class consistency because everything coming from input is character, e.g. "1"
        if (length(input_col) == 1L && is.numeric(datasets[[x$dataname]]()[[input_col]])) {
          input_vals <- as.numeric(input_vals)
        }
        for (col in input_col) {
          # replace NA with NA_character_ for class consistency
          if (
            any(vapply(input_vals, identical, logical(1), "NA")) &&
              anyNA(datasets[[x$dataname]]()[col]) &&
              !any(vapply(unique(datasets[[x$dataname]]()[col]), identical, logical(1), "NA"))
          ) {
            input_vals[vapply(input_vals, identical, logical(1), "NA")] <- NA_character_
          }
        }

        selected <- split_by_sep(input_vals, x$sep)

        dn <- single_data_extract_spec$dataname
        cols <- `if`(length(input_col) > 0, paste(input_col, collapse = ", "), "NULL")
        sel <- `if`(length(selected) > 0, paste(selected, collapse = ", "), "NULL")
        logger::log_debug("data_extract_read_srv@1 dataname: { dn }; filter vars: { cols }; filter values: { sel }")

        list(
          columns = input_col,
          selected = selected,
          multiple = x$multiple,
          drop_keys = x$drop_keys
        )
      }

      r_filter <- eventReactive(
        ignoreNULL = FALSE,
        eventExpr = {
          lapply(
            filter_idx,
            function(idx) {
              input[[paste0("filter", idx, ns.sep, "vals")]]
            }
          )
        },
        valueExpr = {
          res <- if (length(single_data_extract_spec$filter) >= 1) {
            lapply(filter_idx, FUN = extract_n_process_inputs)
          }
          res
        }
      )

      if (!is.null(select_validation_rule)) {
        iv$add_rule("select", select_validation_rule)
      }

      if (!is.null(filter_validation_rule)) {
        for (idx in filter_idx) {
          iv$add_rule(
            paste0("filter", idx, ns.sep, "vals"),
            filter_validation_rule
          )
        }
      }

      tracked_input <- Queue$new()
      r_select <- eventReactive(
        ignoreNULL = FALSE,
        eventExpr = {
          input$select
          # Note that r_select reactivity is triggered by filter vals and not filter col.
          # This is intended since filter col updates filter vals which is then updating both r_filter and r_select.
          # If it depends on filter col then there will be two reactivity cycles:
          # (1) filter-col -> r_select -> read -> ... (2) filter-col -> filter-val -> r_filter -> read -> ...
          lapply(
            filter_idx,
            function(idx) {
              input[[paste0("filter", idx, shiny::ns.sep, "vals")]]
            }
          )
        },
        valueExpr = {
          if (isTRUE(single_data_extract_spec$select$ordered)) {
            shinyjs::runjs(
              sprintf(
                '$("#%s").parent().find("span.caret").removeClass("caret").addClass("fas fa-exchange-alt")',
                session$ns("select")
              )
            )
            tracked_input$remove(setdiff(tracked_input$get(), input$select))
            tracked_input$push(setdiff(input$select, tracked_input$get()))
            res <- tracked_input$get()
            res <- if (is.null(res)) character(0) else res
          } else {
            res <- if (is.null(input$select)) {
              if (is.null(single_data_extract_spec$select)) {
                as.character(unlist(lapply(
                  filter_idx,
                  function(idx) {
                    input[[paste0("filter", idx, ns.sep, "col")]]
                  }
                )))
              } else {
                character(0)
              }
            } else {
              input$select
            }

            if (!is.null(input$select_additional)) {
              res <- append(res, input$select_additional)
            }
            res
          }

          dn <- single_data_extract_spec$dataname
          sel <- `if`(length(res) > 0, paste(res, collapse = ", "), "NULL")
          logger::log_debug("data_extract_read_srv@2 dataname: { dn }; select: { sel }.")

          res
        }
      )

      r_reshape <- reactive({
        res <- if (is.null(input$reshape)) {
          FALSE
        } else {
          input$reshape
        }

        dn <- single_data_extract_spec$dataname
        resh <- paste(res, collapse = ", ")
        logger::log_debug("data_extract_read_srv@3 dataname: { dn }; reshape: { resh }.")

        res
      })

      reactive({
        list(
          filters = r_filter(),
          select = r_select(),
          always_selected = single_data_extract_spec$select$always_selected,
          reshape = r_reshape(),
          iv = iv
        )
      })
    }
  )
}
