#' Module's interactive input
#'
#' @description
#'


#' @export
module_input_ui <- function(id, spec) {
  checkmate::assert_string(id)
  UseMethod("module_input_ui", spec)
}

#' @export
module_input_ui.list <- function(id, spec) {
  checkmate::assert_list(spec, names = "named")
  ns <- shiny::NS(id)
  sapply(
    Filter(length, names(spec)),
    USE.NAMES = TRUE,
    function(name) module_input_ui(ns(name), spec[[name]])
  )
}

#' @export
module_input_ui.picks <- function(id, spec) {
  if (.valid_picks(spec)) {
    stop("Unexpected object used as spec. Use `picks` to create the object.")
  }
  ns <- shiny::NS(id)
  badge_label <- shiny::uiOutput(ns("summary"), container = htmltools::tags$span)
  # todo: icon or color to indicate a column class
  content <- lapply(spec, function(x) .selected_choices_ui(id = ns(is(x)), x))
  htmltools::tags$div(
    # todo: spec to have a label attribute
    teal::badge_dropdown(id = ns("inputs"), label = badge_label, htmltools::tagList(content))
  )
}

#' @export
module_input_srv <- function(id = "", spec, data) {
  checkmate::assert_string(id)
  checkmate::assert_class(data, "reactive")
  UseMethod("module_input_srv", spec)
}

#' @export
module_input_srv.list <- function(id, spec, data) {
  sapply(
    names(Filter(length, spec)),
    USE.NAMES = TRUE,
    function(name) module_input_srv(name, spec[[name]], data)
  )
}

#' @export
module_input_srv.picks <- function(id, spec, data) {
  moduleServer(id, function(input, output, session) {
    attr(spec, ".callback") <- reactiveVal(NULL) # callback to be used outside

    data_r <- shiny::reactive(if (shiny::is.reactive(data)) data() else data)
    spec_resolved <- shiny::reactiveVal(
      restoreValue(
        session$ns("selectors"),
        resolver(spec, shiny::isolate(data_r()))
      )
    )
    session$onBookmark(function(state) {
      logger::log_debug("module_input_srv@onBookmark: storing current selectors")
      state$values$selectors <- spec_resolved()
    })

    # join_keys are needed to variables after merge
    attr(spec_resolved, "join_keys") <- teal.data::join_keys(shiny::isolate(data_r())) # todo: do the same as with .callback

    badge <- shiny::reactive({
      tagList(
        lapply(
          spec_resolved(),
          function(x) {
            if (inherits(x, "values")) {
              if (!identical(as.vector(x$choices), as.vector(x$selected))) {
                bsicons::bs_icon("funnel")
              }
            } else if (length(x$selected)) {
              toString(x$selected)
            } else {
              "~"
            }
          }
        )
      )
    })

    # todo: modify when data changes
    output$summary <- shiny::renderUI(badge())

    Reduce(
      function(data, i) {
        slot_name <- names(spec)[i]
        resolved_on_data <- reactive(determine(x = spec[[i]], data = data()))
        selected <- .selected_choices_srv(
          id = is(spec[[slot_name]]),
          x = shiny::reactive(spec_resolved()[[slot_name]]),
          choices_range = reactive(resolved_on_data()$x$choices)
        )

        # this works as follows:
        #  Each observer is observes input$selected of i-th element of spec ($datasets, $variables, ...)
        #  When i-th select input changes then
        #   - spec_resolved containing current state is being unresolved but only after the i-th element as
        #     values are sequentially dependent. For example if variables (i=2) is selected we don't want
        #     to unresolve (restart) dataset.
        #   - new value (selected) is replacing old value in current slot (i)
        #   - we call resolve which resolves only "unresolved" (delayed) values
        #   - new spec is replacing reactiveValue
        # Thanks to this design reactive values are triggered only once
        shiny::observeEvent(
          selected(),
          ignoreInit = TRUE, # because spec_resolved is already resolved and `selected()` is being set
          ignoreNULL = FALSE,
          {
            if (isTRUE(all.equal(selected(), spec_resolved()[[slot_name]]$selected, tolerance = 1e-15))) {
              # tolerance 1e-15 is a max precision (significant digits) in widgets.
              return(NULL)
            }
            logger::log_info("module_input_server@1 selected has changed. Resolving downstream...")

            new_spec_unresolved <- spec
            # ↓ everything after `i` is to resolve
            new_spec_unresolved[seq_len(i)] <- spec_resolved()[seq_len(i)]
            new_spec_unresolved[[slot_name]]$selected <- selected()

            resolver_warnings <- character(0)
            new_spec_resolved <- withCallingHandlers(
              resolver(new_spec_unresolved, data_r()),
              warning = function(w) {
                resolver_warnings <<- paste(conditionMessage(w), collapse = " ")
              }
            )
            if (length(resolver_warnings)) {
              showNotification(resolver_warnings, type = "error")
            }

            spec_resolved(new_spec_resolved)
          }
        )

        reactive(resolved_on_data()$data)
      },
      x = seq_along(spec),
      init = data_r
    )

    spec_resolved
  })
}

.selected_choices_ui <- function(id, x) {
  ns <- shiny::NS(id)
  uiOutput(ns("selected_container"))
}

.selected_choices_srv <- function(id, x, choices_range) {
  checkmate::assert_string(id)
  checkmate::assert_true(is.reactive(x))
  shiny::moduleServer(id, function(input, output, session) {
    # todo: keep_order
    selected <- shiny::reactiveVal(isolate(x())$selected)
    output$selected_container <- renderUI({
      if (isTRUE(attr(x(), "fixed")) || length(choices_range()) == 1) {
      } else if (is.numeric(x()$choices)) {
        .selected_choices_ui_numeric(session$ns("range"), x = x, choices_range = choices_range)
      } else {
        # todo: provide information about data class in choices_range() so we can provide icons in the pickerInput
        .selected_choices_ui_categorical(session$ns("selected"), x = x, choices_range = choices_range)
      }
    })

    # for numeric
    range_debounced <- reactive(input$range) |> debounce(1000)
    shiny::observeEvent(range_debounced(), {
      if (length(input$range) != 2) {
        return(NULL)
      }
      if (!isTRUE(all.equal(input$range, selected(), tolerance = 1e-15))) {
        # tolerance 1e-15 is a max precision (significant digits) in widgets.
        logger::log_debug(".selected_choices_srv@2 input$selected has changed.")
        selected(input$range)
      }
    })

    .selected_choices_srv_categorical("selected", x = x, choices_range = choices_range)


    # for non-numeric
    shiny::observeEvent(input$selected_open, {
      if (!isTRUE(input$selection_open)) {
        # ↓ pickerInput returns "" when nothing selected. This can cause failure during col select (x[,""])
        new_selected <- if (length(input$selected) && !identical(input$selected, "")) as.vector(input$selected)
        if (!setequal(new_selected, selected())) {
          logger::log_debug(".selected_choices_srv@2 input$selected has changed.")
          selected(new_selected)
        }
      }
    })
    selected
  })
}


.selected_choices_ui_numeric <- function(id, x, choices_range) {
  shinyWidgets::numericRangeInput(
    inputId = id,
    label = paste("Select", is(x()), collapse = " "),
    min = unname(x()$choices[1]),
    max = unname(tail(x()$choices, 1)),
    value = unname(x()$selected)
  )
}



.selected_choices_ui_categorical <- function(id, x, choices_range) {
  missing_choices <- setdiff(x()$choices, choices_range())
  reordered_choices <- x()$choices[order(unname(x()$choices) %in% missing_choices)]

  htmltools::div(
    style = "max-width: 500px;",
    shinyWidgets::pickerInput(
      inputId = id,
      label = paste("Select", is(x()), collapse = " "),
      choices = reordered_choices,
      selected = x()$selected,
      multiple = attr(x(), "multiple"),
      choicesOpt = list(
        content = ifelse(
          # todo: add to the input choice icon = attached to choices when determine
          names(reordered_choices) == unname(reordered_choices),
          sprintf(
            "<span%s>%s</span>",
            ifelse(unname(reordered_choices) %in% missing_choices, ' style="opacity: 0.5;"', ""),
            reordered_choices
          ),
          sprintf(
            '<span%s>%s</span>&nbsp;<small class="text-muted">%s</small>',
            ifelse(unname(reordered_choices) %in% missing_choices, ' style="opacity: 0.5;"', ""),
            unname(reordered_choices),
            names(reordered_choices)
          )
        )
      ),
      options = list(
        "actions-box" = attr(x(), "multiple"),
        # "allow-clear" = attr(x(), "multiple") || attr(x(), "allow-clear"),
        "live-search" = ifelse(length(x()$choices) > 10, TRUE, FALSE),
        # "max-options" = attr(x(), "max-options"),
        "none-selected-text" = "- Nothing selected -",
        "show-subtext" = TRUE
      )
    )
  )
}

.selected_choices_srv_numeric <- function(id, x, choices_range) {

}

.selected_choices_srv_categorical <- function(id, x, choices_range) {
}

#' Restore value from bookmark.
#'
#' Get value from bookmark or return default.
#'
#' Bookmarks can store not only inputs but also arbitrary values.
#' These values are stored by `onBookmark` callbacks and restored by `onBookmarked` callbacks,
#' and they are placed in the `values` environment in the `session$restoreContext` field.
#' Using `teal_data_module` makes it impossible to run the callbacks
#' because the app becomes ready before modules execute and callbacks are registered.
#' In those cases the stored values can still be recovered from the `session` object directly.
#'
#' Note that variable names in the `values` environment are prefixed with module name space names,
#' therefore, when using this function in modules, `value` must be run through the name space function.
#'
#' @param value (`character(1)`) name of value to restore
#' @param default fallback value
#'
#' @return
#' In an application restored from a server-side bookmark,
#' the variable specified by `value` from the `values` environment.
#' Otherwise `default`.
#'
#' @keywords internal
#'
restoreValue <- function(value, default) { # nolint: object_name.
  checkmate::assert_character("value")
  session_default <- shiny::getDefaultReactiveDomain()
  session_parent <- .subset2(session_default, "parent")
  session <- if (is.null(session_parent)) session_default else session_parent

  if (isTRUE(session$restoreContext$active) && exists(value, session$restoreContext$values, inherits = FALSE)) {
    session$restoreContext$values[[value]]
  } else {
    default
  }
}
