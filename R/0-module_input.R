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
  content <- lapply(spec, function(x) .selected_choices_ui(id = ns(is(x))))
  htmltools::tags$div(
    # todo: spec to have a label attribute
    # todo: badge to have css attribute to control the size - make CSS rule - can be controlled globally and module-ly
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
    data_r <- shiny::reactive(if (shiny::is.reactive(data)) data() else data)
    spec_resolved <- shiny::reactiveVal(
      restoreValue(
        session$ns("picks"),
        resolver(spec, shiny::isolate(data_r()))
      )
    )
    session$onBookmark(function(state) {
      logger::log_debug("module_input_srv@onBookmark: storing current picks")
      state$values$picks <- spec_resolved()
    })

    # join_keys are needed to variables after merge
    attr(spec_resolved, "join_keys") <- teal.data::join_keys(shiny::isolate(data_r()))

    badge <- shiny::reactive({
      lapply(
        spec_resolved(),
        function(x) {
          label <- if (length(x$selected)) {
            toString(x$selected)
          } else {
            "~"
          }
          label
        }
      )
    })

    # todo: modify when data changes
    output$summary <- shiny::renderUI(tagList(badge()))

    Reduce(
      function(data, i) {
        choices <- reactiveVal(isolate(spec_resolved())[[i]]$choices)
        selected <- reactiveVal(isolate(spec_resolved())[[i]]$selected)
        all_choices <- reactive(determine(x = spec[[i]], data = data())$x$choices)

        observeEvent(all_choices(), ignoreInit = TRUE, {
          current_choices <- spec_resolved()[[i]]$choices
          current_selected <- spec_resolved()[[i]]$selected
          .update_rv(
            selected, .intersect(current_selected, all_choices()),
            sprintf("module_input_srv@1 %s$%s$selected is outside of the possible choices", id, names(spec)[i])
          )
          .update_rv(
            choices, all_choices(),
            sprintf("module_input_srv@1 %s$%s$choices is outside of the possible choices", id, names(spec)[i])
          )
        })

        observeEvent(spec_resolved()[[i]], ignoreInit = TRUE, {
          .update_rv(choices, spec_resolved()[[i]]$choices, log = "module_input_srv@1 update input choices")
          .update_rv(selected, spec_resolved()[[i]]$selected, log = "module_input_srv@1 update input selected")
        })

        args <- attributes(spec[[i]])
        .selected_choices_srv(
          id = is(spec[[i]]),
          type = is(spec[[i]]),
          choices = choices,
          selected = selected,
          args = args[!names(args) %in% c("names", "class")]
        )

        # this works as follows:
        #  Each observer is observes input$selected of i-th element of spec ($datasets, $variables, ...)
        shiny::observeEvent(
          selected(),
          ignoreInit = TRUE, # because spec_resolved is already resolved and `selected()` is being set
          ignoreNULL = FALSE, # because input$selected can be empty
          .resolve(selected(), slot_idx = i, spec_resolved = spec_resolved, old_spec = spec, data = data_r())
        )

        reactive(.extract(x = isolate(spec_resolved()[[i]]), data()))
      },
      x = seq_along(spec),
      init = data_r
    )

    spec_resolved
  })
}

.selected_choices_ui <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("selected_container"))
}

.selected_choices_srv <- function(id, type, choices, selected, args) {
  checkmate::assert_string(id)
  checkmate::assert_class(choices, "reactiveVal")
  checkmate::assert_class(selected, "reactiveVal")
  checkmate::assert_list(args)

  shiny::moduleServer(id, function(input, output, session) {
    # todo: keep_order
    output$selected_container <- renderUI({
      logger::log_debug(".selected_choices_srv@1 rerender input")
      if (isTRUE(args$fixed) || length(choices()) == 1) {

      } else if (is.numeric(choices())) {
        .selected_choices_ui_numeric(
          session$ns("range"),
          label = sprintf("Select %s range:", type),
          choices = choices(),
          selected = selected(),
          args = args
        )
      } else {
        # todo: provide information about data class so we can provide icons in the pickerInput
        .selected_choices_ui_categorical(
          session$ns("selected"),
          label = sprintf("Select %s:", type),
          choices = choices(),
          selected = selected(),
          args = args
        )
      }
    })

    # for numeric
    range_debounced <- reactive(input$range) |> debounce(1000)
    shiny::observeEvent(range_debounced(), {
      if (length(input$range) != 2) {
        return(NULL)
      }
      .update_rv(selected, input$range, log = ".selected_choices_srv@2 update selected after input changed")
    })

    # for non-numeric
    shiny::observeEvent(input$selected_open, {
      if (!isTRUE(input$selection_open)) {
        # ↓ pickerInput returns "" when nothing selected. This can cause failure during col select (x[,""])
        new_selected <- if (length(input$selected) && !identical(input$selected, "")) as.vector(input$selected)
        .update_rv(selected, new_selected, log = ".selected_choices_srv@1 update selected after input changed")
      }
    })
    selected
  })
}

.selected_choices_ui_numeric <- function(id, label, choices, selected, args) {
  shinyWidgets::numericRangeInput(
    inputId = id,
    label = label,
    min = unname(choices[1]),
    max = unname(tail(choices, 1)),
    value = unname(selected)
  )
}

.selected_choices_ui_categorical <- function(id, label, choices, selected, args) {
  htmltools::div(
    style = "max-width: 500px;",
    shinyWidgets::pickerInput(
      inputId = id,
      label = label,
      choices = choices,
      selected = selected,
      multiple = args$multiple,
      choicesOpt = list(
        content = ifelse(
          # todo: add to the input choice icon = attached to choices when determine
          names(choices) == unname(choices),
          sprintf("<span>%s</span>", choices),
          sprintf(
            '<span>%s</span>&nbsp;<small class="text-muted">%s</small>',
            unname(choices),
            names(choices)
          )
        )
      ),
      options = list(
        "actions-box" = args$multiple,
        # "allow-clear" = args$multiple || args$`allow-clear`,
        "live-search" = length(choices) > 10,
        # "max-options" = args$`max-options`,
        "none-selected-text" = "- Nothing selected -",
        "show-subtext" = TRUE
      )
    )
  )
}

.update_rv <- function(rv, value, log) {
  if (!isTRUE(all.equal(rv(), value, tolerance = 1e-15))) { # tolerance 1e-15 is a max precision in widgets.
    logger::log_debug(log)
    rv(value)
  }
}

#' Resolve downstream after selected changes
#'
#'  @description
#'  When i-th select input changes then
#'   - spec_resolved containing current state is being unresolved but only after the i-th element as
#'     values are sequentially dependent. For example if variables (i=2) is selected we don't want
#'     to unresolve (restart) dataset.
#'   - new value (selected) is replacing old value in current slot (i)
#'   - we call resolve which resolves only "unresolved" (delayed) values
#'   - new spec is replacing reactiveValue
#' Thanks to this design reactive values are triggered only once
#' @param selected (`vector`) rather `character`, or `factor`. `numeric(2)` for `values()` based on numeric column.
#' @param slot_idx (`integer`)
#' @param spec_resolved (`reactiveVal`)
#' @param old_spec (`picks`)
#' @param data (`any` asserted further in `resolver`)
#' @keywords internal
.resolve <- function(selected, slot_idx, spec_resolved, old_spec, data) {
  checkmate::assert_vector(selected, null.ok = TRUE)
  checkmate::assert_integerish(slot_idx, lower = 1)
  checkmate::assert_class(spec_resolved, "reactiveVal")
  checkmate::assert_class(old_spec, "picks")

  if (isTRUE(all.equal(selected, spec_resolved()[[slot_idx]]$selected, tolerance = 1e-15))) {
    return(NULL)
  }
  logger::log_info("module_input_server@1 selected has changed. Resolving downstream...")

  new_spec_unresolved <- old_spec
  # ↓ everything after `slot_idx` is to resolve
  new_spec_unresolved[seq_len(slot_idx - 1)] <- spec_resolved()[seq_len(slot_idx - 1)]
  new_spec_unresolved[[slot_idx]]$selected <- selected

  resolver_warnings <- character(0)
  new_spec_resolved <- withCallingHandlers(
    resolver(new_spec_unresolved, data),
    warning = function(w) {
      resolver_warnings <<- paste(conditionMessage(w), collapse = " ")
    }
  )
  if (length(resolver_warnings)) {
    showNotification(resolver_warnings, type = "error")
  }

  spec_resolved(new_spec_resolved)
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

.intersect <- function(x, y) {
  if (is.numeric(x) && is.numeric(y)) {
    c(
      max(x[1], y[1], na.rm = TRUE),
      min(x[2], y[2], na.rm = TRUE)
    )
  } else {
    intersect(x, y)
  }
}
