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
module_input_ui.specification <- function(id, spec) {
  if (.valid_specification(spec)) {
    stop("Unexpected object used as specification.")
  }
  ns <- shiny::NS(id)
  badge_label <- shiny::textOutput(ns("summary"), container = htmltools::tags$span)
  # todo: icon or color to indicate a column class
  content <- lapply(spec, function(x) .selected_choices_ui(id = ns(is(x)), x))
  htmltools::tags$div(
    # todo: spec to have a label attribute
    .badge_dropdown(ns("inputs"), label = badge_label, content = content)
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
module_input_srv.specification <- function(id, spec, data) {
  moduleServer(id, function(input, output, session) {
    attr(spec, ".callback") <- reactiveVal(NULL) # callback to be used outside

    data_r <- shiny::reactive(if (shiny::is.reactive(data)) data() else data)
    # todo: bookmarking (decide whether it should be built on reactiveVal or setting inputs)
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

    badge_text <- shiny::reactive({
      paste(
        lapply(
          spec_resolved(),
          function(x) {
            if (length(x$selected)) {
              toString(x$selected)
            } else {
              "~"
            }
          }
        ),
        collapse = ": "
      )
    })

    # todo: modify when data changes
    output$summary <- shiny::renderText(badge_text())

    lapply(seq_along(spec), function(i) {
      slot_name <- names(spec)[i]
      selected <- .selected_choices_srv(
        id = is(spec[[slot_name]]),
        x = shiny::reactive(spec_resolved()[[slot_name]])
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
        ignoreInit = TRUE, # because spec_resolved is a initial state
        {
          logger::log_info("module_input_server@1 selected has changed. Resolving downstream...")
          new_spec_unresolved <- spec
          # ↓ everything after `i` is to resolve
          new_spec_unresolved[seq_len(i)] <- spec_resolved()[seq_len(i)]
          new_spec_unresolved[[slot_name]]$selected <- selected()

          resolver_warnings <- character()
          new_spec_resolved <- withCallingHandlers(
            resolver(new_spec_unresolved, data_r()),
            warning = function(w) {
              resolver_warnings <<- paste(conditionMessage(w), collapse = " ")
            }
          )
          if (length(resolver_warnings)) {
            showNotification(resolver_warnings, type = "error")
          }
          if (!identical(new_spec_resolved, spec_resolved())) {
            logger::log_info("Update spec { slot_name } after selection change.")
            spec_resolved(new_spec_resolved)
          }
        }
      )
    })

    spec_resolved
  })
}

.selected_choices_ui <- function(id, x) {
  ns <- shiny::NS(id)
  shinyWidgets::pickerInput(
    inputId = ns("selected"),
    label = paste("Select", is(x), collapse = " "),
    choices = if (is.character(x$choices)) x$choices,
    selected = if (is.character(x$selected)) x$selected,
    multiple = isTRUE(attr(x, "multiple")),
    choicesOpt = if (is.character(x$choices)) list(content = toupper(x$choices)),
    options = list(
      "actions-box" = isTRUE(attr(x, "multiple")),
      "none-selected-text" = "- Nothing selected -",
      "allow-clear" = !isTRUE(attr(x, "multiple")),
      "max-options" = ifelse(isTRUE(attr(x, "multiple")), Inf, 1),
      "show-subtext" = TRUE
    )
  )
}

.selected_choices_srv <- function(id, x) {
  checkmate::assert_string(id)
  checkmate::assert_true(is.reactive(x))
  shiny::moduleServer(id, function(input, output, session) {
    # todo: keep_order
    shiny::observeEvent(x(), {
      logger::log_debug(".selected_choices_srv@1 x has changed (caused by upstream resolve)")
      if (length(x()$choices) == 1) {
        shinyjs::hide("selected")
      }


      # todo: add to the input choice icon = attached to choices when determine
      content <- ifelse(
        names(x()$choices) == unname(x()$choices),
        sprintf("<span>%s</span>", x()$choices),
        sprintf(
          '<span>%s</span>&nbsp;<small class="text-muted">%s</small>',
          unname(x()$choices),
          names(x()$choices)
        )
      )

      shinyWidgets::updatePickerInput(
        inputId = "selected",
        choices = x()$choices,
        selected = x()$selected,
        choicesOpt = list(content = content),
        options = list(
          "live-search" = ifelse(length(x()$choices) > 10, TRUE, FALSE)
        )
      )
    })
    selected <- shiny::reactiveVal()
    # todo: if only one choice then replace with the text only
    shiny::observeEvent(input$selected, {
      if (!identical(input$selected, selected)) {
        logger::log_debug(".selected_choices_srv@2 input$selected has changed.")
        selected(input$selected)
      }
    })
    selected
  })
}

.badge_dropdown <- function(id, label, content) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    htmltools::tags$style(".choices-selected-badge-dropdown:has(~ div .shiny-validation-message) {
      border-color: red !important;
    }"),
    htmltools::tags$div(
      htmltools::tags$span(
        label,
        id = ns("summary_badge"),
        class = "badge bg-primary choices-selected-badge-dropdown",
        style = "cursor: pointer; user-select: none; border: 1px solid transparent;",
        onclick = sprintf(
          "
          var container = document.getElementById('%s');
          var summary = document.getElementById('%s');

          if(container.style.display === 'none' || container.style.display === '') {
            container.style.display = 'block';

            // Add click outside handler
            setTimeout(function() {
              function handleClickOutside(event) {
                if (!container.contains(event.target) && !summary.contains(event.target)) {
                  container.style.display = 'none';
                  document.removeEventListener('click', handleClickOutside);
                }
              }
              document.addEventListener('click', handleClickOutside);
            }, 10);
          }
        ",
          ns("inputs_container"),
          ns("summary_badge")
        )
      ),
      htmltools::tags$div(
        content,
        id = ns("inputs_container"),
        style = "display: none; position: absolute; background: white; border: 1px solid #ccc; border-radius: 4px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); padding: 10px; z-index: 1000; min-width: 200px;",
      )
    )
  )
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
