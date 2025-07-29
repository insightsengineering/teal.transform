#' @export
module_input_ui <- function(id, spec) {
  ns <- shiny::NS(id)
  if (.valid_specification(spec)) {
    stop("Unexpected object used as specification.")
  }
  badge_label <- shiny::textOutput(ns("summary"), container = tags$span)
  content <- lapply(spec, function(x) selected_choices_ui(id = ns(is(x)), x))
  tags$div(
    # todo: spec to have a label attribute
    .badge_dropdown(ns("inputs"), label = badge_label, content = content)
  )
}

#' @export
module_input_srv <- function(id, spec, data) {
  checkmate::assert_string(id)
  checkmate::assert_true(.is.specification(spec))
  checkmate::assert_class(data, "reactive")
  moduleServer(id, function(input, output, session) {
    data_r <- shiny::reactive(if (shiny::is.reactive(data)) data() else data)
    spec_r <- shiny::reactiveVal(resolver(spec, shiny::isolate(data_r())))

    badge_text <- shiny::reactive({
      paste(
        lapply(spec_r(), function(x) toString(x$selected)),
        collapse = ": "
      )
    })

    # todo: modify when data changes
    output$summary <- shiny::renderText(badge_text())

    lapply(seq_along(spec), function(i) {
      selected <- selected_choices_srv(
        id = is(spec[[i]]),
        x = shiny::reactive(spec_r()[[i]])
      )
      shiny::observeEvent(
        selected(),
        ignoreInit = TRUE, # because spec_r is a initial state
        {
          logger::log_info("module_input_server@1 selected has changed. Resolving downstream...")
          new_spec_unresolved <- spec
          new_spec_unresolved[[i]]$selected <- selected()
          if (i > 1) {
            # we don't want to resolve previous items
            new_spec_unresolved[seq_len(i - 1)] <- spec_r()[seq_len(i - 1)]
          }
          new_spec <- resolver(new_spec_unresolved, data_r())
          if (!identical(new_spec, spec_r())) {
            logger::log_info("Update spec { names(spec)[i] } after selection change.")
            spec_r(new_spec)
          }
        }
      )
    })

    spec_r
  })
}

selected_choices_ui <- function(id, x) {
  ns <- shiny::NS(id)
  shiny::selectInput(
    inputId = ns("selected"),
    label = paste("Select", is(x), collapse = " "),
    choices = if (!is.delayed(x$choices)) x$choices,
    selected = if (!is.delayed(x$selected)) x$selected,
    multiple = isTRUE(x$multiple)
  )
}

selected_choices_srv <- function(id, x) {
  checkmate::assert_string(id)
  checkmate::assert_true(is.reactive(x))
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(x(), {
      logger::log_debug("selected_choices_srv@1 x has changed (caused by upstream resolve)")
      shiny::updateSelectInput(
        inputId = "selected",
        choices = x()$choices,
        selected = x()$selected
      )
    })
    selected <- shiny::reactiveVal()
    # todo: if only one choice then replace with the text only
    shiny::observeEvent(input$selected, {
      if (!identical(input$selected, selected)) {
        logger::log_debug("selected_choices_srv@2 input$selected has changed.")
        selected(input$selected)
      }
    })
    selected
  })
}

.badge_dropdown <- function(id, label, content) {
  ns <- shiny::NS(id)
  htmltools::tags$div(
    htmltools::tags$span(
      label,
      id = ns("summary_badge"),
      class = "badge bg-primary",
      style = "cursor: pointer; user-select: none;",
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
}
