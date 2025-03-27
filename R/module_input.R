
helper_input <- function(id,
                         label,
                         multiple = FALSE) {
  shiny::selectInput(
    id,
    label,
    choices = NULL,
    selected = NULL,
    multiple = multiple)
}

module_input_ui <- function(id, label, spec) {
  ns <- NS(id)
  input <- tagList(
    a(label),
  )
  l <- lapply(spec, function(x) {
    helper_input(ns(is(x)),
                 paste("Select", is(x), collapse = " "),
                 multiple = is(x) != "datasets")
  })
  input <- tagList(input, l)
}

module_input_server <- function(id, spec, data) {
  moduleServer(id, function(input, output, session) {

    react_updates <- reactive({
      if (!anyNA(spec) && is.delayed(spec)) {
        spec <- teal.transform::resolver(spec, data())
      }
      for (i in seq_along(input)) {
        variable <- names(input)[i]
        x <- input[[variable]]
        spec_v <- spec[[variable]]
        # a <- !is.null(x) && all(x %in% $names)
        # browser(expr = !isFALSE(a) && !isTRUE(a))
        if  (!is.null(x) && all(x %in% spec_v$names) && any(!x %in% spec_v$select)) {
          spec <- spec |>
            update_spec(variable, input[[variable]]) |>
            teal.transform::resolver(data())
        }
      }
      spec
    })

    observe({
      req(react_updates())
      spec <- react_updates()
      for (i in seq_along(spec)) {
        variable <- names(spec)[i]

        # Relies on order of arguments
        if (is.delayed(spec[[variable]])) {
          break
        }
        shiny::updateSelectInput(
          session,
          variable,
          choices = unorig(spec[[variable]]$names),
          selected = unorig(spec[[variable]]$select)
        )
        # FIXME set on gray the input
        # FIXME: Hide input field if any type on specification cannot be solved
      }
    })


    # Full selection ####
    react_selection <- reactive({
      spec <- req(react_updates())
      req(!is.delayed(spec))
      selection <- vector("list", length(spec))
      names(selection) <- names(spec)
      for (i in seq_along(spec)) {
        variable <- names(spec)[i]
        selection[[variable]] <- unorig(spec[[variable]]$select)
      }
      selection
    })
  })
}


