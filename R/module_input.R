helper_input <- function(id,
                         label,
                         multiple = FALSE) {
  shiny::selectInput(
    id,
    label,
    choices = NULL,
    selected = NULL,
    multiple = multiple
  )
}

#' @export
module_input_ui <- function(id, label, spec) {
  ns <- NS(id)
  input <- tagList(
    a(label),
  )

  if (valid_specification(spec)) {
    stop("Unexpected object used as specification.")
  }

  l <- lapply(spec, function(x) {
    helper_input(ns(is(x)),
      paste("Select", is(x), collapse = " "),
      multiple = is(x) != "datasets"
    )
  })
  input <- tagList(input, l)
}

#' @export
module_input_server <- function(id, spec, data) {
  stopifnot(is.specification(spec))
  stopifnot(is.character(id))
  moduleServer(id, function(input, output, session) {
    react_updates <- reactive({
      if (is.reactive(data)) {
        d <- data()
      } else {
        d <- data
      }
      if (!anyNA(spec) && is.delayed(spec)) {
        spec <- resolver(spec, d)
      }
      for (i in seq_along(names(input))) {
        variable <- names(input)[i]
        x <- input[[variable]]
        spec_v <- spec[[variable]]
        # resolved <- !is.character(spec_v$names) && all(x %in% spec_v$names) && any(!x %in% spec_v$select)

        if (!is.null(x) && any(nzchar(x))) {
          spec <- resolver(update_spec(spec, variable, x), d)
        } else {
          spec <- resolver(spec, d)
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
          choices = unorig(spec[[variable]]$choices),
          selected = unorig(spec[[variable]]$selected)
        )
        # FIXME: set on gray the input
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
        selection[[variable]] <- unorig(spec[[variable]]$selected)
      }
      selection
    })
  })
}
