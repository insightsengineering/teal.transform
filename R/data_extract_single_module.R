#' Returns a `shiny.tag` with the UI elements for a `data_extract_spec`
#'
#' @details
#' Creates a `shiny.tag` element defining the UI elements corresponding to a
#' single `data_extract_spec` object.
#'
#' @param id (`character(1)`) the id of the module.
#' @param single_data_extract_spec (`data_extract_spec`) the
#' [data_extract_spec()] object to handle.
#'
#' @return `shiny.tag` the HTML element defining the UI.
#'
#' @keywords internal
#'
data_extract_single_ui <- function(id = NULL, single_data_extract_spec) {
  stopifnot(inherits(single_data_extract_spec, "data_extract_spec"))
  ns <- NS(id)

  ## filter input
  extract_spec_filter <- single_data_extract_spec$filter
  filter_display <- do.call(
    div,
    lapply(
      seq_along(extract_spec_filter),
      function(idx) {
        x <- extract_spec_filter[[idx]]
        if (inherits(x, "filter_spec")) {
          data_extract_filter_ui(filter = x, id = ns(paste0("filter", idx)))
        } else {
          stop("Unsupported object class")
        }
      }
    )
  )

  ## select input
  extract_spec_select <- single_data_extract_spec$select
  if (!is.null(extract_spec_select$fixed)) {
    attr(extract_spec_select$fixed, which = "dataname") <- single_data_extract_spec$dataname
  }

  select_display <- if (is.null(extract_spec_select)) {
    NULL
  } else {
    data_extract_select_ui(extract_spec_select, id = ns("select"))
  }

  ## reshape input
  extract_spec_reshape <- single_data_extract_spec$reshape
  reshape_display <- checkboxInput(
    inputId = ns("reshape"),
    label = "Reshape long to wide format",
    value = extract_spec_reshape
  )
  # always disable reshape button and hide if it is not pre-configured
  reshape_display <- shinyjs::disabled(reshape_display)
  if (!extract_spec_reshape) reshape_display <- shinyjs::hidden(reshape_display)

  ## all combined
  div(filter_display, select_display, reshape_display)
}

#' The server function for a single `data_extract_spec` object
#'
#' @details
#' The Shiny server function for handling a single [data_extract_spec] object.
#'
#' @inheritParams data_extract_filter_srv
#' @inheritParams data_extract_single_ui
#'
#' @return `NULL`.
#'
#' @keywords internal
#'
data_extract_single_srv <- function(id, datasets, single_data_extract_spec) {
  moduleServer(
    id,
    function(input, output, session) {
      logger::log_trace("data_extract_single_srv initialized with dataset: { single_data_extract_spec$dataname }.")

      # ui could be initialized with a delayed select spec so the choices and selected are NULL
      # here delayed are resolved
      isolate({
        resolved <- resolve_delayed(single_data_extract_spec, datasets)
        teal.widgets::updateOptionalSelectInput(
          session = session,
          inputId = "select",
          choices = resolved$select$choices,
          selected = resolved$select$selected
        )
      })

      for (idx in seq_along(resolved$filter)) {
        x <- resolved$filter[[idx]]
        if (inherits(x, "filter_spec")) {
          data_extract_filter_srv(
            id = paste0("filter", idx),
            datasets = datasets,
            filter = x
          )
        }
        NULL
      }
    }
  )
}
