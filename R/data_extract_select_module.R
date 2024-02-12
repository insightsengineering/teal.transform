#' Returns a `shiny.tag.list` object with the UI for a `select_spec` object
#'
#' @param select (`select_spec`) A definition of a select spec element.
#' Setting [select_spec()] with `ordered = TRUE` makes this selector responsive
#' to the variable selection order.
#' @param id (`character(1)`) The shiny `inputId` of the element.
#'
#' @return `shiny.tag.list` with the UI.
#'
#' @keywords internal
#'
data_extract_select_ui <- function(select, id = "select") {
  checkmate::assert_class(select, "select_spec")
  checkmate::assert_string(id)

  ## select input
  res <- list(
    teal.widgets::optionalSelectInput(
      inputId = id,
      label = select$label,
      choices = `if`(inherits(select, "delayed_select_spec"), NULL, select$choices),
      selected = `if`(inherits(select, "delayed_select_spec"), NULL, select$selected),
      multiple = select$multiple,
      fixed = select$fixed
    )
  )

  if (!is.null(select$always_selected)) {
    res <- append(
      res,
      list(
        shinyjs::hidden(
          selectInput(
            inputId = paste0(id, "_additional"),
            label = "",
            choices = select$always_selected,
            selected = select$always_selected,
            multiple = length(select$always_selected) > 1
          )
        ),
        helpText(
          "Default Column(s)",
          tags$code(paste(select$always_selected, collapse = " "))
        )
      )
    )
  }

  do.call("tagList", res)
}
