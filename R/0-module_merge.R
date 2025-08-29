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
        output$mapped <- renderText(yaml::as.yaml(map_merged(selectors)))
      })
    },
    ui_args = list(inputs = inputs),
    server_args = list(inputs = inputs),
    transformators = transformators
  )
}
