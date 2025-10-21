#' Merge module
#'
#' Example module
tm_merge <- function(label = "merge-module", inputs, transformators = list()) {
  # todo: move to vignette
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
              teal.transform::picks_ui(
                id = ns(id),
                spec = inputs[[id]]
              )
            )
          })
        ),
        shiny::div(
          reactable::reactableOutput(ns("table_merged")),
          shiny::verbatimTextOutput(ns("join_keys")),
          shiny::verbatimTextOutput(ns("mapped")),
          shiny::verbatimTextOutput(ns("src"))
        )
      )
    },
    server = function(id, data, inputs) {
      moduleServer(id, function(input, output, session) {
        selectors <- picks_srv(id, spec = inputs, data = data)

        merged <- merge_srv("merge", data = data, selectors = selectors)

        table_q <- reactive({
          req(merged$data())
          within(merged$data(), reactable::reactable(anl), selectors = selectors)
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

        output$mapped <- renderText(yaml::as.yaml(merged$variables()))

        output$join_keys <- renderPrint(teal.data::join_keys(merged$data()))
      })
    },
    ui_args = list(inputs = inputs),
    server_args = list(inputs = inputs),
    transformators = transformators
  )
}
