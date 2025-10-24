#' Merge module
#'
#' @param picks (`list` of `picks`)
#' Example module
tm_merge <- function(label = "merge-module", picks, transformators = list()) {
  # todo: move to vignette
  module(
    label = label,
    ui = function(id, picks) {
      ns <- NS(id)
      tags$div(
        tags$div(
          class = "row g-2",
          lapply(names(picks), function(id) {
            tags$div(
              class = "col-auto",
              tags$strong(tags$label(id)),
              teal.transform::picks_ui(
                id = ns(id),
                picks = picks[[id]]
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
    server = function(id, data, picks) {
      moduleServer(id, function(input, output, session) {
        selectors <- picks_srv(id, picks = picks, data = data)

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
    ui_args = list(picks = picks),
    server_args = list(picks = picks),
    transformators = transformators
  )
}
