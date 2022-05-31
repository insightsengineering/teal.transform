simple_des <- teal.transform::data_extract_spec(
  dataname = "iris",
  filter = teal.transform::filter_spec(vars = "Petal.Length", choices = c("1.4", "1.5")),
  select = teal.transform::select_spec(choices = c("Petal.Length", "Species"))
)

sample_filtered_data <- {
  data <- teal.data::teal_data(teal.data::dataset("iris", iris))
  datasets <- teal.slice:::filtered_data_new(data)
  teal.slice:::filtered_data_set(data, datasets)
  datasets
}

ui <- shiny::fluidPage(
  teal.transform::data_extract_ui(id = "des", label = "test des ui", data_extract_spec = simple_des),
  shiny::verbatimTextOutput(outputId = "formatted_des"),
)
srv <- function(input, output, session) {
  extracted_des <- teal.transform::data_extract_srv(
    id = "des", datasets = sample_filtered_data, data_extract_spec = simple_des
  )
  output$formatted_des <- shiny::renderPrint(cat(teal.transform::format_data_extract(extracted_des())))
}

shiny::shinyApp(ui, srv)
