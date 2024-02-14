#' Formatting data extracts
#'
#' Returns a human-readable string representation of an extracted `data_extract_spec` object.
#'
#' This function formats the output of [`data_extract_srv`].
#' See the example for more information.
#'
#' @param data_extract `list` the list output of `data_extract_srv`.

#' @return `character(1)` representation of the `data_extract` object.
#'
#' @examples
#' library(shiny)
#' simple_des <- data_extract_spec(
#'   dataname = "iris",
#'   filter = filter_spec(vars = "Petal.Length", choices = c("1.4", "1.5")),
#'   select = select_spec(choices = c("Petal.Length", "Species"))
#' )
#'
#' ui <- fluidPage(
#'   data_extract_ui(
#'     id = "extract",
#'     label = "data extract ui",
#'     data_extract_spec = simple_des,
#'     is_single_dataset = TRUE
#'   ),
#'   verbatimTextOutput("formatted_extract")
#' )
#' server <- function(input, output, session) {
#'   extracted_input <- data_extract_srv(
#'     id = "extract",
#'     datasets = list(iris = iris),
#'     data_extract_spec = simple_des
#'   )
#'   output$formatted_extract <- renderPrint({
#'     cat(format_data_extract(extracted_input()))
#'   })
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#' @export
#'
format_data_extract <- function(data_extract) {
  if (is.null(data_extract)) {
    return(NULL)
  }

  checkmate::assert_list(data_extract)
  required_names <- c("select", "filters", "dataname")
  if (!checkmate::test_subset(required_names, choices = names(data_extract))) {
    stop(sprintf("data_extract must be a named list with names: %s", paste0(required_names, collapse = " ")))
  }

  out <- sprintf("<Data Extract for dataset: %s>", data_extract$dataname)
  out <- c(out, "Filters:")
  for (filter in data_extract$filters) {
    filtering_columns <- paste0(filter$columns, collapse = " ")
    selected_values <- paste0(filter$selected, collapse = " ")
    out <- c(out, sprintf("  Columns: %s Selected: %s", filtering_columns, selected_values))
  }

  out <- c(out, "Selected columns:")
  selected_columns <- paste0(data_extract$select, collapse = " ")
  out <- c(out, sprintf("  %s", selected_columns))

  paste0(out, collapse = "\n")
}
