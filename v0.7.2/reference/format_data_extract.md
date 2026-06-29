# Formatting data extracts

Returns a human-readable string representation of an extracted
`data_extract_spec` object.

## Usage

``` r
format_data_extract(data_extract)
```

## Arguments

- data_extract:

  `list` the list output of `data_extract_srv`.

## Value

`character(1)` representation of the `data_extract` object.

## Details

This function formats the output of
[`data_extract_srv`](https://insightsengineering.github.io/teal.transform/reference/data_extract_srv.md).
See the example for more information.

## Examples

``` r
library(shiny)

simple_des <- data_extract_spec(
  dataname = "iris",
  filter = filter_spec(vars = "Petal.Length", choices = c("1.4", "1.5")),
  select = select_spec(choices = c("Petal.Length", "Species"))
)

ui <- bslib::page_fluid(
  data_extract_ui(
    id = "extract",
    label = "data extract ui",
    data_extract_spec = simple_des,
    is_single_dataset = TRUE
  ),
  verbatimTextOutput("formatted_extract")
)
server <- function(input, output, session) {
  extracted_input <- data_extract_srv(
    id = "extract",
    datasets = list(iris = iris),
    data_extract_spec = simple_des
  )
  output$formatted_extract <- renderPrint({
    cat(format_data_extract(extracted_input()))
  })
}

if (interactive()) {
  shinyApp(ui, server)
}
```
