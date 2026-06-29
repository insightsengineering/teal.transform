# Function to compose `validators` from `data_extract_multiple_srv`

This function takes the output from `data_extract_multiple_srv` and
collates the
[`shinyvalidate::InputValidator`](https://rstudio.github.io/shinyvalidate/reference/InputValidator.html)s
returned into a single `validator` and enables this.

## Usage

``` r
compose_and_enable_validators(iv, selector_list, validator_names = NULL)
```

## Arguments

- iv:

  ([`shinyvalidate::InputValidator`](https://rstudio.github.io/shinyvalidate/reference/InputValidator.html))
  A `validator`.

- selector_list:

  (`reactive` named list of `reactives`). Typically this is the output
  from `data_extract_multiple_srv`. The `validators` in this list
  (specifically `selector_list()[[validator_names]]()iv`) will be added
  into `iv`.

- validator_names:

  (`character` or `NULL`). If `character` then only `validators` in the
  elements of `selector_list()` whose name is in this list will be
  added. If `NULL` all `validators` will be added

## Value

([`shinyvalidate::InputValidator`](https://rstudio.github.io/shinyvalidate/reference/InputValidator.html))
enabled `iv` with appropriate `validators` added into it.

## Examples

``` r
library(shiny)
library(shinyvalidate)
library(shinyjs)
#> 
#> Attaching package: ‘shinyjs’
#> The following object is masked from ‘package:shiny’:
#> 
#>     runExample
#> The following object is masked from ‘package:teal.data’:
#> 
#>     show
#> The following object is masked from ‘package:teal.code’:
#> 
#>     show
#> The following objects are masked from ‘package:methods’:
#> 
#>     removeClass, show
library(teal.widgets)

iris_extract <- data_extract_spec(
  dataname = "iris",
  select = select_spec(
    label = "Select variable:",
    choices = variable_choices(iris, colnames(iris)),
    selected = "Sepal.Length",
    multiple = TRUE,
    fixed = FALSE
  )
)

data_list <- list(iris = reactive(iris))

ui <- bslib::page_fluid(
  useShinyjs(),
  bslib::layout_sidebar(
    verbatimTextOutput("out1"),
    sidebar = tagList(
      data_extract_ui(
        id = "x_var",
        label = "Please select an X column",
        data_extract_spec = iris_extract
      ),
      data_extract_ui(
        id = "y_var",
        label = "Please select a Y column",
        data_extract_spec = iris_extract
      ),
      data_extract_ui(
        id = "col_var",
        label = "Please select a color column",
        data_extract_spec = iris_extract
      )
    )
  )
)

server <- function(input, output, session) {
  selector_list <- data_extract_multiple_srv(
    list(x_var = iris_extract, y_var = iris_extract, col_var = iris_extract),
    datasets = data_list,
    select_validation_rule = list(
      x_var = sv_required("Please select an X column"),
      y_var = compose_rules(
        sv_required("Exactly 2 'Y' column variables must be chosen"),
        function(x) if (length(x) != 2) "Exactly 2 'Y' column variables must be chosen"
      )
    )
  )
  iv_r <- reactive({
    iv <- InputValidator$new()
    compose_and_enable_validators(
      iv,
      selector_list,
      # if validator_names = NULL then all validators are used
      # to turn on only "x_var" then set this argument to "x_var"
      validator_names = NULL
    )
  })

  output$out1 <- renderPrint({
    if (iv_r()$is_valid()) {
      ans <- lapply(selector_list(), function(x) {
        cat(format_data_extract(x()), "\n\n")
      })
    } else {
      "Check that you have made a valid selection"
    }
  })
}

if (interactive()) {
  shinyApp(ui, server)
}
```
