# Creates a named list of `data_extract_srv` output

`data_extract_multiple_srv` loops over the list of `data_extract` given
and runs `data_extract_srv` for each one returning a list of reactive
objects.

## Usage

``` r
data_extract_multiple_srv(data_extract, datasets, ...)

# S3 method for class 'reactive'
data_extract_multiple_srv(data_extract, datasets, ...)

# S3 method for class 'FilteredData'
data_extract_multiple_srv(data_extract, datasets, ...)

# S3 method for class 'list'
data_extract_multiple_srv(
  data_extract,
  datasets,
  join_keys = NULL,
  select_validation_rule = NULL,
  filter_validation_rule = NULL,
  dataset_validation_rule = if (is.null(select_validation_rule) &&
    is.null(filter_validation_rule)) {
     NULL
 } else {
    
    shinyvalidate::sv_required("Please select a dataset")
 },
  ...
)
```

## Arguments

- data_extract:

  (named `list` of `data_extract_spec` objects) the list
  `data_extract_spec` objects. The names of the elements in the list
  need to correspond to the `ids` passed to `data_extract_ui`.

  See example for details.

- datasets:

  (`FilteredData` or `list` of `reactive` or non-`reactive`
  `data.frame`) object containing data either in the form of
  `FilteredData` or as a list of `data.frame`. When passing a list of
  non-reactive `data.frame` objects, they are converted to reactive
  `data.frame`s internally. When passing a list of reactive or
  non-reactive `data.frame` objects, the argument `join_keys` is
  required also.

- ...:

  An additional argument `join_keys` is required when `datasets` is a
  list of `data.frame`. It shall contain the keys per dataset in
  `datasets`.

- join_keys:

  (`join_keys` or `NULL`) of join keys per dataset in `datasets`.

- select_validation_rule:

  (`NULL` or `function` or `named list` of `function`) Should there be
  any `shinyvalidate` input validation of the select parts of the
  `data_extract_ui`. If all `data_extract` require the same validation
  function then this can be used directly (i.e.
  `select_validation_rule = shinyvalidate::sv_required()`).

  For more fine-grained control use a list:

  `select_validation_rule = list(extract_1 = sv_required(), extract2 = ~ if (length(.) > 2) "Error")`

  If `NULL` then no validation will be added.

  See example for more details.

- filter_validation_rule:

  (`NULL` or `function` or `named list` of `function`) Same as
  `select_validation_rule` but for the filter (values) part of the
  `data_extract_ui`.

- dataset_validation_rule:

  (`NULL` or `function` or `named list` of `function`) Same as
  `select_validation_rule` but for the choose dataset part of the
  `data_extract_ui`

## Value

reactive named `list` containing outputs from
[`data_extract_srv()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_srv.md).
Output list names are the same as `data_extract` input argument.

## Examples

``` r
library(shiny)
library(shinyvalidate)
library(shinyjs)
library(teal.widgets)

iris_select <- data_extract_spec(
  dataname = "iris",
  select = select_spec(
    label = "Select variable:",
    choices = variable_choices(iris, colnames(iris)),
    selected = "Sepal.Length",
    multiple = TRUE,
    fixed = FALSE
  )
)

iris_filter <- data_extract_spec(
  dataname = "iris",
  filter = filter_spec(
    vars = "Species",
    choices = c("setosa", "versicolor", "virginica"),
    selected = "setosa",
    multiple = TRUE
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
        data_extract_spec = iris_select
      ),
      data_extract_ui(
        id = "species_var",
        label = "Please select 2 Species",
        data_extract_spec = iris_filter
      )
    )
  )
)

server <- function(input, output, session) {
  selector_list <- data_extract_multiple_srv(
    list(x_var = iris_select, species_var = iris_filter),
    datasets = data_list,
    select_validation_rule = list(
      x_var = sv_required("Please select an X column")
    ),
    filter_validation_rule = list(
      species_var = compose_rules(
        sv_required("Exactly 2 Species must be chosen"),
        function(x) if (length(x) != 2) "Exactly 2 Species must be chosen"
      )
    )
  )
  iv_r <- reactive({
    iv <- InputValidator$new()
    compose_and_enable_validators(
      iv,
      selector_list,
      validator_names = NULL
    )
  })

  output$out1 <- renderPrint({
    if (iv_r()$is_valid()) {
      ans <- lapply(selector_list(), function(x) {
        cat(format_data_extract(x()), "\n\n")
      })
    } else {
      "Please fix errors in your selection"
    }
  })
}

if (interactive()) {
  shinyApp(ui, server)
}
```
