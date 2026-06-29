# Extraction of the selector(s) details

Extracting details of the selection(s) in
[data_extract_ui](https://insightsengineering.github.io/teal.transform/reference/data_extract_ui.md)
elements.

## Usage

``` r
data_extract_srv(id, datasets, data_extract_spec, ...)

# S3 method for class 'FilteredData'
data_extract_srv(id, datasets, data_extract_spec, ...)

# S3 method for class 'list'
data_extract_srv(
  id,
  datasets,
  data_extract_spec,
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

- id:

  An ID string that corresponds with the ID used to call the module's UI
  function.

- datasets:

  (`FilteredData` or `list` of `reactive` or non-`reactive`
  `data.frame`) object containing data either in the form of
  `FilteredData` or as a list of `data.frame`. When passing a list of
  non-reactive `data.frame` objects, they are converted to reactive
  `data.frame`s internally. When passing a list of reactive or
  non-reactive `data.frame` objects, the argument `join_keys` is
  required also.

- data_extract_spec:

  (`data_extract_spec` or a list of `data_extract_spec`) A list of data
  filter and select information constructed by
  [data_extract_spec](https://insightsengineering.github.io/teal.transform/reference/data_extract_spec.md).

- ...:

  An additional argument `join_keys` is required when `datasets` is a
  list of `data.frame`. It shall contain the keys per dataset in
  `datasets`.

- join_keys:

  (`join_keys` or `NULL`) of keys per dataset in `datasets`.

- select_validation_rule:

  (`NULL` or `function`) Should there be any `shinyvalidate` input
  validation of the select parts of the `data_extract_ui`.

  You can use a validation function directly (i.e.
  `select_validation_rule = shinyvalidate::sv_required()`) or for more
  fine-grained control use a function:

  `select_validation_rule = ~ if (length(.) > 2) "Error"`.

  If `NULL` then no validation will be added. See example for more
  details.

- filter_validation_rule:

  (`NULL` or `function`) Same as `select_validation_rule` but for the
  filter (values) part of the `data_extract_ui`.

- dataset_validation_rule:

  (`NULL` or `function`) Same as `select_validation_rule` but for the
  choose dataset part of the `data_extract_ui`

## Value

A reactive `list` containing following fields:

- `filters`: A list with the information on the filters that are applied
  to the data set.

- `select`: The variables that are selected from the dataset.

- `always_selected`: The column names from the data set that should
  always be selected.

- `reshape`: Whether reshape long to wide should be applied or not.

- `dataname`: The name of the data set.

- `internal_id`: The `id` of the corresponding shiny input element.

- `keys`: The names of the columns that can be used to merge the data
  set.

- `iv`: A
  [`shinyvalidate::InputValidator`](https://rstudio.github.io/shinyvalidate/reference/InputValidator.html)
  containing `validator` for this `data_extract`.

## References

data_extract_srv

## Examples

``` r
library(shiny)
library(shinyvalidate)
library(teal.data)
library(teal.widgets)

# Sample ADSL dataset
ADSL <- data.frame(
  STUDYID = "A",
  USUBJID = LETTERS[1:10],
  SEX = rep(c("F", "M"), 5),
  AGE = rpois(10, 30),
  BMRKR1 = rlnorm(10)
)

# Specification for data extraction
adsl_extract <- data_extract_spec(
  dataname = "ADSL",
  filter = filter_spec(vars = "SEX", choices = c("F", "M"), selected = "F"),
  select = select_spec(
    label = "Select variable:",
    choices = variable_choices(ADSL, c("AGE", "BMRKR1")),
    selected = "AGE",
    multiple = TRUE,
    fixed = FALSE
  )
)

# Using reactive list of data.frames
data_list <- list(ADSL = reactive(ADSL))

join_keys <- join_keys(join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")))

# App: data extraction with validation
ui <- bslib::page_fluid(
  bslib::layout_sidebar(
    verbatimTextOutput("out1"),
    encoding = tagList(
      data_extract_ui(
        id = "adsl_var",
        label = "ADSL selection",
        data_extract_spec = adsl_extract
      )
    )
  )
)
server <- function(input, output, session) {
  adsl_reactive_input <- data_extract_srv(
    id = "adsl_var",
    datasets = data_list,
    data_extract_spec = adsl_extract,
    join_keys = join_keys,
    select_validation_rule = sv_required("Please select a variable.")
  )

  iv_r <- reactive({
    iv <- InputValidator$new()
    iv$add_validator(adsl_reactive_input()$iv)
    iv$enable()
    iv
  })

  output$out1 <- renderPrint({
    if (iv_r()$is_valid()) {
      cat(format_data_extract(adsl_reactive_input()))
    } else {
      "Please fix errors in your selection"
    }
  })
}

if (interactive()) {
  shinyApp(ui, server)
}

# App: simplified data extraction
ui <- bslib::page_fluid(
  bslib::layout_sidebar(
    verbatimTextOutput("out1"),
    sidebar = tagList(
      data_extract_ui(
        id = "adsl_var",
        label = "ADSL selection",
        data_extract_spec = adsl_extract
      )
    )
  )
)

server <- function(input, output, session) {
  adsl_reactive_input <- data_extract_srv(
    id = "adsl_var",
    datasets = data_list,
    data_extract_spec = adsl_extract
  )

  output$out1 <- renderPrint(adsl_reactive_input())
}

if (interactive()) {
  shinyApp(ui, server)
}
```
