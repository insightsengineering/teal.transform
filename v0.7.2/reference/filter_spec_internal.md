# Data extract dynamic filter specification

Builds a configuration for the `data_extract_ui` module. This function
covers the configuration of filtering datasets (so called
`filter_spec`), which then is used to build the UI element in the `teal`
app.

## Usage

``` r
filter_spec_internal(
  vars_choices,
  vars_selected = NULL,
  vars_label = NULL,
  vars_fixed = FALSE,
  vars_multiple = TRUE,
  choices = NULL,
  selected = NULL,
  label = NULL,
  fixed = FALSE,
  multiple = TRUE,
  sep = attr(vars_choices, "sep"),
  drop_keys = FALSE,
  dataname = NULL,
  initialized = FALSE
)

# S3 method for class 'delayed_data'
filter_spec_internal(
  vars_choices,
  vars_selected = NULL,
  vars_label = NULL,
  vars_fixed = FALSE,
  vars_multiple = TRUE,
  choices = NULL,
  selected = NULL,
  label = NULL,
  fixed = FALSE,
  multiple = TRUE,
  sep = attr(vars_choices, "sep"),
  drop_keys = FALSE,
  dataname = NULL,
  initialized = FALSE
)

# Default S3 method
filter_spec_internal(
  vars_choices,
  vars_selected = NULL,
  vars_label = NULL,
  vars_fixed = FALSE,
  vars_multiple = TRUE,
  choices = NULL,
  selected = NULL,
  label = NULL,
  fixed = FALSE,
  multiple = TRUE,
  sep = attr(vars_choices, "sep"),
  drop_keys = FALSE,
  dataname = NULL,
  initialized = FALSE
)
```

## Arguments

- vars_choices:

  (`character` or `delayed_data`) the vector of dataset column names
  available to build dynamic filter `delayed_data` objects can be
  created via
  [`variable_choices()`](https://insightsengineering.github.io/teal.transform/reference/variable_choices.md).

- vars_selected:

  (`NULL` or named `character`) the selected column name out from
  `choices`.

- vars_label:

  (`character`) the title printed on the UI element generated on the
  basis of this `filter_spec`.

- vars_fixed:

  (`logical`) if true allow to change the selected variables in the UI
  element; otherwise, do not allow.

- vars_multiple:

  (`logical`) if true allow to select multiple variables in the UI
  elements; otherwise, do not allow.

- choices:

  (`character` or `numeric` or `logical` or (`delayed_data`) object.
  Named character vector to define the choices of a shiny
  [`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html).
  These choices will be used to filter the dataset.

  These shall be filter values of the `vars` input separated by the
  separator(`sep`). Please watch out that the filter values have to
  follow the order of the `vars` input. In the following example we will
  show how to filter two columns:

  `vars = c("PARAMCD","AVISIT")` and
  `choices = c("CRP - BASELINE", "ALT - BASELINE")` will lead to a
  filtering of
  `(PARAMCD == "CRP" & AVISIT == "BASELINE") | (PARAMCD == "ALT" & AVISIT == "BASELINE")`.

  The `sep` input has to be `" - "` in this case.

  `delayed_data` objects can be created via
  [`variable_choices()`](https://insightsengineering.github.io/teal.transform/reference/variable_choices.md)
  or
  [`value_choices()`](https://insightsengineering.github.io/teal.transform/reference/value_choices.md).

- selected:

  (`character` or `numeric` or `logical` or (`delayed_data` or
  `delayed_choices`) object. Named character vector to define the
  selected values of a shiny
  [`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html)
  (default values). This value will be displayed inside the shiny app
  upon start. `delayed_choices` objects resolve selection when choices
  become available.

- label:

  (`character`) optional, defines a label on top of this specific shiny
  [`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html).
  The default value is `"Filter by"`.

- fixed:

  (`logical`) if true allow to change the initially selected values of
  the variables; otherwise, do not allow.

- multiple:

  (`logical`) Whether multiple values shall be allowed in the shiny
  [`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html).

- sep:

  (`character`) A separator string to split the `choices` or `selected`
  inputs into the values of the different columns.

- drop_keys:

  (`logical`) optional, whether to drop filter column from the dataset
  keys, `TRUE` on default.

- dataname:

  (`character`) the name of the dataset this filter covers. Set during
  the initialization of the `teal` application.

- initialized:

  (`logical`) indicates whether this filter was already initialized in
  the application. TRUE if this filter was already consumed by the
  server function; FALSE otherwise.

## Value

`filter_spec` or `delayed_filter_spec` S3-class object.

## See also

filter_spec
