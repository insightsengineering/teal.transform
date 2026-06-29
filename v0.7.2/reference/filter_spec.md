# Data extract filter specification

It consists in choices and additionally the variable names for the
choices.

## Usage

``` r
filter_spec(
  vars,
  choices = NULL,
  selected = if (inherits(choices, "delayed_data")) NULL else choices[1],
  multiple = length(selected) > 1 || inherits(selected, "multiple_choices"),
  label = "Filter by",
  sep = attr(choices, "sep"),
  drop_keys = FALSE
)
```

## Arguments

- vars:

  (`character` or `delayed_data`) object. Character vector giving the
  columns to be filtered. These should be key variables of the data set
  to be filtered. `delayed_data` objects can be created via
  [`variable_choices()`](https://insightsengineering.github.io/teal.transform/reference/variable_choices.md),
  [`value_choices()`](https://insightsengineering.github.io/teal.transform/reference/value_choices.md),
  or
  [`choices_selected()`](https://insightsengineering.github.io/teal.transform/reference/choices_selected.md).

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

- multiple:

  (`logical`) Whether multiple values shall be allowed in the shiny
  [`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html).

- label:

  (`character`) optional, defines a label on top of this specific shiny
  [`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html).
  The default value is `"Filter by"`.

- sep:

  (`character`) A separator string to split the `choices` or `selected`
  inputs into the values of the different columns.

- drop_keys:

  (`logical`) optional, whether to drop filter column from the dataset
  keys, `TRUE` on default.

## Value

`filter_spec`-S3-class object or `delayed_filter_spec`-S3-class object.

## Details

The `filter_spec` is used inside `teal` apps to allow filtering datasets
for their key variables. Imagine having an adverse events table. It has
the columns `PARAMCD` and `CNSR`. `PARAMCD` contains the levels `"OS"`,
`"PFS"`, `"EFS"`. `CNSR` contains the levels `"0"` and `"1"`. The first
example should show how a `filter_spec` setup will influence the
drop-down menu the app user will see.

## Examples

``` r
# for Adverse Events table
filter_spec(
  vars = c("PARAMCD", "CNSR"),
  sep = "-",
  choices = c("OS-1" = "OS-1", "OS-0" = "OS-0", "PFS-1" = "PFS-1"),
  selected = "OS-1",
  multiple = FALSE,
  label = "Choose endpoint and Censor"
)
#> filter_spec with delayed data:
#> $ vars_choices
#> [1] "PARAMCD" "CNSR"   
#> $ vars_selected
#> [1] "PARAMCD" "CNSR"   
#> $ vars_label
#> NULL
#> $ vars_fixed
#> [1] TRUE
#> $ vars_multiple
#> [1] TRUE
#> $ choices
#>    OS-1    OS-0   PFS-1 
#>  "OS-1"  "OS-0" "PFS-1" 
#> $ selected
#> [1] "OS-1"
#> $ label
#> [1] "Choose endpoint and Censor"
#> $ multiple
#> [1] FALSE
#> $ fixed
#> [1] FALSE
#> $ sep
#> [1] "-"
#> $ drop_keys
#> [1] FALSE
#> $ dataname
#> NULL
#> $ initialized
#> [1] FALSE

# filtering a single variable
filter_spec(
  vars = c("PARAMCD"),
  sep = "-",
  choices = c("OS", "PFS", "EFS"),
  selected = "OS",
  multiple = FALSE,
  label = "Choose endpoint"
)
#> filter_spec with delayed data:
#> $ vars_choices
#> [1] "PARAMCD"
#> $ vars_selected
#> [1] "PARAMCD"
#> $ vars_label
#> NULL
#> $ vars_fixed
#> [1] TRUE
#> $ vars_multiple
#> [1] TRUE
#> $ choices
#> [1] "OS"  "PFS" "EFS"
#> $ selected
#> [1] "OS"
#> $ label
#> [1] "Choose endpoint"
#> $ multiple
#> [1] FALSE
#> $ fixed
#> [1] FALSE
#> $ sep
#> [1] "-"
#> $ drop_keys
#> [1] FALSE
#> $ dataname
#> NULL
#> $ initialized
#> [1] FALSE

# filtering a single variable by multiple levels of the variable
filter_spec(
  vars = c("PARAMCD"),
  sep = "-",
  choices = c("OS", "PFS", "EFS"),
  selected = c("OS", "PFS"),
  multiple = TRUE,
  label = "Choose endpoint"
)
#> filter_spec with delayed data:
#> $ vars_choices
#> [1] "PARAMCD"
#> $ vars_selected
#> [1] "PARAMCD"
#> $ vars_label
#> NULL
#> $ vars_fixed
#> [1] TRUE
#> $ vars_multiple
#> [1] TRUE
#> $ choices
#> [1] "OS"  "PFS" "EFS"
#> $ selected
#> [1] "OS"  "PFS"
#> $ label
#> [1] "Choose endpoint"
#> $ multiple
#> [1] TRUE
#> $ fixed
#> [1] FALSE
#> $ sep
#> [1] "-"
#> $ drop_keys
#> [1] FALSE
#> $ dataname
#> NULL
#> $ initialized
#> [1] FALSE

# delayed version
filter_spec(
  vars = variable_choices("ADSL", "SEX"),
  sep = "-",
  choices = value_choices("ADSL", "SEX", "SEX"),
  selected = "F",
  multiple = FALSE,
  label = "Choose endpoint and Censor"
)
#> filter_spec with delayed data:
#> $ vars_choices
#>   variable_choices with delayed data: ADSL
#>   $ data
#>   [1] "ADSL"
#>   $ subset
#>   [1] "SEX"
#>   $ key
#>   NULL
#> $ vars_selected
#>   variable_choices with delayed data: ADSL
#>   $ data
#>   [1] "ADSL"
#>   $ subset
#>   [1] "SEX"
#>   $ key
#>   NULL
#> $ vars_label
#> NULL
#> $ vars_fixed
#> [1] TRUE
#> $ vars_multiple
#> [1] TRUE
#> $ choices
#>   value_choices with delayed data:  ADSL
#>   $ data
#>   [1] "ADSL"
#>   $ var_choices
#>   [1] "SEX"
#>   $ var_label
#>   [1] "SEX"
#>   $ subset
#>   NULL
#>   $ sep
#>   [1] " - "
#> $ selected
#> [1] "F"
#> $ label
#> [1] "Choose endpoint and Censor"
#> $ multiple
#> [1] FALSE
#> $ fixed
#> [1] FALSE
#> $ sep
#> [1] "-"
#> $ drop_keys
#> [1] FALSE
#> $ dataname
#> NULL
#> $ initialized
#> [1] FALSE
# using `choices_selected()`
filter_spec(
  vars = choices_selected(variable_choices("ADSL", subset = c("SEX", "AGE")), "SEX", fixed = FALSE),
  multiple = TRUE
)
#> filter_spec with delayed data:
#> $ vars_choices
#>   variable_choices with delayed data: ADSL
#>   $ data
#>   [1] "ADSL"
#>   $ subset
#>   [1] "SEX" "AGE"
#>   $ key
#>   NULL
#> $ vars_selected
#> [1] "SEX"
#> $ vars_label
#> [1] "Filter by"
#> $ vars_fixed
#> [1] FALSE
#> $ vars_multiple
#> [1] FALSE
#> $ choices
#> NULL
#> $ selected
#> NULL
#> $ label
#> NULL
#> $ multiple
#> [1] TRUE
#> $ fixed
#> [1] FALSE
#> $ sep
#> [1] " - "
#> $ drop_keys
#> [1] FALSE
#> $ dataname
#> NULL
#> $ initialized
#> [1] FALSE

filter_spec(
  vars = choices_selected(variable_choices("ADSL"), "SEX", fixed = TRUE),
  multiple = TRUE
)
#> filter_spec with delayed data:
#> $ vars_choices
#>   variable_choices with delayed data: ADSL
#>   $ data
#>   [1] "ADSL"
#>   $ subset
#>   NULL
#>   $ key
#>   NULL
#> $ vars_selected
#> [1] "SEX"
#> $ vars_label
#> NULL
#> $ vars_fixed
#> [1] TRUE
#> $ vars_multiple
#> [1] FALSE
#> $ choices
#> NULL
#> $ selected
#> NULL
#> $ label
#> [1] "Filter by"
#> $ multiple
#> [1] TRUE
#> $ fixed
#> [1] FALSE
#> $ sep
#> [1] " - "
#> $ drop_keys
#> [1] FALSE
#> $ dataname
#> NULL
#> $ initialized
#> [1] FALSE

# choose all choices
adsl_filter <- filter_spec(
  vars = choices_selected(variable_choices("ADSL"), "SEX", fixed = FALSE),
  choices = value_choices("ADSL", "SEX"),
  selected = all_choices()
)
```
