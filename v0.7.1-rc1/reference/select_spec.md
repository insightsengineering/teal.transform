# Column selection input specification

`select_spec` is used inside `teal` to create a
[`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html)
that will select columns from a dataset.

## Usage

``` r
select_spec(
  choices,
  selected = if (inherits(choices, "delayed_data")) NULL else choices[1],
  multiple = length(selected) > 1 || inherits(selected, "multiple_choices"),
  fixed = FALSE,
  always_selected = NULL,
  ordered = FALSE,
  label = "Select"
)

select_spec.delayed_data(
  choices,
  selected = NULL,
  multiple = length(selected) > 1,
  fixed = FALSE,
  always_selected = NULL,
  ordered = FALSE,
  label = NULL
)

select_spec.default(
  choices,
  selected = choices[1],
  multiple = length(selected) > 1,
  fixed = FALSE,
  always_selected = NULL,
  ordered = FALSE,
  label = NULL
)
```

## Arguments

- choices:

  (`character` or `delayed_data`) object. Named character vector to
  define the choices of a shiny
  [`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html).
  These have to be columns in the dataset defined in the
  [`data_extract_spec()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_spec.md)
  where this is called. `delayed_data` objects can be created via
  [`variable_choices()`](https://insightsengineering.github.io/teal.transform/reference/variable_choices.md)
  or
  [`value_choices()`](https://insightsengineering.github.io/teal.transform/reference/value_choices.md).

- selected:

  (`character` or `NULL` or `delayed_choices` or `delayed_data`)
  optional named character vector to define the selected values of a
  shiny
  [`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html).
  Passing a `delayed_choices` object defers selection until data is
  available. Defaults to the first value of `choices` or `NULL` for
  delayed data loading.

- multiple:

  (`logical`) Whether multiple values shall be allowed in the shiny
  [`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html).

- fixed:

  (`logical`) optional
  [`data_extract_spec()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_spec.md)
  specific feature to hide the choices selected in case they are not
  needed. Setting fixed to `TRUE` will not allow the user to select
  columns. It will then lead to a selection of columns in the dataset
  that is defined by the developer of the app.

- always_selected:

  (`character`) Additional column names from the data set that should
  always be selected

- ordered:

  (`logical(1)`) Flags whether selection order should be tracked.

- label:

  (`character`) optional, defines a label on top of this specific shiny
  [`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html).
  The default value is `"Select"`.

## Value

A `select_spec`-S3 class object or `delayed_select_spec`-S3-class
object. It contains all input values.

If `select_spec`, then the function double checks the `choices` and
`selected` inputs.

## Examples

``` r
# Selection with just one column allowed
select_spec(
  choices = c("AVAL", "BMRKR1", "AGE"),
  selected = c("AVAL"),
  multiple = FALSE,
  fixed = FALSE,
  label = "Column"
)
#> $choices
#>     AVAL   BMRKR1      AGE 
#>   "AVAL" "BMRKR1"    "AGE" 
#> 
#> $selected
#>   AVAL 
#> "AVAL" 
#> 
#> $multiple
#> [1] FALSE
#> 
#> $fixed
#> [1] FALSE
#> 
#> $always_selected
#> NULL
#> 
#> $ordered
#> [1] FALSE
#> 
#> $label
#> [1] "Column"
#> 
#> attr(,"class")
#> [1] "select_spec"

# Selection with just multiple columns allowed
select_spec(
  choices = c("AVAL", "BMRKR1", "AGE"),
  selected = c("AVAL", "BMRKR1"),
  multiple = TRUE,
  fixed = FALSE,
  label = "Columns"
)
#> $choices
#>     AVAL   BMRKR1      AGE 
#>   "AVAL" "BMRKR1"    "AGE" 
#> 
#> $selected
#>     AVAL   BMRKR1 
#>   "AVAL" "BMRKR1" 
#> 
#> $multiple
#> [1] TRUE
#> 
#> $fixed
#> [1] FALSE
#> 
#> $always_selected
#> NULL
#> 
#> $ordered
#> [1] FALSE
#> 
#> $label
#> [1] "Columns"
#> 
#> attr(,"class")
#> [1] "select_spec"

# Selection without user access
select_spec(
  choices = c("AVAL", "BMRKR1"),
  selected = c("AVAL", "BMRKR1"),
  multiple = TRUE,
  fixed = TRUE,
  label = "Columns"
)
#> $choices
#>     AVAL   BMRKR1 
#>   "AVAL" "BMRKR1" 
#> 
#> $selected
#>     AVAL   BMRKR1 
#>   "AVAL" "BMRKR1" 
#> 
#> $multiple
#> [1] TRUE
#> 
#> $fixed
#> [1] TRUE
#> 
#> $always_selected
#> NULL
#> 
#> $ordered
#> [1] FALSE
#> 
#> $label
#> [1] "Columns"
#> 
#> attr(,"class")
#> [1] "select_spec"

# Delayed version
select_spec(
  label = "Select variable:",
  choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
  selected = "BMRKR1",
  multiple = FALSE,
  fixed = FALSE
)
#> select_spec with delayed data: ADSL
#> $ choices
#>   variable_choices with delayed data: ADSL
#>   $ data
#>   [1] "ADSL"
#>   $ subset
#>   [1] "BMRKR1" "BMRKR2"
#>   $ key
#>   NULL
#> $ selected
#> [1] "BMRKR1"
#> $ multiple
#> [1] FALSE
#> $ fixed
#> [1] FALSE
#> $ always_selected
#> NULL
#> $ ordered
#> [1] FALSE
#> $ label
#> [1] "Select variable:"

# delayed_choices passed to selected
select_spec(
  label = "Select variable:",
  choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
  selected = all_choices()
)
#> select_spec with delayed data: ADSL
#> $ choices
#>   variable_choices with delayed data: ADSL
#>   $ data
#>   [1] "ADSL"
#>   $ subset
#>   [1] "BMRKR1" "BMRKR2"
#>   $ key
#>   NULL
#> $ selected
#>   variable_choices with delayed data: ADSL
#>   $ data
#>   [1] "ADSL"
#>   $ subset
#>   function(data) {
#>           fun(original_fun(data))
#>         }
#> <environment: 0x555b5ed9cdf0>
#>   $ key
#>   NULL
#> $ multiple
#> [1] TRUE
#> $ fixed
#> [1] FALSE
#> $ always_selected
#> NULL
#> $ ordered
#> [1] FALSE
#> $ label
#> [1] "Select variable:"

# Both below objects are semantically the same
select_spec(choices = variable_choices("ADSL"), selected = variable_choices("ADSL"))
#> select_spec with delayed data: ADSL
#> $ choices
#>   variable_choices with delayed data: ADSL
#>   $ data
#>   [1] "ADSL"
#>   $ subset
#>   NULL
#>   $ key
#>   NULL
#> $ selected
#>   variable_choices with delayed data: ADSL
#>   $ data
#>   [1] "ADSL"
#>   $ subset
#>   NULL
#>   $ key
#>   NULL
#> $ multiple
#> [1] TRUE
#> $ fixed
#> [1] FALSE
#> $ always_selected
#> NULL
#> $ ordered
#> [1] FALSE
#> $ label
#> [1] "Select"
select_spec(choices = variable_choices("ADSL"), selected = all_choices())
#> select_spec with delayed data: ADSL
#> $ choices
#>   variable_choices with delayed data: ADSL
#>   $ data
#>   [1] "ADSL"
#>   $ subset
#>   NULL
#>   $ key
#>   NULL
#> $ selected
#>   variable_choices with delayed data: ADSL
#>   $ data
#>   [1] "ADSL"
#>   $ subset
#>   NULL
#>   $ key
#>   NULL
#> $ multiple
#> [1] TRUE
#> $ fixed
#> [1] FALSE
#> $ always_selected
#> NULL
#> $ ordered
#> [1] FALSE
#> $ label
#> [1] "Select"
```
