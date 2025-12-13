# Data extract input for `teal` modules

The Data extract input can be used to filter and select columns from a
data set. This function enables such an input in `teal`. Please use the
constructor function data_extract_spec to set it up.

## Usage

``` r
data_extract_spec(dataname, select = NULL, filter = NULL, reshape = FALSE)
```

## Arguments

- dataname:

  (`character`) The name of the dataset to be extracted.

- select:

  (`NULL` or `select_spec`-S3 class or `delayed_select_spec`) Columns to
  be selected from the input dataset mentioned in `dataname`. The setup
  can be created using
  [select_spec](https://insightsengineering.github.io/teal.transform/reference/select_spec.md)
  function.

- filter:

  (`NULL` or `filter_spec` or its respective delayed version) Setup of
  the filtering of key columns inside the dataset. This setup can be
  created using the
  [filter_spec](https://insightsengineering.github.io/teal.transform/reference/filter_spec.md)
  function. Please note that if both select and filter are set to
  `NULL`, then the result will be a filter spec UI with all variables as
  possible choices and a select spec with multiple set to `TRUE`.

- reshape:

  (`logical`) whether reshape long to wide. Note that it will be used
  only in case of long dataset with multiple keys selected in filter
  part.

## Value

`data_extract_spec` object.

## Note

No checks based on columns can be done because the data is only referred
to by name.

## Module Development

`teal.transform` uses this object to construct a UI element in a module.

## References

[select_spec](https://insightsengineering.github.io/teal.transform/reference/select_spec.md)
[filter_spec](https://insightsengineering.github.io/teal.transform/reference/filter_spec.md)

## Examples

``` r
adtte_filters <- filter_spec(
  vars = c("PARAMCD", "CNSR"),
  sep = "-",
  choices = c("OS-1" = "OS-1", "OS-0" = "OS-0", "PFS-1" = "PFS-1"),
  selected = "OS-1",
  multiple = FALSE,
  label = "Choose endpoint and Censor"
)

data_extract_spec(
  dataname = "ADTTE",
  filter = adtte_filters,
  select = select_spec(
    choices = c("AVAL", "BMRKR1", "AGE"),
    selected = c("AVAL", "BMRKR1"),
    multiple = TRUE,
    fixed = FALSE,
    label = "Column"
  )
)
#> $dataname
#> [1] "ADTTE"
#> 
#> $select
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
#> [1] "Column"
#> 
#> attr(,"class")
#> [1] "select_spec"
#> 
#> $filter
#> $filter[[1]]
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
#> [1] "ADTTE"
#> $ initialized
#> [1] FALSE
#> 
#> 
#> $reshape
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "data_extract_spec"

data_extract_spec(
  dataname = "ADSL",
  filter = NULL,
  select = select_spec(
    choices = c("AGE", "SEX", "USUBJID"),
    selected = c("SEX"),
    multiple = FALSE,
    fixed = FALSE
  )
)
#> $dataname
#> [1] "ADSL"
#> 
#> $select
#> $choices
#>       AGE       SEX   USUBJID 
#>     "AGE"     "SEX" "USUBJID" 
#> 
#> $selected
#>   SEX 
#> "SEX" 
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
#> [1] "Select"
#> 
#> attr(,"class")
#> [1] "select_spec"
#> 
#> $filter
#> NULL
#> 
#> $reshape
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "data_extract_spec"
data_extract_spec(
  dataname = "ADSL",
  filter = filter_spec(
    vars = variable_choices("ADSL", subset = c("AGE"))
  )
)
#> data_extract_spec with delayed data: ADSL
#> 
#> $ dataname
#> [1] "ADSL"
#> $ select
#> NULL
#> $ filter
#>   [[1]]
#>     filter_spec with delayed data:
#>     $ vars_choices
#>       variable_choices with delayed data: ADSL
#>       $ data
#>       [1] "ADSL"
#>       $ subset
#>       [1] "AGE"
#>       $ key
#>       NULL
#>     $ vars_selected
#>       variable_choices with delayed data: ADSL
#>       $ data
#>       [1] "ADSL"
#>       $ subset
#>       [1] "AGE"
#>       $ key
#>       NULL
#>     $ vars_label
#>     NULL
#>     $ vars_fixed
#>     [1] TRUE
#>     $ vars_multiple
#>     [1] TRUE
#>     $ choices
#>     NULL
#>     $ selected
#>     NULL
#>     $ label
#>     [1] "Filter by"
#>     $ multiple
#>     [1] FALSE
#>     $ fixed
#>     [1] FALSE
#>     $ sep
#>     [1] " - "
#>     $ drop_keys
#>     [1] FALSE
#>     $ dataname
#>     [1] "ADSL"
#>     $ initialized
#>     [1] FALSE
#> $ reshape
#> [1] FALSE

dynamic_filter <- filter_spec(
  vars = choices_selected(variable_choices("ADSL"), "COUNTRY"),
  multiple = TRUE
)
data_extract_spec(
  dataname = "ADSL",
  filter = dynamic_filter
)
#> data_extract_spec with delayed data: ADSL
#> 
#> $ dataname
#> [1] "ADSL"
#> $ select
#> NULL
#> $ filter
#>   [[1]]
#>     filter_spec with delayed data:
#>     $ vars_choices
#>       variable_choices with delayed data: ADSL
#>       $ data
#>       [1] "ADSL"
#>       $ subset
#>       NULL
#>       $ key
#>       NULL
#>     $ vars_selected
#>     [1] "COUNTRY"
#>     $ vars_label
#>     [1] "Filter by"
#>     $ vars_fixed
#>     [1] FALSE
#>     $ vars_multiple
#>     [1] FALSE
#>     $ choices
#>     NULL
#>     $ selected
#>     NULL
#>     $ label
#>     NULL
#>     $ multiple
#>     [1] TRUE
#>     $ fixed
#>     [1] FALSE
#>     $ sep
#>     [1] " - "
#>     $ drop_keys
#>     [1] FALSE
#>     $ dataname
#>     [1] "ADSL"
#>     $ initialized
#>     [1] FALSE
#> $ reshape
#> [1] FALSE
```
