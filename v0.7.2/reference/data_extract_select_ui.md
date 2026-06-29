# Returns a `shiny.tag.list` object with the UI for a `select_spec` object

Returns a `shiny.tag.list` object with the UI for a `select_spec` object

## Usage

``` r
data_extract_select_ui(select, id = "select")
```

## Arguments

- select:

  (`select_spec`) A definition of a select spec element. Setting
  [`select_spec()`](https://insightsengineering.github.io/teal.transform/reference/select_spec.md)
  with `ordered = TRUE` makes this selector responsive to the variable
  selection order.

- id:

  (`character(1)`) The shiny `inputId` of the element.

## Value

`shiny.tag.list` with the UI.
