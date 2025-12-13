# Returns a `shiny.tag` object with the UI for a `filter_spec` object

Returns a `shiny.tag` object with the UI for a `filter_spec` object

## Usage

``` r
data_extract_filter_ui(filter, id = "filter")
```

## Arguments

- filter:

  (`filter_spec`) the object generated with
  [`filter_spec()`](https://insightsengineering.github.io/teal.transform/reference/filter_spec.md).

- id:

  (`character(1)`) the shiny `inputId` for the generated `shiny.tag`.

## Value

`shiny.tag` defining the `filter_spec`'s UI element.

## Details

Creates two `optionSelectInput` elements (one for column and one for
values) based on a definition of a
[`filter_spec()`](https://insightsengineering.github.io/teal.transform/reference/filter_spec.md)
object.
