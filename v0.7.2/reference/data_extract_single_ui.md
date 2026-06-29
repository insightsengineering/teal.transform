# Returns a `shiny.tag` with the UI elements for a `data_extract_spec`

Returns a `shiny.tag` with the UI elements for a `data_extract_spec`

## Usage

``` r
data_extract_single_ui(id = NULL, single_data_extract_spec)
```

## Arguments

- id:

  (`character(1)`) the id of the module.

- single_data_extract_spec:

  (`data_extract_spec`) the
  [`data_extract_spec()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_spec.md)
  object to handle.

## Value

`shiny.tag` the HTML element defining the UI.

## Details

Creates a `shiny.tag` element defining the UI elements corresponding to
a single `data_extract_spec` object.
