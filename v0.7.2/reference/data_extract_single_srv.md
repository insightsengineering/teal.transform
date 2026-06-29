# The server function for a single `data_extract_spec` object

The server function for a single `data_extract_spec` object

## Usage

``` r
data_extract_single_srv(id, datasets, single_data_extract_spec)
```

## Arguments

- id:

  (`character`) id string.

- datasets:

  (`named list`) a list of reactive `data.frame` type objects.

- single_data_extract_spec:

  (`data_extract_spec`) the
  [`data_extract_spec()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_spec.md)
  object to handle.

## Value

`NULL`.

## Details

The Shiny server function for handling a single
[data_extract_spec](https://insightsengineering.github.io/teal.transform/reference/data_extract_spec.md)
object.
