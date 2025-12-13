# Aggregates data extract selectors

Simplifies `selector_list` into aggregated list with one element per
same selector - same dataset, same filter configuration and same reshape
status.

## Usage

``` r
get_dplyr_call_data(selector_list, join_keys = teal.data::join_keys())
```

## Arguments

- selector_list:

  (`reactive`) output from
  [`data_extract_multiple_srv()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_multiple_srv.md)
  or a reactive named list of outputs from
  [`data_extract_srv()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_srv.md).
  When using a reactive named list, the names must be identical to the
  shiny ids of the respective
  [`data_extract_ui()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_ui.md).

- join_keys:

  (`join_keys`) nested list of keys used for joining.

## Value

(`list`) simplified selectors with aggregated set of filters,
selections, reshapes etc. All necessary data for merging.
