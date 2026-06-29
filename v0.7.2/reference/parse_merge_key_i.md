# Parses merge keys

Parses merge keys

## Usage

``` r
parse_merge_key_i(
  selector_list,
  idx,
  dplyr_call_data = get_dplyr_call_data(selector_list),
  merge_key = get_merge_key_i(selector_list, idx, dplyr_call_data)
)
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

- idx:

  (`integer`) optional, current selector index in all selectors list.

- dplyr_call_data:

  (`list`) simplified selectors with aggregated set of filters.

- merge_key:

  keys obtained from `get_merge_key_i`.

## Value

`call` with merge keys.
