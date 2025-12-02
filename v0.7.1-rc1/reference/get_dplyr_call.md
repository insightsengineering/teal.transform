# Parses filter, select, rename and reshape call

Parses filter, select, rename and reshape call

## Usage

``` r
get_dplyr_call(
  selector_list,
  idx = 1L,
  join_keys = teal.data::join_keys(),
  dplyr_call_data = get_dplyr_call_data(selector_list, join_keys = join_keys),
  datasets = NULL
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

- join_keys:

  (`join_keys`) nested list of keys used for joining.

- dplyr_call_data:

  (`list`) simplified selectors with aggregated set of filters,
  selections, reshapes etc. All necessary data for merging.

- data:

  (`NULL` or named `list`) of datasets.

## Value

(`call`) filter, select, rename and reshape call.
