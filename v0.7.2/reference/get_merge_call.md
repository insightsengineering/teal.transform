# Get merge call from a list of selectors

Creates list of calls depending on selector(s) and type of the merge.
The merge order is the same as in selectors passed to the function.

## Usage

``` r
get_merge_call(
  selector_list,
  join_keys = teal.data::join_keys(),
  dplyr_call_data = get_dplyr_call_data(selector_list, join_keys = join_keys),
  merge_function = "dplyr::full_join",
  anl_name = "ANL"
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

- join_keys:

  (`join_keys`) nested list of keys used for joining.

- dplyr_call_data:

  (`list`) simplified selectors with aggregated set of filters.

- merge_function:

  (`character(1)` or `reactive`) A character string of a function that
  accepts the arguments `x`, `y` and `by` to perform the merging of
  datasets.

- anl_name:

  (`character(1)`) Name of the analysis dataset.

## Value

List with merge `call` elements.
