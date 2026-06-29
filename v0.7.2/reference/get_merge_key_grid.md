# Gets merge key pair list from keys list

Gets merge key pair list from keys list

## Usage

``` r
get_merge_key_grid(selector_list, join_keys = teal.data::join_keys())
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

List of merge key pairs between all datasets.
