# Merge selectors when `dataname`, `reshape`, `filters` and `keys` entries are identical

Merge selectors when `dataname`, `reshape`, `filters` and `keys` entries
are identical

## Usage

``` r
merge_selectors(selector_list)
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

## Value

List of merged selectors or original parameter if the conditions to
merge are not applicable.
