# Validate data_extracts in merge_datasets

Validate selected inputs from data_extract before passing to data_merge
to avoid `dplyr` errors or unexpected results.

## Usage

``` r
check_data_merge_selectors(selector_list)
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

`NULL` if check is successful and `shiny` validate error otherwise.
