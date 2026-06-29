# Returns non-key column names from data

Returns non-key column names from data.

## Usage

``` r
get_dataset_prefixed_col_names(data)
```

## Arguments

- data:

  (`data.frame`) Data with attribute `filter_and_columns`. This can only
  be created by
  [`data_extract_srv()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_srv.md),
  which returns a shiny
  [`shiny::reactive()`](https://rdrr.io/pkg/shiny/man/reactive.html).

## Value

A named `character` vector with the non-key columns of the `data`.

## References

[`data_extract_srv()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_srv.md)
