# Get unite columns

Get key names which spreads values into columns. Reshape is done only on
keys which are in `filter_spec`.

## Usage

``` r
get_reshape_unite_col(selector)
```

## Arguments

- selector:

  one element of selector_list obtained by `get_dplyr_call_data`.

## Value

A `character` vector of all the selector's keys that are defined in the
filters.
