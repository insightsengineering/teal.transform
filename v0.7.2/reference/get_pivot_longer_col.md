# Get pivot longer columns

Get values names which are spread into columns.

## Usage

``` r
get_pivot_longer_col(selector)
```

## Arguments

- selector:

  one element of selector_list obtained by `get_dplyr_call_data`.

## Value

A `character` vector of all the selected columns that are not a `keys`
element.
