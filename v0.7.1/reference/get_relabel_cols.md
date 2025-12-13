# Get columns to relabel

Get columns to relabel excluding these which has been reshaped
(pivot_wider).

## Usage

``` r
get_relabel_cols(columns_source, dplyr_call_data)
```

## Arguments

- columns_source:

  (`list`)

- dplyr_call_data:

  (`list`)

## Value

`columns_source` list without columns that have been reshaped.
