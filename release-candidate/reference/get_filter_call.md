# Build a `dplyr` filter call

Build a `dplyr` filter call

## Usage

``` r
get_filter_call(filter, dataname = NULL, datasets = NULL)
```

## Arguments

- filter:

  (`list`) Either list of lists or list with `select` and `selected`
  items.

- dataname:

  (`NULL` or `character`) name of dataset.

- datasets:

  (`NULL` or named `list`).

## Value

`dplyr` filter `call`.
