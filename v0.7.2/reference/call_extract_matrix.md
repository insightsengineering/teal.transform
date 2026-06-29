# Get call to subset and select matrix

Get call to subset and select matrix

## Usage

``` r
call_extract_matrix(dataname = ".", row = NULL, column = NULL)
```

## Arguments

- dataname:

  (`character(1)` or `name`).

- row:

  (`name` or `call` or `logical` or `integer` or `character`) optional
  name of the `row` or condition.

- column:

  (`name` or `call` or `logical` or `integer` or `character`) optional
  name of the `column` or condition.

## Value

[`Extract()`](https://rdrr.io/r/base/Extract.html) `call` for matrix in
`x[i, j]` notation.
