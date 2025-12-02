# Get call to subset and select array

Get call to subset and select array

## Usage

``` r
call_extract_array(dataname = ".", row = NULL, column = NULL, aisle = NULL)
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

- aisle:

  (`name` or `call` or `logical` or `integer` or `character`) optional
  name of the `row` or condition.

## Value

[`Extract()`](https://rdrr.io/r/base/Extract.html) `call` for
3-dimensional array in `x[i, j, k]` notation.
