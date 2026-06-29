# Verify uniform dataset source across data extract specification

Checks if the input `data_extract_spec` objects all come from the same
dataset.

## Usage

``` r
is_single_dataset(...)
```

## Arguments

- ...:

  either `data_extract_spec` objects or lists of `data_extract_spec`
  objects that do not contain `NULL`

## Value

`TRUE` if all `data_extract_spec` objects come from the same dataset,
`FALSE` otherwise.
