# Checks that the `extract_input` specification does not allow multiple selection

Stops if condition not met.

## Usage

``` r
check_no_multiple_selection(extract_input)
```

## Arguments

- extract_input:

  (`list` or `NULL`) a list of `data_extract_spec`

## Value

Raises an error when check fails, otherwise, it returns `NULL`,
invisibly.
