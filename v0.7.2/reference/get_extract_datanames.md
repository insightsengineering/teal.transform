# Gets names of the datasets from a list of `data_extract_spec` objects

Fetches `dataname` slot per `data_extract_spec` from a list of
`data_extract_spec`.

## Usage

``` r
get_extract_datanames(data_extracts)
```

## Arguments

- data_extracts:

  (`data_extract_spec(1)`) object or a list (of lists) of
  `data_extract_spec`.

## Value

`character` vector with the unique `dataname` set.
