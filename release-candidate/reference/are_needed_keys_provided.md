# Checks whether the provided slices have the corresponding join keys

Checks whether the provided slices have the corresponding join keys

## Usage

``` r
are_needed_keys_provided(join_keys, merged_selector_list)
```

## Arguments

- join_keys:

  (`join_keys`) the provided join keys.

- merged_selector_list:

  (`list`) the specification of datasets' slices to merge.

## Value

`TRUE` if all pairs of the slices have the corresponding keys and
`FALSE` otherwise.

## Note

`merged_selector_list` contains a list of descriptions of data frame
slices; each coming from a single dataset. This function checks whether
all pairs of the datasets have the join keys needed to merge the slices.
