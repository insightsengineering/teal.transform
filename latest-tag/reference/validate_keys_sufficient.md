# Validates whether the provided keys are sufficient to merge the datasets slices

Validates whether the provided keys are sufficient to merge the datasets
slices

## Usage

``` r
validate_keys_sufficient(join_keys, merged_selector_list)
```

## Arguments

- join_keys:

  (`join_keys`) the provided join keys.

- merged_selector_list:

  (`list`) the specification of datasets' slices to merge.

## Value

`TRUE` if the provided keys meet the requirement and `shiny` validate
error otherwise.

## Note

The keys are not sufficient if the datasets slices described in
`merged_selector_list` come from datasets, which don't have the
appropriate join keys in `join_keys`.
