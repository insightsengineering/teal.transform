# Function to check data_extract_specs

Checks if `dataname` argument exists as a dataset. Checks if selected or
filter columns exist within the datasets. Throws a `shiny` validation
error if the above requirements are not met.

## Usage

``` r
check_data_extract_spec_react(datasets, data_extract)
```

## Arguments

- datasets:

  (`FilteredData`) the object created using the `teal` API.

- data_extract:

  (`list`) the output of the `data_extract` module.

## Value

`NULL`.
