# Returns a reactive list with values read from the inputs of `data_extract_spec`

Returns a reactive list with values read from the inputs of
`data_extract_spec`

## Usage

``` r
data_extract_read_srv(
  id,
  datasets,
  single_data_extract_spec,
  iv,
  select_validation_rule = NULL,
  filter_validation_rule = NULL
)
```

## Arguments

- id:

  (`character`) id string.

- datasets:

  (`named list`) a list of reactive `data.frame` type objects.

- single_data_extract_spec:

  (`data_extract_spec`) the
  [`data_extract_spec()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_spec.md)
  object to handle.

## Value

[`shiny::reactive`](https://rdrr.io/pkg/shiny/man/reactive.html) the
reactive list with reactive values read from the UI.

## Details

Reads the UI inputs of a single `data_extract_spec` object in a running
`teal` application. Returns a reactive list of reactive values read from
the input.

The returned list has keys corresponding to the UI inputs: `select`,
`filters`, `always_selected`, `reshape`.
