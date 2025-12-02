# Gets the relabel call

Gets the relabel call

## Usage

``` r
get_anl_relabel_call(columns_source, datasets, anl_name = "ANL")
```

## Arguments

- columns_source:

  (named `list`) where names are column names, values are labels +
  additional attribute `dataname`

- datasets:

  (named `list` of `reactive` or non-`reactive` `data.frame`) object
  containing data as a list of `data.frame`. When passing a list of
  non-reactive `data.frame` objects, they are converted to reactive
  `data.frame` objects internally.

- anl_name:

  (`character(1)`) Name of the analysis dataset.

## Value

(`call`) to relabel `dataset` and assign to `anl_name`.
