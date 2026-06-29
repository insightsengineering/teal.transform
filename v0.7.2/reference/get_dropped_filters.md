# Names of filtered-out filters dropped from selection

Names of filtered-out filters dropped from selection

## Usage

``` r
get_dropped_filters(selector)
```

## Arguments

- selector:

  one element of selector_list obtained by `get_dplyr_call_data`.

## Value

Vector of `character` names of the filters which should be dropped from
select call.

## Details

Names of filtered-out filters dropped from automatic selection (key vars
are automatically included in select). Dropped filter is filter which
became not unique for all observations. This means that if variable is
filtered to just one level, it's not a key anymore.

Other variables used in filter should also be dropped from automatic
selection, unless they have been selected.
