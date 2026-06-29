# Gets keys vector from keys list

Gets keys vector from keys list

## Usage

``` r
get_merge_key_pair(selector_from, selector_to, key_from)
```

## Arguments

- selector_from:

  (`list`) of `data_extract_srv` objects.

- selector_to:

  (`list`) of `data_extract_srv` objects.

- key_from:

  (`character`) keys used in the first selector while joining.

## Value

`character` vector of selector keys.

## Details

This function covers up to now 4 cases:

- Dataset without parent: Primary keys are returned;

- Dataset source = dataset target: The primary keys subtracted of all
  key columns that get purely filtered. This means just one value would
  be left after filtering inside this column Then it can be taken out;

- Target `dataname` is parent foreign keys;

- Any other case foreign keys;
