# Resolve expression after delayed data are loaded

Resolve expression after delayed data are loaded

## Usage

``` r
resolve_delayed_expr(x, ds, is_value_choices)
```

## Arguments

- x:

  (`function`) Function that is applied on dataset. It must take only a
  single argument "data" and return character vector with columns /
  values.

- ds:

  (`data.frame`) Dataset.

- is_value_choices:

  (`logical`) Determines which check of the returned value will be
  applied.

## Value

`character` vector - result of calling function `x` on dataset `ds`.
