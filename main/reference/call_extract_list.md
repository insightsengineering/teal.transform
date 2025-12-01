# Compose extract call with `$` operator

Compose extract call with `$` operator

## Usage

``` r
call_extract_list(dataname, varname, dollar = TRUE)
```

## Arguments

- dataname:

  (`character(1)` or `name`) name of the object.

- varname:

  (`character(1)` or `name`) name of the slot in data.

- dollar:

  (`logical(1)`) whether returned call should use `$` or `[[` operator.

## Value

[`Extract()`](https://rdrr.io/r/base/Extract.html) `call` in `$` or `[[`
notation (depending on parameters).
