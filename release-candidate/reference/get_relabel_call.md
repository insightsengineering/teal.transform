# Create relabel call from named character

Function creates relabel call from named character.

## Usage

``` r
get_relabel_call(labels)
```

## Arguments

- labels:

  (named `character`) where name is name is function argument name and
  value is a function argument value.

## Value

`call` object with relabel step.

## Examples

``` r
get_relabel_call(
  labels = c(
    x = as.name("ANL"),
    AGE = "Age",
    AVAL = "Continuous variable"
  )
)
#> teal.data::col_relabel(x = ANL, AGE = "Age", AVAL = "Continuous variable")

get_relabel_call(
  labels = c(
    AGE = "Age",
    AVAL = "Continuous variable"
  )
)
#> teal.data::col_relabel(AGE = "Age", AVAL = "Continuous variable")
```
