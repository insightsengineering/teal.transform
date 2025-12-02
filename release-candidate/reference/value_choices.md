# Value labeling and filtering based on variable relationship

Wrapper on
[choices_labeled](https://insightsengineering.github.io/teal.transform/reference/choices_labeled.md)
to label variable values basing on other variable values.

## Usage

``` r
value_choices(data, var_choices, var_label = NULL, subset = NULL, sep = " - ")

# S3 method for class 'character'
value_choices(data, var_choices, var_label = NULL, subset = NULL, sep = " - ")

# S3 method for class 'data.frame'
value_choices(data, var_choices, var_label = NULL, subset = NULL, sep = " - ")
```

## Arguments

- data:

  (`data.frame`, `character`) If `data.frame`, then data to extract
  labels from. If `character`, then name of the dataset to extract data
  from once available.

- var_choices:

  (`character`, `delayed_variable_choices`) Choice of column names.

- var_label:

  (`character`) vector with labels column names.

- subset:

  (`character` or `function`) If `character`, vector with values to
  subset. If `function`, then this function is used to determine the
  possible columns (e.g. all factor columns). In this case, the function
  must take only single argument "data" and return a character vector.

  See examples for more details.

- sep:

  (`character`) separator used in case of multiple column names.

## Value

named character vector or `delayed_data` object.

## Examples

``` r
ADRS <- teal.data::rADRS
value_choices(ADRS, "PARAMCD", "PARAM", subset = c("BESRSPI", "INVET"))
#> number of choices: 2 
#> 
#> BESRSPI: Best Confirmed Overall Response by Investigator
#> INVET: Investigator End Of Induction Response
#> 
value_choices(ADRS, c("PARAMCD", "ARMCD"), c("PARAM", "ARM"))
#> number of choices: 9 
#> 
#> BESRSPI - ARM A: Best Confirmed Overall Response by Investigator - A: Drug X
#> INVET - ARM A: Investigator End Of Induction Response - A: Drug X
#> OVRINV - ARM A: Overall Response by Investigator - by visit - A: Drug X
#> BESRSPI - ARM C: Best Confirmed Overall Response by Investigator - C: Combination
#> INVET - ARM C: Investigator End Of Induction Response - C: Combination
#> OVRINV - ARM C: Overall Response by Investigator - by visit - C: Combination
#> BESRSPI - ARM B: Best Confirmed Overall Response by Investigator - B: Placebo
#> INVET - ARM B: Investigator End Of Induction Response - B: Placebo
#> OVRINV - ARM B: Overall Response by Investigator - by visit - B: Placebo
#> 
value_choices(ADRS, c("PARAMCD", "ARMCD"), c("PARAM", "ARM"),
  subset = c("BESRSPI - ARM A", "INVET - ARM A", "OVRINV - ARM A")
)
#> number of choices: 3 
#> 
#> BESRSPI - ARM A: Best Confirmed Overall Response by Investigator - A: Drug X
#> INVET - ARM A: Investigator End Of Induction Response - A: Drug X
#> OVRINV - ARM A: Overall Response by Investigator - by visit - A: Drug X
#> 
value_choices(ADRS, c("PARAMCD", "ARMCD"), c("PARAM", "ARM"), sep = " --- ")
#> number of choices: 9 
#> 
#> BESRSPI --- ARM A: Best Confirmed Overall Response by Investigator --- A: Drug X
#> INVET --- ARM A: Investigator End Of Induction Response --- A: Drug X
#> OVRINV --- ARM A: Overall Response by Investigator - by visit --- A: Drug X
#> BESRSPI --- ARM C: Best Confirmed Overall Response by Investigator --- C: Combination
#> INVET --- ARM C: Investigator End Of Induction Response --- C: Combination
#> OVRINV --- ARM C: Overall Response by Investigator - by visit --- C: Combination
#> BESRSPI --- ARM B: Best Confirmed Overall Response by Investigator --- B: Placebo
#> INVET --- ARM B: Investigator End Of Induction Response --- B: Placebo
#> OVRINV --- ARM B: Overall Response by Investigator - by visit --- B: Placebo
#> 

# delayed version
value_choices("ADRS", c("PARAMCD", "ARMCD"), c("PARAM", "ARM"))
#> value_choices with delayed data:  ADRS
#> $ data
#> [1] "ADRS"
#> $ var_choices
#> [1] "PARAMCD" "ARMCD"  
#> $ var_label
#> [1] "PARAM" "ARM"  
#> $ subset
#> NULL
#> $ sep
#> [1] " - "

# functional subset
value_choices(ADRS, "PARAMCD", "PARAM", subset = function(data) {
  levels(data$PARAMCD)[1:2]
})
#> number of choices: 2 
#> 
#> BESRSPI: Best Confirmed Overall Response by Investigator
#> INVET: Investigator End Of Induction Response
#> 
```
