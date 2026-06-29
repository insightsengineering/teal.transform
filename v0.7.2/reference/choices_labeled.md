# Set "`<choice>:<label>`" type of names

This is often useful for
[`choices_selected()`](https://insightsengineering.github.io/teal.transform/reference/choices_selected.md)
as it marks up the drop-down boxes for
[`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html).

## Usage

``` r
choices_labeled(choices, labels, subset = NULL, types = NULL)

# S3 method for class 'choices_labeled'
print(x, ...)
```

## Arguments

- choices:

  (`character` or `factor` or `numeric` or `logical`) vector.

- labels:

  (`character`) vector containing labels to be applied to `choices`. If
  `NA` then "Label Missing" will be used.

- subset:

  (`character` or `factor` or `numeric` or `logical`) vector that is a
  subset of `choices`. This is useful if only a few variables need to be
  named. If this argument is used, the returned vector will match its
  order.

- types:

  (`character`) vector containing the types of the columns to be used
  for applying the appropriate icons to the
  [choices_selected](https://insightsengineering.github.io/teal.transform/reference/choices_selected.md)
  drop down box (e.g. "numeric").

- x:

  an object used to select a method.

- ...:

  further arguments passed to or from other methods.

## Value

Named `character` vector.

## Details

If either `choices` or `labels` are factors, they are coerced to
character. Duplicated elements from `choices` get removed.

## Methods (by generic)

- `print(choices_labeled)`: Print choices_labeled object

## Examples

``` r
library(teal.data)
#> Loading required package: teal.code
library(shiny)

ADSL <- rADSL
ADTTE <- rADTTE

choices1 <- choices_labeled(names(ADSL), col_labels(ADSL, fill = FALSE))
choices2 <- choices_labeled(ADTTE$PARAMCD, ADTTE$PARAM)

# if only a subset of variables are needed, use subset argument
choices3 <- choices_labeled(
  names(ADSL),
  col_labels(ADSL, fill = FALSE),
  subset = c("ARMCD", "ARM")
)

ui <- bslib::page_fluid(
  selectInput("c1",
    label = "Choices from ADSL",
    choices = choices1,
    selected = choices1[1]
  ),
  selectInput("c2",
    label = "Choices from ADTTE",
    choices = choices2,
    selected = choices2[1]
  ),
  selectInput("c3",
    label = "Arm choices from ADSL",
    choices = choices3,
    selected = choices3[1]
  )
)
server <- function(input, output) {}

if (interactive()) {
  shinyApp(ui, server)
}
```
