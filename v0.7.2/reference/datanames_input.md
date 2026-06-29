# Help text with available datasets input

Creates
[`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html) with
the names of available datasets for the current module.

## Usage

``` r
datanames_input(data_extracts)
```

## Arguments

- data_extracts:

  (`list`) of data extracts for single variable.

## Value

`shiny.tag` defining help-text element that can be added to a UI
element.
