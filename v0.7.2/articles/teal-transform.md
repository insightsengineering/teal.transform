# Getting Started with teal.transform

## Introduction

The `teal.transform` package is an integral component of the `teal`
framework. It serves a dual purpose:

- For `teal` application developers, it provides a means to specify
  which dataset columns can be accessed through the user interface,
  streamlining column selection within their applications.
- For `teal` module developers, it offers a standardized user interface
  for column selection from datasets and facilitates dataset merging,
  resulting in the creation of analysis datasets for use within their
  modules.

## Data Extraction and Data Merging

The primary goal of `teal.transform` is to provide functions that help
in abstracting the process of Data Extraction and Data Merging in UI
elements of the `shiny` app. This helps in reducing the amount of code
required to create a UI elements that directly transform the data to
perform the required analysis. This is how the app user gains
flexibility to transform their data right from the UI.

To explore the combined use of data extraction and merging to see the
full use of `teal.transform`, please explore the [Combining data-extract
with
data-merge](https://insightsengineering.github.io/teal.transform/articles/combining-data-extract-with-data-merge.md)
vignette.

To delve into the process of selecting specific data columns, please
consult the [Data
Extraction](https://insightsengineering.github.io/teal.transform/articles/data-extract.md)
vignette.

For comprehensive information regarding data merging, please refer to
the [Data
Merge](https://insightsengineering.github.io/teal.transform/articles/data-merge.md)
vignette.
