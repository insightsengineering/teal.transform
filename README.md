# teal.transform

<!-- start badges -->
[![CRAN Version](https://www.r-pkg.org/badges/version/teal.transform?color=green)](https://cran.r-project.org/package=teal.transform)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/teal.transform?color=green)](https://cran.r-project.org/package=teal.transform)
[![Last Month Downloads](http://cranlogs.r-pkg.org/badges/last-month/teal.transform?color=green)](https://cran.r-project.org/package=teal.transform)
[![Last Week Downloads](http://cranlogs.r-pkg.org/badges/last-week/teal.transform?color=green)](https://cran.r-project.org/package=teal.transform)

[![Check 🛠](https://github.com/insightsengineering/teal.transform/actions/workflows/check.yaml/badge.svg)](https://insightsengineering.github.io/teal.transform/main/unit-test-report/)
[![Docs 📚](https://github.com/insightsengineering/teal.transform/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/teal.transform/latest-tag/)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering/teal.transform/_xml_coverage_reports/data/main/badge.svg)](https://insightsengineering.github.io/teal.transform/main/coverage-report/)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/teal.transform?style=social)
![GitHub repo stars](https://img.shields.io/github/stars/insightsengineering/teal.transform?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/teal.transform)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/teal.transform)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/teal.transform)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/teal.transform)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/teal.transform)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/teal.transform)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering/teal.transform/main?color=purple\&label=package%20version)](https://github.com/insightsengineering/teal.transform/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/teal.transform?color=red\&label=open%20issues)](https://github.com/insightsengineering/teal.transform/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

This package contains functions and shiny modules for extracting and merging data within the `teal` framework.

## Installation

```r
# stable versions
install.packages('teal.transform')

# install.packages("pak")
pak::pak("insightsengineering/teal.transform@*release")
```

Alternatively, you might want to use the development version available on [r-universe](https://r-universe.dev/).

```r
# beta versions
install.packages('teal.transform', repos = c('https://pharmaverse.r-universe.dev', getOption('repos')))

# install.packages("pak")
pak::pak("insightsengineering/teal.transform")
```

## Usage

To understand how to use this package, please refer to the [Getting Started](https://insightsengineering.github.io/teal.transform/latest-tag/articles/teal-transform.html) article, which provides multiple examples of code implementation.

Below is a small example usage:

```r
library(teal.transform)
ADSL <- teal.transform::rADSL

adsl_extract <- data_extract_spec(
  dataname = "ADSL",
  filter = filter_spec(vars = "SEX", choices = c("F", "M")),
  select = select_spec(choices = c("BMRKR1", "AGE"))
)

ui <- data_extract_ui(
  id = "adsl_ui",
  label = "ADSL UI",
  data_extract_spec = adsl_extract
)

library(shiny)
ui <- fluidPage(ui)
server <- function(input, output, session) {}
shinyApp(ui, server)
```

![Showcase](https://github.com/insightsengineering/teal.transform/blob/main/assets/img/showcase.jpg)

## Getting help

If you encounter a bug or have a feature request, please file an issue. For questions, discussions, and staying up to date, please use the `teal` channel in the [`pharmaverse` slack workspace](https://pharmaverse.slack.com).

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.transform.svg)](https://starchart.cc/insightsengineering/teal.transform)

### Stargazers

[![Stargazers repo roster for @insightsengineering/teal.transform](http://reporoster.com/stars/insightsengineering/teal.transform)](https://github.com/insightsengineering/teal.transform/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/teal.transform](http://reporoster.com/forks/insightsengineering/teal.transform)](https://github.com/insightsengineering/teal.transform/network/members)
