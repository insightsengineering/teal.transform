# Data Merge

Joining datasets is an essential step when working with relational
datasets.

To support this, two functions are provided depending on how to process
the `data_extract_spec` object:

1.  `merge_expression_module` can be used when there is no need to
    process the list of `data_extract_spec`. This function reads the
    data and the list of `data_extract_spec` objects and applies the
    merging. Essentially, it serves as a wrapper that combines
    [`data_extract_multiple_srv()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_multiple_srv.md)
    and
    [`merge_expression_srv()`](https://insightsengineering.github.io/teal.transform/reference/merge_expression_srv.md).
2.  `merge_expression_srv` and `data_extract_multiple_srv` can be used
    in scenarios where additional processing of the list of
    `data_extract_spec` is necessary or
    [`data_extract_srv()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_srv.md)
    to customize the `selector_list` input.

The following sections provide examples for both scenarios.

### `merge_expression_module`

Using `merge_expression_module` alone requires a list of
`data_extract_spec` objects for the `data_extract` argument, a list of
reactive or non-reactive `data.frame` objects, and a list of join keys
corresponding to each `data.frame` object.

#### Step 1/5 - Preparing the Data

``` r
library(teal.transform)
library(teal.data)
#> Loading required package: teal.code
library(shiny)

# Define data.frame objects
ADSL <- teal.data::rADSL
ADTTE <- teal.data::rADTTE

# create a list of reactive data.frame objects
datasets <- list(
  ADSL = reactive(ADSL),
  ADTTE = reactive(ADTTE)
)

# create  join_keys
join_keys <- join_keys(
  join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")),
  join_key("ADSL", "ADTTE", c("STUDYID", "USUBJID")),
  join_key("ADTTE", "ADTTE", c("STUDYID", "USUBJID", "PARAMCD"))
)
```

#### Step 2/5 - Creating the Data Extracts

``` r
adsl_extract <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    label = "Select variable:",
    choices = c("AGE", "BMRKR1"),
    selected = "AGE",
    multiple = TRUE,
    fixed = FALSE
  )
)

adtte_extract <- data_extract_spec(
  dataname = "ADTTE",
  select = select_spec(
    choices = c("AVAL", "ASEQ"),
    selected = "AVAL",
    multiple = TRUE,
    fixed = FALSE
  )
)

data_extracts <- list(adsl_extract = adsl_extract, adtte_extract = adtte_extract)
```

#### Step 3/5 - Creating the UI

``` r
merge_ui <- function(id, data_extracts) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3("Encoding"),
      tags$div(
        data_extract_ui(
          ns("adsl_extract"), # must correspond with data_extracts list names
          label = "ADSL extract",
          data_extracts[[1]]
        ),
        data_extract_ui(
          ns("adtte_extract"), # must correspond with data_extracts list names
          label = "ADTTE extract",
          data_extracts[[2]]
        )
      )
    ),
    mainPanel(
      h3("Output"),
      verbatimTextOutput(ns("expr")),
      dataTableOutput(ns("data"))
    )
  )
}
```

#### Step 4/5 - Creating the Server Logic

``` r
merge_srv <- function(id, datasets, data_extracts, join_keys) {
  moduleServer(id, function(input, output, session) {
    merged_data <- merge_expression_module(
      data_extract = data_extracts,
      datasets = datasets,
      join_keys = join_keys,
      merge_function = "dplyr::left_join"
    )

    ANL <- reactive({
      data_list <- lapply(datasets, function(ds) ds())
      eval(envir = list2env(data_list), expr = as.expression(merged_data()$expr))
    })
    output$expr <- renderText(paste(merged_data()$expr, collapse = "\n"))
    output$data <- renderDataTable(ANL())
  })
}
```

#### Step 5/5 - Creating the `shiny` App

``` r
shinyApp(
  ui = bslib::page_fluid(merge_ui("data_merge", data_extracts)),
  server = function(input, output, session) {
    merge_srv("data_merge", datasets, data_extracts, join_keys)
  }
)
```

[Open in
Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajU5iAM4AzIoxgBKADoQ6TFhy48AJlFJQpMhszbsBAC1oRWG6QGIABABE4Qw3HMq1GIc3jmi9AFZwCpAdIBBSwBlABlzAB4AWnNFDEcoRERGILDAywAVDIBRSJi4hKSUzJzpM3MCRjhVeyhzOgFSdyFzKqhfWgA3ewTnV3sPb19-CASBOD88+tpG9mlzc1TwgF5W6o7u9iXxXHnFktzVto24LYONEwgLSuryBc8iQwB9AGs4VhGH57ePqa+IV7vARzCD3R4An5zMBLSR4cywmFwghQ4IZACqlgAmgBJSyw3DwsBo4JogBCAClcbDxDs9v9ARwESFQvjCUEstlWcjYaiMTi8XDYcSyZSBTTdqDzPTIUyOazZaUkSj0ViqYKiSSKWqCbCAAoBLABACyAGExRdpFAlAJqE84AAPPjtJrRByqKB2x3MXxPASoHwghYJaBuVZMtJ4Pbjag+JqraOx33+7mShbUKD0ODUcxhsDBLOx8ydFi0DMxxD4vYLAh6R4EOACHMVKEBADinPVpKNWAA0lgAIzUiULBYJ3xwJRNhHtyup8wwACu1FItFQMabGSwaOyw5HtntE6bADEAqFgpzJRaIJalKRyJ6nb4pgkH97SEmA3tg7B7Ln2YrdzHONzCAj8UxHCpa1oetG1WbloQANVPeVoXPABFIcqxAgtx0nP8kJZSM50XZdV3XVZN23XcFn3Q9VhPM8LwWK9pBfB1H0mV0GlIdgrRtV9nSbPjbXYt8CStO84AEp9Vgk+9ROdDR4EYABzKSF1oKYhAXCAOhIdhaCUAk2K9Z0BHEcwQD2QQpgAOWCAylA0UdDLgegWFCKBWCIBceKwgRXPcxhdSgCAs0DCDzD0ABmKFsl0oglEMFTMLnBY1BUgQABIks6CLIrdNRpPfDT8oK8xBChYTiqHcwLEXRoKlEKo-RIScAHdaFIPRCo9BThmmRqQwbLCCvTTNsz-ZlzH60hZ3KkcTI4gRgGAfsAF11tGkdaTSxb3WKp5Su2iDKthOSpNm2r6oXRriEYFrUDa8xOu63risbbiKp-EYFrTDMsynaEDhm0zfHmv6lrfFbgAAJk2k7zGcyLkeY6j5ygQwQrC6gyoWGKoQAeV81BfNSgrukYdyVxgDJ2OJ0hSZ4s6wAdVBGGpXaCoSDIyzgBmmfYFmEk5rDkY0ABfaRlLU31GE6LSdL0iBHOM91xj8NWitmgQCWlIELKsyUYESpc4HzeW4EYVXzG03SV30wwmYJHzGd8glxgEAKSENrCZYnJ4Eimf3PXZhtvYBE2lDNvH3tmpsobM9Gg3ViZYN6jXde2-XflWHOs72kO7eVoGlDXVhGCSGMhHff5YTFso5wCWzwldY4V02I3uYOr6uLQcv2DGNOCWLh2VetCzrXYGltrgYtcYoTpaEYJtuNhxfB57mZSB2UH2aEgQMDZlqI-Yf2lED91p6y4+Z7nCXUfcEnfJv+197biglCtunHXYVAoEaKcc+l81DX2PgSYg1B0yoHGEDSQZQwB3wgq7JmOV3RTCqKMK2lh3S83oDGLYLdp7IwftIKWggDBGACOgCKGkmz0BtAwJI-9ZZCGoBpJQZ8ray2OmAF8-tWSJ2GOKKMVtKZNlHrQR2EBnZPzdqQD24cpEQF9sRbhUkBDyyhPw9RgjU6azjmDAx+cSHSHEGACW60gA)

------------------------------------------------------------------------

### `data_extract_multiple_srv` + `merge_expression_srv`

In the scenario above, if the user deselects the `ADTTE` variable, the
merging between `ADTTE` and `ADSL` would still occur, even though
`ADTTE` is not used or needed. Here, the developer might update the
`selector_list` input in a reactive manner so that it gets updated based
on conditions set by the developer. Below, we reuse the input from above
and update the app server so that the `adtte_extract` is removed from
the selector_list input when no `ADTTE` variable is selected. The
`reactive_selector_list` is then passed to `merge_expression_srv`:

#### Modifying the Server Logic

``` r
merge_srv <- function(id, datasets, data_extracts, join_keys) {
  moduleServer(id, function(input, output, session) {
    selector_list <- data_extract_multiple_srv(data_extracts, datasets, join_keys)
    reactive_selector_list <- reactive({
      if (is.null(selector_list()$adtte_extract) || length(selector_list()$adtte_extract()$select) == 0) {
        selector_list()[names(selector_list()) != "adtte_extract"]
      } else {
        selector_list()
      }
    })

    merged_data <- merge_expression_srv(
      selector_list = reactive_selector_list,
      datasets = datasets,
      join_keys = join_keys,
      merge_function = "dplyr::left_join"
    )

    ANL <- reactive({
      data_list <- lapply(datasets, function(ds) ds())
      eval(envir = list2env(data_list), expr = as.expression(merged_data()$expr))
    })
    output$expr <- renderText(paste(merged_data()$expr, collapse = "\n"))
    output$data <- renderDataTable(ANL())
  })
}
```

#### Updating the `shiny` app

``` r
shinyApp(
  ui = bslib::page_fluid(merge_ui("data_merge", data_extracts)),
  server = function(input, output, session) {
    merge_srv("data_merge", datasets, data_extracts, join_keys)
  }
)
```

[Open in
Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajU5iAM4AzIoxgBKADoQ6TFhy48AJlFJQpMhszbsBAC1oRWG6QGIABABE4Qw3HMq1GIc3jmi9AFZwCpAdIBBSwBlABlzAB4AWnNFDEcoRERGILDAywAVDIBRSJi4hKSUzJzpM3MCRjhVeyhzOgFSdyFzKqhfWgA3ewTnV3sPb19-CASBOD88+tpG9mlzc1TwgF5W6o7u9iXxXHnFktzVto24LYONEwgLSuryBc8iQwB9AGs4VhGH57ePqa+IV7vARzCD3R4An5zMBLSR4cywmFwghQ4IZACqlgAmgBJSyw3DwsBo4JogBCAClcbDxDs9v9ARwESFQvjCUEstlWcjYaiMTi8XDYcSyZSBTTdqDzPTIUyOazZaUkSj0ViqYKiSSKWqCbCAAoBLABACyAGExRdpFAlAJqE84AAPPjtJrRByqKB2x3MXxPASoHwghYJaBuVZMtJ4Pbjag+JqraOx33+7mShbUKD0ODUcxhsDBLOx8ydFi0DMxxD4vYLAh6R4EOACHMVKEBADinPVpKNWAA0lgAIzUiULBYJ3xwJRNhHtyup8wwACu1FItFQMabGSwaOyw5HtntE6bADEAqFgpzJRaIJalKRyJ6nb4pgkH97SEmA3tg7B7Ln2YrdzHONzCAj8UxHCpa1oetG1WbloQANVPeVoXPABFIcqxAgtx0nP8kJZSM50XZdV3XVZN23XcFn3Q9VhPM8LwWK9pBfB1H0mV0GlIdgrRtV9nSbPjbXYt8CStO84AEp9Vgk+9ROdDR4EYABzKSF1oKYhAXCAOhIdhaCUAk2K9Z0BHEcwQD2QQpgAOWCAylA0UdDLgegWFCKBWCIBceKwgRXPcxhdSgCAs0DCDzD0ABmKFsl0oglEMFTMLnBY1BUgQABIks6CLIrdNRpPfDT8oK8xBChYTiqHcwLEXRoKlEKo-RIScAHdaFIPRCo9BThmmRqQwbLCCvTTNsz-ZlzH60hZ3KkcTI4gRgGAfsAF11tGkdaTSxb3WKp5Su2iDKthOSpNm2r6oXRriEYFrUDa8xOu63risbbiKp-EYFrTDMsynaEDhm0zfHmv6lrfFbgAAJk2k7zGcyLkeY6j5ygQwQrC6gyoWGKoQAeV81BfNSgrukYdyVxgDJ2OJ0hSZ4s6wAdVBGGpXaCoSDIyzgBmmfYFmEk5rDkY0ABfaRlLU31GE6LSdL0iBHOM91xj8NWitmgQCWlIELKsyUYESpc4HzeW4EYVXzG03SV30wwmYJHzGd8glxgEAKSEN-ycNIUQni+10oedJ4SJXNcpIEeX2FD4YtagDXdalcEGXMrDjhXbpfX9wPg5iLOulOI2CtoFoDIEDAICXXGgPzmYePELKLuKiyAB92-qCgVO63Q88YIPG-YZvW9mkesqAizllWAAGX29tHAeh9mcRgGG4F68H7iR4sgBCXMx7BuawC2vaJZm6hxksxGt5XpvtqlucJcuCCZYnJ4Eimd-PXZhtvYBDHPK2075fSOOsbO0dl7cXRkGdWExYK9WTrA1O3wgRNn1h8FBP87bKyBkoNcrBGBJBjEId8-xYRizKHOAItlwiuiLpsUukUXwF3qGgQhcd4Ga1tkrB2KtrQWWtLvbacBiy4woJ0WgjAmzcVhpIrhRVuI7FBuzISVc2YtQAewd+ShP7ugnpomkWEX5YVdkzLKmiphVFGFbOmjp2CoCTuQHRVs1J6ISIY+07MCTEGoOmVA19cySDKGAYxc5zG+Ryu6axFAlBW0sO6Xm9AYxbDoSIyUpiIBP30IYVgAR0ARQ0k2egNoGBJCcbLIQ1ANJKFcapdStAoQvnfqyeOfhxRRitpTJsuD+EGQgM7dwJN3bYS9rQH2N9iJuOjrHWELSZltO4SndpKdMEZ0ydIcQYAJbrSAA)

`merge_expression_module` is replaced here with three parts:

1.  `selector_list`: the output of `data_extract_multiple_srv`, which
    loops over the list of `data_extract` given and runs
    `data_extract_srv` for each one, returning a list of reactive
    objects.
2.  `reactive_selector_list`: an intermediate reactive list updating
    `selector_list` content.
3.  `merged_data`: the output of `merge_expression_srv` using
    `reactive_selector_list` as input.

### Output from merging

Both merge functions, `merge_expression_srv` and
`merge_expression_module`, return a reactive object which contains a
list of the following elements:

- `expr`: code needed to replicate merged dataset.
- `columns_source`: list of columns selected per selector.
- `keys`: the keys of the merged dataset.
- `filter_info`: filters that are applied on the data.

These elements can be further used inside the server to retrieve and use
information about the selections, data, filters, etc.

## Merging of non `CDISC` datasets

General datasets do not have the same relationships as `CDISC` datasets,
so these relationships must be specified using the `join_keys`
functions. For more information, please refer to the `Join Keys`
[vignette](https://insightsengineering.github.io/teal.data/latest-tag/articles/join-keys).
The data merge module respects the relationships given by the user. In
the case of multiple datasets to merge, the order is specified by the
order of elements in the `data_extract` argument of the
`merge_expression_module` function. Merging groups of datasets with
complex relationships can quickly become challenging to specify so
please take extra care when setting this up.
