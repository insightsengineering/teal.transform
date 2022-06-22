adsl_df <- as.data.frame(as.list(stats::setNames(nm = teal.data::get_cdisc_keys("ADSL"))))
adsl <- teal.data::cdisc_dataset("ADSL", adsl_df)
adlb <- teal.data::cdisc_dataset("ADLB", adsl_df)

datasets <- teal.slice:::CDISCFilteredData$new()
datasets$set_dataset(adsl)
datasets$set_dataset(adlb)

adsl_extract <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    label = "Select variable:",
    choices = variable_choices(adsl, teal.data::get_cdisc_keys("ADSL")),
    selected = "STUDYID",
    multiple = TRUE,
    fixed = FALSE
  )
)

adlb_extract <- data_extract_spec(
  dataname = "ADLB",
  select = select_spec(
    label = "Select variable:",
    choices = c("STUDYID"),
    selected = "STUDYID",
    multiple = TRUE,
    fixed = FALSE
  )
)

testthat::test_that("data_extract_srv returns list of elements", {
  shiny::testServer(
    data_extract_srv,
    args = list(id = "x", data_extract_spec = adsl_extract, datasets = datasets),
    expr = {
      testthat::expect_is(session$returned(), "list")
      testthat::expect_identical(
        names(session$returned()),
        c("filters", "select", "always_selected", "reshape", "dataname", "internal_id", "keys")
      )
    }
  )
})

testthat::test_that("data_extract_srv throws error with missing arguments", {
  testthat::expect_error(
    shiny::testServer(
      data_extract_srv,
      args = list(id = "x", datasets = datasets),
      expr =  NULL
    ),
    "argument \"data_extract_spec\" is missing, with no default"
  )

  testthat::expect_error(
    shiny::testServer(
      data_extract_srv,
      args = list(id = "x", data_extract_spec = adsl_extract),
      expr =  NULL
    ),
    "argument \"datasets\" is missing, with no default"
  )
})

testthat::test_that("data_extract_srv throws error with wrong argument input type", {
  testthat::expect_error(
    shiny::testServer(
      data_extract_srv,
      args = list(id = "x", data_extract_spec = c("data_extract"), datasets = datasets),
      expr =  NULL
    ),
    regexp = "`data_extract_spec` argument should be"
  )

  testthat::expect_error(
    shiny::testServer(
      data_extract_srv,
      args = list(id = "x", data_extract_spec = TRUE, datasets = datasets),
      expr =  NULL
    ),
    regexp = "`data_extract_spec` argument should be"
  )

  testthat::expect_error(
    shiny::testServer(
      data_extract_srv,
      args = list(id = "x", data_extract_spec = adsl_extract, datasets = adsl),
      expr =  NULL
    ),
    regexp = "Assertion on 'datasets' failed:"
  )
})

testthat::test_that("data_extract_srv uses the current session id when id is missing", {
  shiny::testServer(
    data_extract_srv,
    args = list(id = "adsl_extract", data_extract_spec = adsl_extract, datasets = datasets),
    expr = {
      testthat::expect_is(session$returned(), "list")
      testthat::expect_identical(
        names(session$returned()),
        c("filters", "select", "always_selected", "reshape", "dataname", "internal_id", "keys")
      )
    }
  )
})

filtered_data <- teal.slice:::FilteredData$new()
filtered_data$set_dataset(teal.data::dataset("iris", iris))

testthat::test_that("data_extract_multiple_srv accepts a named list of `data_extract_spec`", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = testthat::expect_error(
      data_extract_multiple_srv(list(test = data_extract_spec(dataname = "iris")), datasets = filtered_data),
      NA
    )
  )
})

testthat::test_that("data_extract_multiple_srv returns a reactive list with reactives", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = {
      selector_list <- data_extract_multiple_srv(
        list(test = data_extract_spec(dataname = "iris")),
        datasets = filtered_data
      )
      testthat::expect_true(inherits(selector_list, "reactive"))
      lapply(isolate(selector_list()), FUN = function(element) testthat::expect_true(inherits(element, "reactive")))
    }
  )
})

testthat::test_that("data_extract_multiple_srv returns a named list", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = {
      selector_list <-
        data_extract_multiple_srv(list(test = data_extract_spec(dataname = "iris")), datasets = filtered_data)
      testthat::expect_equal(
        names(isolate(selector_list())),
        "test"
      )
    }
  )
})

testthat::test_that("data_extract_multiple_srv accepts an empty list", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = testthat::expect_error(data_extract_multiple_srv(list(), datasets = filtered_data), NA)
  )
})

testthat::test_that("data_extract_multiple_srv returns an empty list if passed an empty list", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = {
      selector_list <- data_extract_multiple_srv(list(), datasets = filtered_data)
      testthat::expect_equal(isolate(selector_list()), list())
    }
  )
})

testthat::test_that("data_extract_multiple_srv prunes `NULL` from the passed list", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = testthat::expect_equal(
      length(data_extract_multiple_srv(
        list(test = data_extract_spec(dataname = "iris"), test2 = NULL),
        datasets = filtered_data
      )),
      1
    )
  )
})

testthat::test_that("data_extract_multiple_srv throws if datasets is not passed FilteredData", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = testthat::expect_error(
      data_extract_multiple_srv(list(test = NULL), datasets = "wrong type"),
      regexp = "Assertion on 'datasets' failed:"
    )
  )
})

testthat::test_that("data_extract_multiple_srv throws if data_extract is not a named list", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = testthat::expect_error(
      data_extract_multiple_srv(list(1), datasets = teal.slice:::FilteredData$new()),
      regexp = "Assertion on 'data_extract' failed: Must have names"
    )
  )
})

testthat::test_that("data_extract_srv returns select ordered according to selection", {
  extract_ordered <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(adsl, teal.data::get_cdisc_keys("ADSL")),
      selected = "STUDYID",
      ordered = TRUE
    )
  )

  shiny::testServer(
    data_extract_srv,
    args = list(id = "x", data_extract_spec = extract_ordered, datasets = datasets),
    expr = {
      session$setInputs(`dataset_ADSL_singleextract-select` = c("b", "c"))
      testthat::expect_identical(filter_and_select_reactive()$select, c("b", "c"))

      session$setInputs(`dataset_ADSL_singleextract-select` = "c")
      testthat::expect_identical(filter_and_select_reactive()$select, "c")

      session$setInputs(`dataset_ADSL_singleextract-select` = c("b", "c"))
      testthat::expect_identical(filter_and_select_reactive()$select, c("c", "b"))
    }
  )
})

testthat::test_that("data_extract_srv returns select ordered according to choices", {
  extract_unordered <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(adsl, teal.data::get_cdisc_keys("ADSL")),
      selected = "STUDYID",
      ordered = FALSE
    )
  )

  shiny::testServer(
    data_extract_srv,
    args = list(id = "x", data_extract_spec = extract_unordered, datasets = datasets),
    expr = {
      session$setInputs(`dataset_ADSL_singleextract-select` = c("b", "c"))
      testthat::expect_identical(filter_and_select_reactive()$select, c("b", "c"))

      session$setInputs(`dataset_ADSL_singleextract-select` = "c")
      testthat::expect_identical(filter_and_select_reactive()$select, "c")

      session$setInputs(`dataset_ADSL_singleextract-select` = c("b", "c"))
      testthat::expect_identical(filter_and_select_reactive()$select, c("b", "c"))
    }
  )
})

testthat::test_that("data_extract_srv with a list of multiple data_extract_spec", {
  extract_list <- list(adsl_extract = adsl_extract, adlb_extract = adlb_extract)

  shiny::testServer(
    data_extract_srv,
    args = list(id = "x", data_extract_spec = extract_list, datasets = datasets),
    expr = {
      session$setInputs(`dataset` = "ADLB")
      testthat::expect_identical(input$dataset, "ADLB")
      testthat::expect_identical(filter_and_select_reactive()$dataname, "ADLB")

      session$setInputs(`dataset` = "ADSL")
      testthat::expect_identical(input$dataset, "ADSL")
      testthat::expect_identical(filter_and_select_reactive()$dataname, "ADSL")
    }
  )
})
