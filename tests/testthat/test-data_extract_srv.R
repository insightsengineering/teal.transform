adsl_df <- as.data.frame(as.list(stats::setNames(nm = teal.data::get_cdisc_keys("ADSL"))))
adsl <- teal.data::cdisc_dataset("ADSL", adsl_df)
adlb <- teal.data::cdisc_dataset("ADLB", adsl_df)

datasets <- teal.slice::init_filtered_data(
  list(ADSL = list(dataset = adsl_df, keys = teal.data::get_cdisc_keys("ADSL"), parent = character(0))),
  cdisc = TRUE
)

data_list <- sapply(X = datasets$datanames(), simplify = FALSE, FUN = function(x) {
  reactive(datasets$get_data(dataname = x, filtered = FALSE))
})

nr_data_list <- sapply(X = datasets$datanames(), simplify = FALSE, FUN = function(x) {
  datasets$get_data(dataname = x, filtered = FALSE)
})

key_list <- sapply(X = datasets$datanames(), simplify = FALSE, FUN = function(x) {
  isolate(datasets$get_keys(dataname = x))
})

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

testthat::test_that(
  desc = "data_extract_srv accepts a FilteredData object or a list of (reactive) data frames to datasets",
  code = {
    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        data_extract_srv(id = "x", data_extract_spec = adsl_extract, datasets = datasets),
        NA
      )
    )

    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        data_extract_srv(id = "x", data_extract_spec = adsl_extract, datasets = data_list, keys = key_list),
        NA
      )
    )

    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        data_extract_srv(id = "x", data_extract_spec = adsl_extract, datasets = nr_data_list, keys = key_list),
        NA
      )
    )

    mixed_data_list <- list(ADSL = reactive(datasets$get_data(dataname = "ADSL", filtered = FALSE)), ADLB = adsl_df)
    mixed_key_list <- list(
      ADSL = isolate(datasets$get_keys(dataname = "ADSL")),
      ADLB = isolate(datasets$get_keys(dataname = "ADSL"))
    )
    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        data_extract_srv(id = "x", data_extract_spec = adsl_extract, datasets = mixed_data_list, keys = mixed_key_list),
        NA
      )
    )
  }
)

testthat::test_that(
  desc = "data_extract_srv accepts throws error when a list of reactive data frames is provided with no keys argument",
  code = {
    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        data_extract_srv(id = "x", data_extract_spec = adsl_extract, datasets = data_list),
        "argument \"keys\" is missing, with no default"
      )
    )
  }
)

testthat::test_that(
  desc = "data_extract_srv accepts throws error when keys argument is not a named list",
  code = {
    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        data_extract_srv(id = "x", data_extract_spec = adsl_extract, datasets = data_list, keys = "key_list"),
        regexp = "Assertion on 'keys' failed: Must be of type 'list', not 'character'.",
        fixed = TRUE
      )
    )

    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        data_extract_srv(
          id = "x",
          data_extract_spec = adsl_extract,
          datasets = data_list, keys = list(c("USUBJID"))
        ),
        regexp = "Assertion on 'keys' failed: Must have names.",
        fixed = TRUE
      )
    )
  }
)

testthat::test_that(
  desc = "data_extract_srv throws error when names of datasets list and keys list do no correspond",
  code = {
    key_list <- list(X = c("STUDYID", "USUBJID"))

    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        data_extract_srv(id = "x", data_extract_spec = adsl_extract, datasets = data_list, keys = key_list),
        regexp = "Assertion on 'names(datasets)' failed",
        fixed = TRUE
      )
    )
  }
)

testthat::test_that("data_extract_srv returns list of elements", {
  shiny::testServer(
    data_extract_srv,
    args = list(id = "x", data_extract_spec = adsl_extract, datasets = nr_data_list, keys = key_list),
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
      args = list(id = "x", datasets = nr_data_list, keys = key_list),
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
      args = list(id = "x", data_extract_spec = c("data_extract"), datasets = nr_data_list, keys = key_list),
      expr =  NULL
    ),
    regexp = "has class 'character'"
  )

  testthat::expect_error(
    shiny::testServer(
      data_extract_srv,
      args = list(id = "x", data_extract_spec = TRUE, datasets = nr_data_list, keys = key_list),
      expr =  NULL
    ),
    regexp = "has class 'logical'"
  )

  testthat::expect_error(
    shiny::testServer(
      data_extract_srv,
      args = list(id = "x", data_extract_spec = adsl_extract, datasets = adsl, keys = key_list),
      expr =  NULL
    ),
    regexp = "Assertion on 'datasets' failed:"
  )
})

testthat::test_that("data_extract_srv uses the current session id when id is missing", {
  shiny::testServer(
    data_extract_srv,
    args = list(id = "adsl_extract", data_extract_spec = adsl_extract, datasets = nr_data_list, keys = key_list),
    expr = {
      testthat::expect_is(session$returned(), "list")
      testthat::expect_identical(
        names(session$returned()),
        c("filters", "select", "always_selected", "reshape", "dataname", "internal_id", "keys")
      )
    }
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

datasets <- teal.slice::init_filtered_data(
  list(
    ADSL = list(dataset = adsl_df, keys = teal.data::get_cdisc_keys("ADSL"), parent = character(0)),
    ADLB = list(dataset = adsl_df, keys = teal.data::get_cdisc_keys("ADLB"), parent = character(0))
  ),
  cdisc = TRUE
)
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
