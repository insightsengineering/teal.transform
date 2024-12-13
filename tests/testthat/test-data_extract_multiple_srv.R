ADSL <- teal.data::rADSL
ADLB <- teal.data::rADLB
ADTTE <- teal.data::rADTTE

data_list <- list(ADSL = reactive(ADSL), ADTTE = reactive(ADTTE), ADLB = reactive(ADLB))
join_keys <- teal.data::default_cdisc_join_keys[c("ADSL", "ADTTE", "ADLB")]

testthat::test_that("data_extract_multiple_srv accepts a named list of `data_extract_spec`", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = testthat::expect_no_error(
      data_extract_multiple_srv(
        data_extract = list(test = data_extract_spec(dataname = "iris")),
        datasets = data_list,
        join_keys = teal.data::join_keys()
      )
    )
  )
})

testthat::test_that("data_extract_multiple_srv returns a named reactive list with reactives", {
  data_list <- list(iris = reactive(iris))
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = {
      selector_list <- data_extract_multiple_srv(
        list(test = data_extract_spec(dataname = "iris")),
        datasets = data_list,
        join_keys = teal.data::join_keys()
      )
      testthat::expect_equal(names(isolate(selector_list())), "test")
      testthat::expect_true(inherits(selector_list, "reactive"))
      lapply(isolate(selector_list()), FUN = function(element) testthat::expect_true(inherits(element, "reactive")))
    }
  )
})

testthat::test_that("data_extract_multiple_srv accepts an empty list", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = testthat::expect_no_error(data_extract_multiple_srv(list(), datasets = data_list, join_keys = join_keys))
  )
})

testthat::test_that("data_extract_multiple_srv returns an empty list if passed an empty list", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = {
      selector_list <- data_extract_multiple_srv(list(), datasets = data_list, join_keys = join_keys)
      testthat::expect_equal(isolate(selector_list()), list())
    }
  )
})

testthat::test_that("data_extract_multiple_srv prunes `NULL` from the passed list", {
  data_list <- list(iris = reactive(iris))
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = testthat::expect_equal(
      length(data_extract_multiple_srv(
        list(test = data_extract_spec(dataname = "iris"), test2 = NULL),
        datasets = data_list
      )),
      1
    )
  )
})

testthat::test_that("data_extract_multiple_srv accepts datasets as FilteredData", {
  mock_datasets <- structure(
    list(
      datanames = function() names(data_list),
      get_data = function(dataname, ...) data_list[[dataname]](),
      get_join_keys = function(dataname, ...) join_keys
    ),
    class = "FilteredData"
  )
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = testthat::expect_no_error(
      data_extract_multiple_srv(data_extract = list(test = NULL), datasets = mock_datasets)
    )
  )
})

testthat::test_that("data_extract_multiple_srv accepts datasets list of reactive data.frame", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = testthat::expect_no_error(
      data_extract_multiple_srv(data_extract = list(test = NULL), datasets = data_list, join_keys = join_keys)
    )
  )
})

testthat::test_that("data_extract_multiple_srv accepts datasets as list of data.frame", {
  mixed_data_list <- list(IRIS = iris, IRIS2 = iris)
  mixed_join_keys_list <- teal.data::join_keys(
    teal.data::join_key("IRIS", "IRIS", "id"),
    teal.data::join_key("IRIS2", "IRIS2", "id"),
    teal.data::join_key("IRIS", "IRIS2", "id")
  )

  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = testthat::expect_no_error(
      data_extract_multiple_srv(
        data_extract = list(test = NULL),
        datasets = mixed_data_list,
        join_keys = mixed_join_keys_list
      )
    )
  )
})

testthat::test_that(
  desc = "data_extract_multiple_srv throws error if datasets is not FilteredData or list of (reactive) data.frame",
  code = {
    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        data_extract_multiple_srv(list(test = NULL), datasets = "wrong type"),
        regexp = "Assertion on 'datasets' failed"
      )
    )
  }
)

testthat::test_that("data_extract_multiple_srv throws if data_extract is not a named list", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = testthat::expect_error(
      data_extract_multiple_srv(list(1), datasets = data_list),
      regexp = "Assertion on 'data_extract' failed: Must have names"
    )
  )
})

testthat::test_that(
  desc = "data_extract_multiple_srv works with join_keys = NULL (default)",
  code = {
    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_silent(
        data_extract_multiple_srv(list(test = NULL), datasets = data_list)
      )
    )
  }
)

testthat::test_that(
  desc = "data_extract_multiple_srv accepts throws error when join_keys argument is not join_keys object",
  code = {
    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        data_extract_multiple_srv(list(test = NULL), datasets = data_list, join_keys = "key_list"),
        regexp = "class 'character'.",
        fixed = TRUE
      )
    )
  }
)
