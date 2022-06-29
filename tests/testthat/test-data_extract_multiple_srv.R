datasets <- teal.slice::init_filtered_data(
  list(iris = list(dataset = iris))
)

data_list <- sapply(X = datasets$datanames(), simplify = FALSE, FUN = function(x) {
  shiny::reactive(datasets$get_data(dataname = x, filtered = FALSE))
})

nr_data_list <- sapply(X = datasets$datanames(), simplify = FALSE, FUN = function(x) {
  shiny::isolate(datasets$get_data(dataname = x, filtered = FALSE))
})

key_list <- sapply(X = datasets$datanames(), simplify = FALSE, FUN = function(x) {
  shiny::isolate(datasets$get_keys(dataname = x))
})

testthat::test_that("data_extract_multiple_srv accepts a named list of `data_extract_spec`", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = testthat::expect_error(
      data_extract_multiple_srv(
        data_extract = list(test = data_extract_spec(dataname = "iris")),
        datasets = data_list,
        join_keys = key_list
      ),
      NA
    )
  )
})

testthat::test_that("data_extract_multiple_srv returns a named reactive list with reactives", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = {
      selector_list <- data_extract_multiple_srv(
        list(test = data_extract_spec(dataname = "iris")),
        datasets = data_list,
        join_keys = key_list
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
    expr = testthat::expect_error(data_extract_multiple_srv(list(), datasets = data_list, join_keys = key_list), NA)
  )
})

testthat::test_that("data_extract_multiple_srv returns an empty list if passed an empty list", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = {
      selector_list <- data_extract_multiple_srv(list(), datasets = data_list, join_keys = key_list)
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
        datasets = data_list,
        join_keys = key_list
      )),
      1
    )
  )
})

testthat::test_that("data_extract_multiple_srv accepts datasets as FilteredData or list of (reactive) data.frame", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = testthat::expect_error(
      data_extract_multiple_srv(data_extract = list(test = NULL), datasets = datasets),
      regexp = NA
    )
  )

  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = testthat::expect_error(
      data_extract_multiple_srv(data_extract = list(test = NULL), datasets = nr_data_list, join_keys = key_list),
      regexp = NA
    )
  )

  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = testthat::expect_error(
      data_extract_multiple_srv(data_extract = list(test = NULL), datasets = data_list, join_keys = key_list),
      regexp = NA
    )
  )

  mixed_data_list <- list(IRIS = reactive(iris), IRIS2 = iris)
  mixed_join_keys_list <- teal.data::join_keys(
    teal.data::join_key("IRIS", "IRIS", character(0)),
    teal.data::join_key("IRIS2", "IRIS2", character(0)),
    teal.data::join_key("IRIS", "IRIS2", character(0))
  )$get()

  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = testthat::expect_error(
      data_extract_multiple_srv(
        data_extract = list(test = NULL),
        datasets = mixed_data_list,
        join_keys = mixed_join_keys_list
      ),
      NA
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
      data_extract_multiple_srv(list(1), datasets = teal.slice::init_filtered_data(list(iris = list(dataset = iris)))),
      regexp = "Assertion on 'data_extract' failed: Must have names"
    )
  )
})

testthat::test_that(
  desc = "data_extract_multiple_srv works with join_keys = NULL (default) or join_keys = character(0)",
  code = {
    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_silent(
        data_extract_multiple_srv(list(test = NULL), datasets = data_list)
      )
    )

    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_silent(
        data_extract_multiple_srv(list(test = NULL), datasets = data_list, join_keys = character(0))
      )
    )
  }
)

testthat::test_that(
  desc = "data_extract_multiple_srv accepts throws error when join_keys argument is not a named list",
  code = {
    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        data_extract_multiple_srv(list(test = NULL), datasets = data_list, join_keys = "key_list"),
        regexp = "not 'character'.",
        fixed = TRUE
      )
    )

    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        data_extract_multiple_srv(list(test = NULL), datasets = data_list, join_keys = list(c("USUBJID"))),
        regexp = "Assertion on 'join_keys' failed: Must have names.",
        fixed = TRUE
      )
    )
  }
)

testthat::test_that(
  desc = "data_extract_multiple_srv throws error when names of datasets list and join_keys list do no correspond",
  code = {
    key_list <- list(X = c(""))

    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        data_extract_multiple_srv(list(test = NULL), datasets = data_list, join_keys = key_list),
        regexp = "Names must be a subset of",
        fixed = TRUE
      )
    )
  }
)
