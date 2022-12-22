adsl <- as.data.frame(as.list(stats::setNames(nm = c(teal.data::get_cdisc_keys("ADSL"), "AGE"))))
adlb <- as.data.frame(
  as.list(stats::setNames(nm = c(teal.data::get_cdisc_keys("ADLB"), "AVAL", "CHG", "CHG2", "ABLFL")))
)

adsl <- teal.data::cdisc_dataset("ADSL", adsl)
adlb <- teal.data::cdisc_dataset("ADLB", adlb)
data <- teal.data::cdisc_data(adsl, adlb)

datasets_used <- teal.slice::init_filtered_data(data)

adsl_data_extract_srv_output <-
  list(
    dataname = "ADSL",
    filters = NULL,
    select = "AGE",
    keys = datasets_used$get_keys("ADSL"),
    reshape = FALSE,
    internal_id = "adsl_extract"
  )

adlb_data_extract_srv_output <-
  list(
    dataname = "ADLB",
    filters = NULL,
    select = c("AVAL", "CHG"),
    keys = datasets_used$get_keys("ADLB"),
    reshape = FALSE,
    internal_id = "adlb_extract"
  )

selector_list <- reactive({
  list(
    adsl_extract = reactive(adsl_data_extract_srv_output)
  )
})

testthat::test_that("data_merge_srv returns a reactive containing a list", {
  # filtered data ("datasets_used") case produces deprecation warning as chunks are deprecated
  rlang::local_options(lifecycle_verbosity = "quiet")

  shiny::testServer(
    data_merge_srv,
    args = list(selector_list = selector_list, datasets = datasets_used),
    expr = {
      testthat::expect_is(session$returned, "reactive")
      testthat::expect_is(session$returned(), "list")
      testthat::expect_identical(
        c("expr", "columns_source", "keys", "filter_info", "data", "chunks"),
        names(session$returned())
      )
    }
  )
})

testthat::test_that("data_merge_srv$data returns data.frame when passing 1 extract in selector_list", {
  # filtered data ("datasets_used") case produces deprecation warning as chunks are deprecated
  rlang::local_options(lifecycle_verbosity = "quiet")

  shiny::testServer(
    data_merge_srv,
    args = list(selector_list = selector_list, datasets = datasets_used),
    expr = {
      testthat::expect_is(session$returned()$data, "function")
      testthat::expect_is(session$returned()$data(), "data.frame")
      testthat::expect_identical(
        data.frame("STUDYID" = "STUDYID", "USUBJID" = "USUBJID", "AGE" = "AGE"),
        session$returned()$data()
      )
    }
  )
})

testthat::test_that("data_merge_srv throws error with missing selector_list and datasets arguments", {
  # filtered data ("datasets_used") case produces deprecation warning as chunks are deprecated
  rlang::local_options(lifecycle_verbosity = "quiet")

  testthat::expect_error(
    shiny::testServer(
      data_merge_srv,
      args = list(datasets = datasets_used),
      expr = NULL
    ),
    "argument \"selector_list\" is missing, with no default"
  )

  testthat::expect_error(
    shiny::testServer(
      data_merge_srv,
      args = list(selector_list = selector_list),
      expr = NULL
    ),
    "argument \"datasets\" is missing, with no default"
  )
})

testthat::test_that("data_merge_srv default merge_function is dplyr::full_join", {
  # filtered data ("datasets_used") case produces deprecation warning as chunks are deprecated
  rlang::local_options(lifecycle_verbosity = "quiet")

  shiny::testServer(
    data_merge_srv,
    args = list(selector_list = selector_list, datasets = datasets_used),
    expr = {
      testthat::expect_identical(merge_function, "dplyr::full_join")
    }
  )
})

testthat::test_that("data_merge_srv default anl_name is ANL", {
  # filtered data ("datasets_used") case produces deprecation warning as chunks are deprecated
  rlang::local_options(lifecycle_verbosity = "quiet")

  shiny::testServer(
    data_merge_srv,
    args = list(selector_list = selector_list, datasets = datasets_used),
    expr = {
      testthat::expect_identical(anl_name, "ANL")
    }
  )
})

testthat::test_that("data_merge_srv throws error when anl_name is not character or using non-allowed names", {
  # filtered data ("datasets_used") case produces deprecation warning as chunks are deprecated
  rlang::local_options(lifecycle_verbosity = "quiet")

  testthat::expect_error(
    shiny::testServer(
      data_merge_srv,
      args = list(selector_list = selector_list, datasets = datasets_used, anl_name = 1),
      expr = NULL
    ),
    regexp = "Must be of type 'string', not 'double'",
    fixed = TRUE
  )

  testthat::expect_error(
    shiny::testServer(
      data_merge_srv,
      args = list(selector_list = selector_list, datasets = datasets_used, anl_name = "565"),
      expr = NULL
    ),
    "make.names(anl_name) == anl_name is not TRUE",
    fixed = TRUE
  )

  testthat::expect_error(
    shiny::testServer(
      data_merge_srv,
      args = list(selector_list = selector_list, datasets = datasets_used, anl_name = "TRUE"),
      expr = NULL
    ),
    "make.names(anl_name) == anl_name is not TRUE",
    fixed = TRUE
  )

  testthat::expect_error(
    shiny::testServer(
      data_merge_srv,
      args = list(selector_list = selector_list, datasets = datasets_used, anl_name = "NA"),
      expr = NULL
    ),
    "make.names(anl_name) == anl_name is not TRUE",
    fixed = TRUE
  )
})

testthat::test_that("data_merge_srv throws error when selector_list is not a list or a reactive", {
  # filtered data ("datasets_used") case produces deprecation warning as chunks are deprecated
  rlang::local_options(lifecycle_verbosity = "quiet")

  testthat::expect_error(
    shiny::testServer(
      data_merge_srv,
      args = list(selector_list = "A", datasets = datasets_used),
      expr = NULL
    ),
    "Assertion on 'selector_list' failed: Must inherit from class 'reactive', but has class 'character'."
  )

  testthat::expect_error(
    shiny::testServer(
      data_merge_srv,
      args = list(selector_list = reactive(c("A")), datasets = datasets_used),
      expr = session$returned()
    ),
    "Must be of type 'list', not 'character'."
  )
})

testthat::test_that("data_merge_srv throws error selector_list is not named list", {
  # filtered data ("datasets_used") case produces deprecation warning as chunks are deprecated
  rlang::local_options(lifecycle_verbosity = "quiet")

  testthat::expect_error(
    shiny::testServer(
      data_merge_srv,
      args = list(selector_list = reactive(list("A")), datasets = datasets_used),
      expr = session$returned()
    ),
    "Assertion on 'selector_list()' failed: Must have names",
    fixed = TRUE
  )
})

testthat::test_that("data_merge_srv accepts reactive and character merge_function", {
  # filtered data ("datasets_used") case produces deprecation warning as chunks are deprecated
  rlang::local_options(lifecycle_verbosity = "quiet")

  m_fun <- reactive("dplyr::left_join")
  testthat::expect_error(
    shiny::testServer(
      data_merge_srv,
      args = list(selector_list = selector_list, datasets = datasets_used, merge_function = m_fun),
      expr = session$returned()
    ),
    NA
  )

  testthat::expect_error(
    shiny::testServer(
      data_merge_srv,
      args = list(selector_list = selector_list, datasets = datasets_used, merge_function = "dplyr::left_join"),
      expr = session$returned()
    ),
    NA
  )
})

selector_list2 <- reactive({
  list(
    adsl_extract = reactive(adsl_data_extract_srv_output),
    adlb_extract = reactive(adlb_data_extract_srv_output)
  )
})

testthat::test_that("data_merge_srv returns merged data.frame when passing 2 extracts in selector_list", {
  # filtered data ("datasets_used") case produces deprecation warning as chunks are deprecated
  rlang::local_options(lifecycle_verbosity = "quiet")

  shiny::testServer(
    data_merge_srv,
    args = list(selector_list = selector_list2, datasets = datasets_used),
    expr = {
      testthat::expect_true(inherits(session$returned()$data, "function"))
      testthat::expect_true(inherits(session$returned()$data(), "data.frame"))
      testthat::expect_identical(
        data.frame("STUDYID" = "STUDYID", "USUBJID" = "USUBJID", "AGE" = "AGE", "AVAL" = "AVAL", "CHG" = "CHG"),
        session$returned()$data()
      )
    }
  )
})
