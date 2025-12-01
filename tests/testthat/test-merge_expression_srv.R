adsl <- as.data.frame(as.list(stats::setNames(nm = c(c("STUDYID", "USUBJID"), "AGE"))))
adlb <- as.data.frame(
  as.list(stats::setNames(nm = c(c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"), "AVAL", "CHG", "CHG2", "ABLFL")))
)

data_list <- list(ADSL = reactive(adsl), ADLB = reactive(adlb))
data_list_nr <- list(ADSL = adsl, ADLB = adlb)

join_keys <- teal.data::default_cdisc_join_keys[c("ADSL", "ADLB")]

adsl_data_extract_srv_output <-
  list(
    dataname = "ADSL",
    filters = NULL,
    select = "AGE",
    keys = join_keys["ADSL", "ADSL"],
    reshape = FALSE,
    internal_id = "adsl_extract"
  )

adlb_data_extract_srv_output <-
  list(
    dataname = "ADLB",
    filters = NULL,
    select = c("AVAL", "CHG"),
    keys = join_keys["ADLB", "ADLB"],
    reshape = FALSE,
    internal_id = "adlb_extract"
  )

selector_list <- reactive({
  list(
    adsl_extract = reactive(adsl_data_extract_srv_output)
  )
})

testthat::test_that("merge_expression_srv returns a reactive containing a list", {
  shiny::testServer(
    merge_expression_srv,
    args = list(selector_list = selector_list, datasets = data_list, join_keys = join_keys),
    expr = {
      testthat::expect_is(session$returned, "reactive")
      testthat::expect_is(session$returned(), "list")
      testthat::expect_identical(
        c("expr", "columns_source", "keys", "filter_info"),
        names(session$returned())
      )
    }
  )
})

testthat::test_that("merge_expression_srv throws error with missing selector_list and datasets arguments", {
  testthat::expect_error(
    shiny::testServer(
      merge_expression_srv,
      args = list(datasets = data_list),
      expr = NULL
    ),
    "argument \"selector_list\" is missing, with no default"
  )

  testthat::expect_error(
    shiny::testServer(
      merge_expression_srv,
      args = list(selector_list = selector_list),
      expr = NULL
    ),
    "argument \"datasets\" is missing, with no default"
  )
})

testthat::test_that("merge_expression_srv default merge_function is dplyr::full_join", {
  shiny::testServer(
    merge_expression_srv,
    args = list(selector_list = selector_list, datasets = data_list, join_keys = join_keys),
    expr = {
      testthat::expect_identical(merge_function, "dplyr::full_join")
    }
  )
})

testthat::test_that("merge_expression_srv default anl_name is ANL", {
  shiny::testServer(
    merge_expression_srv,
    args = list(selector_list = selector_list, datasets = data_list, join_keys = join_keys),
    expr = {
      testthat::expect_identical(anl_name, "ANL")
    }
  )
})

testthat::test_that("merge_expression_srv default anl_name is ANL", {
  shiny::testServer(
    merge_expression_srv,
    args = list(selector_list = selector_list, datasets = data_list, join_keys = join_keys),
    expr = {
      testthat::expect_identical(anl_name, "ANL")
    }
  )
})

testthat::test_that("merge_expression_srv throws error when anl_name is not character or using non-allowed names", {
  testthat::expect_error(
    shiny::testServer(
      merge_expression_srv,
      args = list(selector_list = selector_list, datasets = data_list, join_keys = join_keys, anl_name = 1),
      expr = NULL
    ),
    regexp = "Must be of type 'string', not 'double'",
    fixed = TRUE
  )

  testthat::expect_error(
    shiny::testServer(
      merge_expression_srv,
      args = list(selector_list = selector_list, datasets = data_list, join_keys = join_keys, anl_name = "565"),
      expr = NULL
    ),
    "make.names(anl_name) == anl_name is not TRUE",
    fixed = TRUE
  )

  testthat::expect_error(
    shiny::testServer(
      merge_expression_srv,
      args = list(selector_list = selector_list, datasets = data_list, join_keys = join_keys, anl_name = "TRUE"),
      expr = NULL
    ),
    "make.names(anl_name) == anl_name is not TRUE",
    fixed = TRUE
  )

  testthat::expect_error(
    shiny::testServer(
      merge_expression_srv,
      args = list(selector_list = selector_list, datasets = data_list, join_keys = join_keys, anl_name = "NA"),
      expr = NULL
    ),
    "make.names(anl_name) == anl_name is not TRUE",
    fixed = TRUE
  )
})

testthat::test_that("merge_expression_srv throws error selector_list is not a list or a reactive", {
  testthat::expect_error(
    shiny::testServer(
      merge_expression_srv,
      args = list(selector_list = "A", datasets = data_list, join_keys = join_keys),
      expr = NULL
    )
  )

  testthat::expect_error(
    shiny::testServer(
      merge_expression_srv,
      args = list(selector_list = reactive(c("A")), datasets = data_list, join_keys = join_keys),
      expr = session$returned()
    )
  )
})

testthat::test_that("merge_expression_srv throws error if selector_list is not named list", {
  testthat::expect_error(
    shiny::testServer(
      merge_expression_srv,
      args = list(selector_list = reactive(list("A")), datasets = data_list, join_keys = join_keys),
      expr = session$returned()
    ),
    "Assertion on 'selector_list()' failed: Must have names",
    fixed = TRUE
  )
})

testthat::test_that("merge_expression_srv accepts reactive and character merge_function", {
  m_fun <- reactive("dplyr::left_join")
  testthat::expect_no_error(
    shiny::testServer(
      merge_expression_srv,
      args = list(selector_list = selector_list, datasets = data_list, join_keys = join_keys, merge_function = m_fun),
      expr = session$returned()
    )
  )

  testthat::expect_no_error(
    shiny::testServer(
      merge_expression_srv,
      args = list(
        selector_list = selector_list,
        datasets = data_list,
        join_keys = join_keys,
        merge_function = "dplyr::left_join"
      ),
      expr = session$returned()
    )
  )
})

selector_list <- reactive({
  list(
    adsl_extract = reactive(adsl_data_extract_srv_output),
    adlb_extract = reactive(adlb_data_extract_srv_output)
  )
})

testthat::test_that("merge_expression_srv returns merge expression when passing 2 extracts in selector_list", {
  shiny::testServer(
    merge_expression_srv,
    args = list(selector_list = selector_list, datasets = data_list, join_keys = join_keys),
    expr = {
      testthat::expect_true(inherits(session$returned()$expr, "list"))
      testthat::expect_true(inherits(session$returned()$expr[[2]], "<-"))
      testthat::expect_identical(
        c(
          "library(magrittr)",
          "ANL_1 <- ADSL %>% dplyr::select(STUDYID, USUBJID, AGE)",
          "ANL_2 <- ADLB %>% dplyr::select(STUDYID, USUBJID, AVAL, CHG)",
          "ANL <- ANL_1",
          "ANL <- dplyr::full_join(ANL, ANL_2, by = c(\"STUDYID\", \"USUBJID\"))"
        ),
        paste(session$returned()$expr)
      )
    }
  )
})

testthat::test_that("merge_expression_srv throws error if datasets is not a named list", {
  testthat::expect_error(
    merge_expression_srv(selector_list = selector_list, datasets = list(adsl, adlb), join_keys = join_keys),
    "Assertion on 'datasets' failed: Must have names."
  )
})

testthat::test_that("merge_expression_srv throws error if join_keys is not a join_keys object", {
  testthat::expect_error(
    shiny::testServer(
      merge_expression_srv,
      args = list(selector_list = selector_list, datasets = data_list, join_keys = list("USUBJID")),
      expr = NULL
    ),
    "class 'join_keys', but has class 'list'"
  )
})

testthat::test_that("merge_expression_srv accepts a list of (reactive) data.frames for datasets argument", {
  testthat::expect_no_error(
    shiny::testServer(
      merge_expression_srv,
      args = list(selector_list = selector_list, datasets = data_list, join_keys = join_keys),
      expr = NULL
    )
  )

  testthat::expect_no_error(
    shiny::testServer(
      merge_expression_srv,
      args = list(selector_list = selector_list, datasets = data_list_nr, join_keys = join_keys),
      expr = NULL
    )
  )
})
