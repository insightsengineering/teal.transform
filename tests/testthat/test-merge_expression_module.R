adsl <- as.data.frame(as.list(stats::setNames(nm = c(teal.data::get_cdisc_keys("ADSL"), "AGE"))))
adlb <- as.data.frame(
  as.list(stats::setNames(nm = c(teal.data::get_cdisc_keys("ADLB"), "AVAL", "CHG", "CHG2", "ABLFL")))
)

data_list <- list(ADSL = reactive(adsl), ADLB = reactive(adlb))
join_keys <- list(
  ADSL = list(
    ADSL = c(STUDYID = "STUDYID", USUBJID = "USUBJID"),
    ADLB = c(STUDYID = "STUDYID", USUBJID = "USUBJID")
  ),
  ADLB = list(
    ADSL = c(STUDYID = "STUDYID", USUBJID = "USUBJID"),
    ADLB = c(STUDYID = "STUDYID", USUBJID = "USUBJID", PARAMCD = "PARAMCD", AVISIT = "AVISIT")
  )
)

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
adlb_extract <- data_extract_spec(
  dataname = "ADLB",
  filter = filter_spec(vars = "PARAMCD", choices = "PARAMCD", selected = "PARAMCD"),
  select = select_spec(
    label = "Select variable:",
    choices = c("AVAL", "CHG"),
    selected = "AVAL",
    multiple = TRUE,
    fixed = FALSE
  )
)

testthat::test_that(
  "merge_expression_module accepts a list of data_extract_spec, a list of reactive data frames and a list of join keys",
  code = {
    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        merge_expression_module(
          data_extract = list(adsl_var = adsl_extract, adlb_var = adlb_extract),
          datasets = data_list,
          join_keys = join_keys
        ),
        NA
      )
    )
  }
)

testthat::test_that("merge_expression_module returns a reactive containing a list", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = {
      output <- merge_expression_module(
        data_extract = list(adsl_var = adsl_extract, adlb_var = adlb_extract),
        datasets = data_list,
        join_keys = join_keys
      )
      testthat::expect_is(output, "reactive")
      testthat::expect_is(isolate(output()), "list")
      testthat::expect_identical(
        c("expr", "columns_source", "keys", "filter_info"),
        names(isolate(output()))
      )
    }
  )
})

testthat::test_that("merge_expression_module throws error if data_extract is not a list of data_extract_spec", {
  testthat::expect_error(
    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = {
        merge_expression_module(
          data_extract = list(adsl_var = adsl_extract, adlb_var = "aa"),
          datasets = data_list,
          join_keys = join_keys
        )
      }
    ),
    "May only contain the following types: {list,data_extract_spec}, but element 2 has type 'character'",
    fixed = TRUE
  )
})

testthat::test_that("merge_expression_module throws error if data_extract is not a named list", {
  shiny::withReactiveDomain(
    domain = shiny::MockShinySession$new(),
    expr = testthat::expect_error(
      merge_expression_module(
        data_extract = list(adsl_extract, adlb_extract),
        datasets = data_list,
        join_keys = join_keys
      ),
      "Assertion on 'data_extract' failed: Must have names."
    )
  )
})
