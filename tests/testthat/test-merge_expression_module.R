adsl <- as.data.frame(as.list(stats::setNames(nm = c(c("STUDYID", "USUBJID"), "AGE"))))
adlb <- as.data.frame(
  as.list(stats::setNames(nm = c(c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"), "AVAL", "CHG", "CHG2", "ABLFL")))
)

data_list <- list(ADSL = reactive(adsl), ADLB = reactive(adlb))
join_keys <- teal.data::join_keys(
  teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")),
  teal.data::join_key("ADSL", "ADLB", c("STUDYID", "USUBJID")),
  teal.data::join_key("ADLB", "ADLB", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
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
  "merge_expression_module accepts a list of data_extract_spec, a list of reactive data frames and a join keys",
  code = {
    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_no_error(
        merge_expression_module(
          data_extract = list(adsl_var = adsl_extract, adlb_var = adlb_extract),
          datasets = data_list,
          join_keys = join_keys
        )
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

testthat::test_that("merge_expression_module works if list some elements of the list are  NULL", {
  testthat::expect_silent(
    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = {
        merge_expression_module(
          data_extract = list(adsl_var = adsl_extract, adlb_var = NULL),
          datasets = data_list,
          join_keys = join_keys
        )
      }
    )
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
    paste0(
      "Assertion on 'data_extract' failed: ",
      "May only contain the following types: {list,data_extract_spec,NULL}, but element 2 has type 'character'."
    ),
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
