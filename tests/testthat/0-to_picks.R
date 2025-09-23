testthat::test_that("to_picks converts eager select_spec to variables without ordered, always_selected nor label", {
  test <- select_spec(
    choices = c("AVAL", "BMRKR1", "AGE"),
    selected = "AVAL",
    multiple = FALSE,
    fixed = FALSE,
    label = "Column",
    ordered = TRUE,
    always_selected = "AGE"
  )

  out <- select_spec_to_variables(test)
  testthat::expect_s3_class(out, "variables")
  testthat::expect_identical(out$choices, unclass(test$choices))
  testthat::expect_identical(out$selected, unclass(test$selected))
  testthat::expect_identical(attr(out, "multiple"), test$multiple)
})

testthat::test_that("to_picks converts delayed select_spec to variables preserving delayed_data and is resolvable", {
  subset_fun <- function(data) names(Filter(is.factor, data))
  test <- select_spec(
    choices = variable_choices("ADRS", subset = subset_fun),
    selected = "AVISIT",
    multiple = FALSE,
    fixed = FALSE,
    label = "Column",
    ordered = TRUE,
    always_selected = "AGE"
  )

  out <- suppressWarnings(select_spec_to_variables(test))
  testthat::expect_s3_class(out, "variables")
  testthat::expect_s3_class(out$choices, "delayed_data")
  testthat::expect_identical(out$selected, "AVISIT")

  testthat::expect_identical(
    determine(out, data = rADRS)$x,
    determine(variables(choices = subset_fun(rADRS), selected = "AVISIT"), data = rADRS)$x
  )
})

testthat::test_that("extract_filters pulls converts filter_spec to picks with value", {
  des <- data_extract_spec(
    dataname = "ADLB",
    filter = list(
      filter_spec(
        vars = "PARAMCD",
        choices = value_choices(data$ADLB, "PARAMCD", "PARAM"),
        selected = levels(data$ADLB$PARAMCD)[1],
        multiple = FALSE,
        label = "Select lab:"
      ),
      filter_spec(
        vars = "AVISIT",
        choices = levels(data$ADLB$AVISIT),
        selected = levels(data$ADLB$AVISIT)[1],
        multiple = FALSE,
        label = "Select visit:"
      )
    )
  )


  extract_filters(des)
})


testthat::test_that("to_picks", {
  des <- list(
    data_extract_spec(
      dataname = "ADSL",
      select = select_spec(
        choices = variable_choices(data$ADSL),
        selected = "AGE",
        multiple = FALSE,
        fixed = FALSE
      )
    ),
    data_extract_spec(
      dataname = "ADLB",
      select = select_spec(
        choices = variable_choices(data$ADLB, c("AVAL", "CHG", "PCHG", "ANRIND", "BASE")),
        selected = "AVAL",
        multiple = FALSE,
        fixed = FALSE
      ),
      filter = list(
        filter_spec(
          vars = "PARAMCD",
          choices = value_choices(data$ADLB, "PARAMCD", "PARAM"),
          selected = levels(data$ADLB$PARAMCD)[1],
          multiple = FALSE,
          label = "Select lab:"
        ),
        filter_spec(
          vars = "AVISIT",
          choices = levels(data$ADLB$AVISIT),
          selected = levels(data$ADLB$AVISIT)[1],
          multiple = FALSE,
          label = "Select visit:"
        )
      )
    )
  )

  to_picks()
})

# can't convert list(data_extract_spec("ADSL", ...), data_extract_spec("ADTTE", ...)) reliably
#  to picks as picks can't conditionally determine next step based on the dataset selection
#
