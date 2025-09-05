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
  out <- select_spec_to_values(test)
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

  out <- suppressWarnings(select_spec_to_values(test))
  testthat::expect_s3_class(out, "variables")
  testthat::expect_s3_class(out$choices, "delayed_data")
  testthat::expect_identical(out$selected, "AVISIT")

  testthat::expect_identical(
    determine(out, data = rADRS, join_keys = join_keys())$x,
    variables(choices = subset_fun(rADRS), selected = subset_fun(rADRS)[1])
  )
})
