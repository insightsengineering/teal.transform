extract_nomultiple <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    choices = c("ARM", "ACTARM", "SEX"),
    selected = "ACTARM",
    multiple = FALSE
  )
)
extract_multiple <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    choices = c("ARM", "ACTARM", "SEX"),
    selected = "ACTARM",
    multiple = TRUE
  )
)

testthat::test_that("check_no_multiple_selection works", {
  testthat::expect_error(
    check_no_multiple_selection(extract_multiple),
    regexp = "May only contain the following types: \\{data_extract_spec\\}, but element 1 has type 'character'"
  )
  testthat::expect_error(
    check_no_multiple_selection(list_extract_spec(extract_multiple)),
    regexp = "multiple selection"
  )
  testthat::expect_null(check_no_multiple_selection(list_extract_spec(extract_nomultiple)))
})

testthat::test_that("list_extract_spec works", {
  testthat::expect_equal(list_extract_spec(extract_multiple), list_extract_spec(list_extract_spec(extract_multiple)))
  testthat::expect_false(identical(
    list_extract_spec(extract_multiple), extract_multiple
  ))
})
