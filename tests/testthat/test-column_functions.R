testthat::test_that("get_dataset_prefixed_col_names returns vector of columns", {
  data_test <- data.frame()
  attr(data_test, "filter_and_columns") <- list(columns = c("col_1"))

  testthat::expect_identical(".col_1", get_dataset_prefixed_col_names(data_test))
})

testthat::test_that("get_dataset_prefixed_col_names returns vector of multiple columns", {
  data_test <- data.frame()
  attr(data_test, "filter_and_columns") <- list(columns = c("col_1", "col_2"))

  testthat::expect_identical(c(".col_1", ".col_2"), get_dataset_prefixed_col_names(data_test))
})
