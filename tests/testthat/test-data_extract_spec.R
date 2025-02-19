testthat::test_that("data_extract_spec accepts string or NULL in dataname argument", {
  testthat::expect_no_error(data_extract_spec(dataname = "a"))
  testthat::expect_no_error(data_extract_spec(dataname = NULL))
  testthat::expect_error(data_extract_spec(dataname = 1))
})

testthat::test_that("data_extract_spec accepts select_spec or NULL in select argument", {
  testthat::expect_no_error(data_extract_spec(select = select_spec()))
  testthat::expect_no_error(data_extract_spec(select = NULL))
  testthat::expect_error(data_extract_spec(select = list()))
})

testthat::test_that("data_extract_spec accepts list of filter_spec, filter_spec or NULL in filter argument", {
  testthat::expect_no_error(data_extract_spec(filter = list(filter_spec(), filter_spec())))
  testthat::expect_no_error(data_extract_spec(filter = filter_spec()))
  testthat::expect_no_error(data_extract_spec(filter = NULL))
  testthat::expect_error(data_extract_spec(filter = list(a = 1)))
})

testthat::test_that("data_extract_spec accepts a flag in reshape argument", {
  testthat::expect_no_error(data_extract_spec(reshape = TRUE))
  testthat::expect_error(data_extract_spec(reshape = c("A", "B")))
})

testthat::test_that("data_extract_spec has defaults", {
  testthat::expect_equal(
    data_extract_spec(),
    data_extract_spec(dataname = NULL, select = select_spec(), filter = NULL, reshape = FALSE)
  )
})

testthat::test_that("data_extract_spec returns data_extract_spec when eager in the arguments", {
  ss <- select_spec(choices = letters)
  fs <- filter_spec(vars = "col1", choices = letters)
  fs$dataname <- "dataset"
  testthat::expect_identical(
    data_extract_spec(dataname = "dataset", select = ss, filter = fs),
    structure(
      list(
        dataname = "dataset",
        select = ss,
        filter = list(fs),
        reshape = FALSE
      ),
      class = "data_extract_spec"
    )
  )
})

testthat::test_that("data_extract_spec returns delayed_data_extract_spec when delayed_data in the arguments", {
  testthat::expect_s3_class(data_extract_spec(select = select_spec()), "delayed_data_extract_spec")
})
