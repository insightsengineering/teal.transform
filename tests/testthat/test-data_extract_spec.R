testthat::test_that("data_extract_spec accepts string in dataname argument", {
  testthat::expect_no_error(data_extract_spec(dataname = "a"))
  testthat::expect_no_error(data_extract_spec(dataname = "all"))
  testthat::expect_error(data_extract_spec(dataname = NULL))
  testthat::expect_error(data_extract_spec(dataname = character(0)))
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
    data_extract_spec(
      dataname = "all",
      select = select_spec(selected = all_choices()),
      filter = filter_spec(),
      reshape = FALSE
    )
  )
})

testthat::test_that("data_extract_spec returns data_extract_spec when filter and select are eager", {
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

testthat::test_that("data_extract_spec fails when dataname is 'all' and select/filter are eager", {
  testthat::skip("todo")
  testthat::expect_error(data_extract_spec(select = select_spec(choices = letters)))
  testthat::expect_error(data_extract_spec(filter = select_spec(vars = "col1", choices = letters)))
})

testthat::test_that("data_extract_spec warns when dataname is 'all' and `data != all` in any variable/value_choices", {
  testthat::skip("todo")
  testthat::expect_warning(data_extract_spec(select = select_spec(choices = variable_choices(data = "iris"))))
  testthat::expect_warning(data_extract_spec(select = filter_spec(choices = value_choices(data = "iris"))))
})

testthat::test_that("resolve.data_extract_spec multiplies when dataname is 'all'", {
  # todo: make sure that data in variable/value_choices is different or force rewrite (as it is now)
})

testthat::test_that("data_extract_spec returns delayed_data_extract_spec when delayed_data in the arguments", {
  testthat::expect_s3_class(data_extract_spec(select = select_spec()), "delayed_data_extract_spec")
})
