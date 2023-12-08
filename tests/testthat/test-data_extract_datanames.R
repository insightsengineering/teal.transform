des <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    choices = c("ARM", "ACTARM", "SEX"),
    selected = "ACTARM"
  )
)
des_list <- list(
  des,
  data_extract_spec(
    dataname = "ADLB",
    select = select_spec(
      choices = c("AVAL", "AVALU"),
      selected = "AVAL"
    )
  )
)

testthat::test_that("get_extract_datanames accepts a single data_extract_spec object", {
  testthat::expect_silent(get_extract_datanames(des))
})

testthat::test_that("get_extract_datanames accepts a list of data_extract_spec objects", {
  testthat::expect_silent(get_extract_datanames(des_list))
})

testthat::test_that("get_extract_datanames accepts a list of lists of data_extract_spec objects", {
  testthat::expect_silent(get_extract_datanames(list(des_list, list(des))))
})

testthat::test_that("get_extract_datanames accepts a list of data_extract_spec objects and NULL", {
  testthat::expect_silent(get_extract_datanames(list(des, des, NULL)))
})

testthat::test_that("get_extract_datanames accepts a list of data_extract_spec objects and logical", {
  testthat::expect_silent(get_extract_datanames(list(des, des, TRUE)))
})

testthat::test_that("get_extract_datanames accepts a list of lists of data_extract_spec objects and NULL", {
  testthat::expect_silent(get_extract_datanames(list(des_list, list(des), NULL)))
})

testthat::test_that("get_extract_datanames accepts a list of lists of data_extract_spec objects and logical", {
  testthat::expect_silent(get_extract_datanames(list(des_list, list(des), TRUE)))
})

testthat::test_that("get_extract_datanames returns the dataname of the single data_extract_spec object", {
  ged_output <- get_extract_datanames(des)
  testthat::expect_identical(ged_output, "ADSL")
  testthat::expect_equal(length(ged_output), 1)
})

testthat::test_that("get_extract_datanames returns the unique datanames from the list of data_extract_spec objects", {
  ged_output <- get_extract_datanames(des_list)
  testthat::expect_identical(ged_output, c("ADSL", "ADLB"))
})

testthat::test_that(
  "get_extract_datanames returns the unique datanames from the list of lists of data_extract_spec objects",
  code = {
    ged_output <- get_extract_datanames(list(des_list, list(des)))
    testthat::expect_identical(ged_output, c("ADSL", "ADLB"))
  }
)

testthat::test_that(
  "get_extract_datanames throws error when no data_extract_spec nor list  (of lists) of data_extract_spec is passed",
  {
    testthat::expect_error(get_extract_datanames(1))
    testthat::expect_error(get_extract_datanames("A"))
    testthat::expect_error(get_extract_datanames(TRUE))
    testthat::expect_error(get_extract_datanames(list(des, 1)))
    testthat::expect_error(get_extract_datanames(list(des, "A")))
  }
)

testthat::test_that("get_extract_datanames throws error with empty list", {
  testthat::expect_error(get_extract_datanames(list()), "length(data_extracts) > 0 is not TRUE", fixed = TRUE)
})

testthat::test_that("get_extract_datanames returns unique dataname when data_extract_specs have the same dataname", {
  ged_output <- get_extract_datanames(list(des, des))
  testthat::expect_identical(ged_output, "ADSL")
  testthat::expect_equal(length(ged_output), 1)
})
