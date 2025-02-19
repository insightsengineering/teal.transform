testthat::test_that("variable_choices returns variable_choices", {
  testthat::expect_s3_class(variable_choices(), "choices_labeled")
})

testthat::test_that("variable_choices accepts data to be NULL, string or data.frame with cols", {
  testthat::expect_no_error(variable_choices(data = NULL))
  testthat::expect_no_error(variable_choices(data = "a"))
  testthat::expect_no_error(variable_choices(data = mtcars))
  testthat::expect_error(variable_choices(data = data.frame()))
  testthat::expect_error(variable_choices(data = c("a", "b")))
})

testthat::test_that("variable_choices accepts subset to be character or a function. Both checked against colnames", {
  testthat::expect_no_error(variable_choices(iris, c("Species", "Sepal.Length")))
  testthat::expect_no_error(variable_choices(iris, subset = function(data) colnames(data)))
  testthat::expect_error(variable_choices(iris, c("Species", "idontexist")), "Must be a subset of")
})

testthat::test_that("variable_choices accepts subset to be character or a function when data is NULL", {
  testthat::expect_no_error(variable_choices(NULL, c("Species", "Sepal.Length")))
  testthat::expect_no_error(variable_choices(NULL, subset = function(data) colnames(data)))
  testthat::expect_no_error(variable_choices(NULL, c("Species", "idontexist")))
  testthat::expect_no_error(variable_choices(NULL, subset = function(data) c("Species", "idontexist")))
})

testthat::test_that("variable_choices has defaults for all arguments", {
  testthat::expect_no_error(variable_choices())
})

testthat::test_that("variable_choices subset function is evaluated when data is eager", {
  testthat::expect_identical(
    variable_choices(iris, subset = function(data) names(data)),
    variable_choices(iris, subset = colnames(iris))
  )
  testthat::expect_error(variable_choices(iris, subset = function(data) c("idontexist")), "Must be a subset of")
})

testthat::test_that("resolved delayed_variable_choices is identical to variable_choices specified with data", {
  testthat::expect_identical(
    resolve(variable_choices(), datasets = list(iris = iris)),
    variable_choices(iris, subset = function(data) names(data))
  )
  testthat::expect_identical(
    resolve(variable_choices(subset = c("Species", "Sepal.Length")), datasets = list(iris = iris)),
    variable_choices(iris, subset = c("Species", "Sepal.Length"))
  )
})

testthat::test_that("resolving delayed_variable_choices throws the error when subset isn't a colname", {
  testthat::expect_error(
    resolve(variable_choices(subset = function(data) "idontexist"), datasets = list(iris = iris)),
    "Must be a subset of"
  )
  testthat::expect_error(
    resolve(variable_choices(data = NULL, subset = "idontexist"), datasets = list(iris = iris)),
    "Must be a subset of"
  )
})
