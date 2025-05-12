test_that("datasets", {
  expect_no_error(dataset0 <- datasets("df", "df"))
  expect_no_error(dataset1 <- datasets("df"))
  expect_no_error(dataset2 <- datasets(where(is.matrix)))
  expect_no_error(dataset3 <- datasets(where(is.data.frame)))
})

test_that("variables", {
  expect_no_error(var0 <- variables("a", "a"))
  expect_no_error(var1 <- variables("a"))
  expect_no_error(var2 <- variables(where(is.factor)))
  # Allowed to specify whatever we like, it is not until resolution that this raises errors
  expect_no_error(var3 <- variables(where(is.factor), where(function(x) {
    head(x, 1)
  })))
  expect_no_error(var4 <- variables(where(is.matrix), where(function(x) {
    head(x, 1)
  })))
})

test_that("raw combine of types", {
  expect_equal(c(datasets("df")), datasets("df"))
  expect_length(c(datasets("df"), variables("df")), 2L)
  expect_length(c(datasets("df"), variables("df"), values("df")), 3L)
})

test_that("combine types", {
  expect_no_error(c(
    datasets(where(is.data.frame), selected = "df1"),
    variables(where(is.numeric))
  ))
})

test_that("values", {
  expect_no_error(val0 <- values("a", "a"))
  expect_no_error(val1 <- values("a"))
  expect_no_error(val2 <- values(where(is.factor)))
  # Allowed to specify whatever we like, it is not until resolution that this raises errors
  expect_no_error(val3 <- values(where(is.factor), function(x) {
    head(x, 1)
  }))
  expect_no_error(val4 <- values(where(is.matrix), function(x) {
    head(x, 1)
  }))
})
