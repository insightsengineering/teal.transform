test_that("datasets", {
  expect_no_error(dataset0 <- datasets("df", "df"))
  out <- list(names = "df", select = "df")
  class(out) <- c("delayed", "datasets", "type", "list")
  expect_equal(dataset0, out, check.attributes = FALSE)
  expect_no_error(dataset1 <- datasets("df"))
  expect_true(is(dataset1$names, "vector"))
  expect_no_error(dataset2 <- datasets(is.matrix))
  expect_true(is(dataset2$names, "vector"))
  expect_no_error(dataset3 <- datasets(is.data.frame))
})

test_that("variables", {
  expect_no_error(var0 <- variables("a", "a"))
  expect_no_error(var1 <- variables("a"))
  expect_no_error(var2 <- variables(is.factor))
  # Allowed to specify whatever we like, it is not until resolution that this raises errors
  expect_no_error(var3 <- variables(is.factor, function(x) {
    head(x, 1)
  }))
  expect_no_error(var4 <- variables(is.matrix, function(x) {
    head(x, 1)
  }))
})

test_that("raw combine of types", {
  out <- c(datasets("df"), variables("df"))
  expect_length(out, 2L)
  expect_no_error(c(datasets("df"), variables("df"), values("df")))
})

test_that("values", {
  expect_no_error(val0 <- values("a", "a"))
  expect_no_error(val1 <- values("a"))
  expect_no_error(val2 <- values(is.factor))
  # Allowed to specify whatever we like, it is not until resolution that this raises errors
  expect_no_error(val3 <- values(is.factor, function(x) {
    head(x, 1)
  }))
  expect_no_error(val4 <- values(is.matrix, function(x) {
    head(x, 1)
  }))
})
