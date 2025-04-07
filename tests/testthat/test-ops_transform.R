basic_ops <- function(fun) {
  FUN <- match.fun(fun)
  type1 <- FUN("ABC")
  types <- type1 & type1
  out <- list(names = "ABC", select = list(first))
  class(out) <- c(fun, "type", "list")
  expect_equal(types, out, check.attributes = FALSE)
  type2 <- FUN("ABC2")
  types <- type1 & type2
  out <- list(names = c("ABC", "ABC2"), select = list(first))
  class(out) <- c("delayed", fun, "type", "list")
  expect_equal(types, out, check.attributes = FALSE)
  expect_equal(types$names, c("ABC", "ABC2"), check.attributes = FALSE)
  types2 <- types & type2
  expect_equal(types$names, c("ABC", "ABC2"), check.attributes = FALSE)
  expect_s3_class(types, class(out))
  type3 <- FUN("ABC2", select = all_choices)
  types <- type1 & type3
  expect_length(types$select, 2)
  type2b <- FUN(first_choice)
  type2c <- FUN(last_choice)
  out <- type2b & type2c
  expect_length(out$names, 2)
  expect_error(FUN("ABC") & 1)
  out <- type1 & type2b
  expect_true(is.list(out$names))
}

test_that("datasets & work", {
  basic_ops("datasets")
})


test_that("variables & work", {
  basic_ops("variables")
})

test_that("values & work", {
  basic_ops("values")
})

test_that("&(datsets, variables) create a single transform", {
  dataset1 <- datasets("ABC2")
  var1 <- variables("abc")
  vars <- dataset1 & var1
  vars2 <- var1 & dataset1
  expect_equal(vars$datasets$names, "ABC2", check.attributes = FALSE)
  expect_equal(vars$variables$names, "abc", check.attributes = FALSE)
})

test_that("&(datsets, number) errors", {
  expect_error(datasets("abc") & 1)
})

test_that("datsets & values work", {
  dataset1 <- datasets("ABC2")
  val1 <- values("abc")
  vars <- dataset1 & val1
  expect_equal(vars$datasets$names, "ABC2", check.attributes = FALSE)
  expect_equal(vars$values$names, "abc", check.attributes = FALSE)
})

test_that("&(datsets, number) errors", {
  expect_error(variables("abc") & 1)
})

test_that("variables & values work", {
  var1 <- variables("ABC2")
  val1 <- values("abc")
  vars <- var1 & val1
  expect_equal(vars$variables$names, "ABC2", check.attributes = FALSE)
  expect_equal(vars$values$names, "abc", check.attributes = FALSE)
})

test_that("&(values, number) errors", {
  expect_error(values("abc") & 1)
})

test_that("datasets & variables & values create a single specification", {
  dataset1 <- datasets("ABC2")
  var1 <- variables("ABC2")
  val1 <- values("abc")
  vars <- dataset1 & var1 & val1
  vars2 <- val1 & var1 & dataset1
  expect_equal(vars$datasets$names, "ABC2", check.attributes = FALSE)
  expect_equal(vars$variables$names, "ABC2", check.attributes = FALSE)
  expect_equal(vars$values$names, "abc", check.attributes = FALSE)
})

test_that("&(transform, number) errors", {
  expect_error(datasets("ABC2") & variables("ABC2") & values("abc") & 1)
  expect_error(datasets("ABC2") & values("abc") & 1)
})


test_that("| combines two transformers", {
  spec <- datasets("ABC") | datasets("abc")
  expect_length(spec, 2)
  expect_true(is.null(names(spec)))
})
