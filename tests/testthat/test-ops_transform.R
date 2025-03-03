basic_ops <- function(fun) {
  FUN <- match.fun(fun)
  type1 <- FUN("ABC")
  type2 <- FUN("ABC2")
  types <- type1 & type1
  out <- list(names = "ABC", select = list(first_choice))
  class(out) <- c("delayed", fun, "type")
  expect_equal(types[[fun]], out)
  types <- type1 & type2
  expect_equal(types[[fun]]$names, c("ABC", "ABC2"))
  types2 <- types & type2
  expect_equal(types[[fun]]$names, c("ABC", "ABC2"))
  expect_s3_class(types[[fun]], class(out))
  type3 <- FUN("ABC2", select = all_choices)
  types <- type1 & type3
  expect_length(types[[fun]]$select, 2)
  type2b <- FUN(first_choice)
  type2c <- FUN(last_choice)
  out <- type2b & type2c
  expect_length(out[[fun]]$names, 2)
  expect_error(FUN("ABC") & 1)
  out <- type1 & type2b
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

test_that("datsets & variables work", {
  dataset1 <- datasets("ABC2")
  var1 <- variables("abc")
  vars <- dataset1 & var1
  vars2 <- var1 & dataset1
  expect_equal(vars, vars2)
  expect_equal(vars$datasets$names, "ABC2")
  expect_equal(vars$variables$names, "abc")
  expect_error(vars & 1)
})

test_that("datsets & values work", {
  dataset1 <- datasets("ABC2")
  val1 <- values("abc")
  vars <- dataset1 & val1
  vars2 <- val1 & dataset1
  expect_equal(vars, vars2)
  expect_equal(vars$datasets$names, "ABC2")
  expect_equal(vars$values$names, "abc")
  expect_error(vars & 1)
})

test_that("variables & values work", {
  var1 <- variables("ABC2")
  val1 <- values("abc")
  vars <- var1 & val1
  vars2 <- val1 & var1
  expect_equal(vars, vars2)
  expect_equal(vars$variables$names, "ABC2")
  expect_equal(vars$values$names, "abc")
  expect_error(vars & 1)
})

test_that("datasets & variables & values work", {
  dataset1 <- datasets("ABC2")
  var1 <- variables("ABC2")
  val1 <- values("abc")
  vars <- dataset1 & var1 & val1
  vars2 <- val1 & var1 & dataset1
  expect_equal(vars, vars2)
  expect_equal(vars$datasets$names, "ABC2")
  expect_equal(vars$variables$names, "ABC2")
  expect_equal(vars$values$names, "abc")
  expect_error(vars & 1)
})



test_that("datasets", {
  first <- function(x){
    if (length(x) > 0) {
      false <- rep(FALSE, length.out = length(x))
      false[1] <- TRUE
      return(false)
    }
    return(FALSE)
  }

  dataset1 <- datasets("df", first)
  expect_true(is(dataset1$datasets$names, "vector"))
  dataset2 <- datasets(is.matrix, first)
  expect_true(is(dataset2$datasets$names, "vector"))
  dataset3 <- datasets(is.data.frame, first)
  mix <- dataset1 & dataset2
  expect_true(is(mix$datasets$names, "vector"))
})

test_that("variables", {
  var1 <- variables("a", first)
  var2 <- variables(is.factor, first)
  var3 <- variables(is.factor, function(x){head(x, 1)})
  var4 <- variables(is.matrix, function(x){head(x, 1)})
})
