test_that("datasets Ops work", {
  dataset1 <- dataset("ABC")
  dataset2 <- dataset("ABC2")
  datasets <- dataset1 & dataset1
  expect_equal(datasets$dataset, "ABC")
  datasets <- dataset1 & dataset2
  expect_equal(datasets$dataset, c("ABC", "ABC2"))
  datasets2 <- datasets & dataset2
  expect_equal(datasets$dataset, c("ABC", "ABC2"))
})

test_that("variables Ops work", {
  var1 <- variable("abc")
  var2 <- variable("abc2")
  vars <- var1 & var1
  expect_equal(vars$variable, "abc")
  vars <- var1 & var2
  expect_equal(vars$variable, c("abc", "abc2"))
})

test_that("variables, datsets Ops work", {
  dataset1 <- dataset("ABC2")
  var1 <- variable("abc")
  expect_equal(dataset1 & var1, var1 & dataset1)
  expect_equal(vars$variable, c("ABC", "ABC2"))
})
