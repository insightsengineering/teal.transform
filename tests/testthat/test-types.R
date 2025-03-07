first <- function(x){
  if (length(x) > 0) {
    false <- rep(FALSE, length.out = length(x))
    false[1] <- TRUE
    return(false)
  }
  return(FALSE)
}

test_that("datasets", {

  expect_no_error(dataset0 <- datasets("df", "df"))
  out <- list(names = "df", select = "df")
  class(out) <- c("delayed", "datasets", "type", "list")
  expect_equal(dataset0[["datasets"]], out, check.attributes = FALSE)
  expect_no_error(dataset1 <- datasets("df", first))
  # expect_true(is.vector(dataset1$datasets$names))
  expect_no_error(dataset2 <- datasets(is.matrix, first))
  # expect_true(is.vector(dataset2$datasets$names))
  expect_no_error(dataset3 <- datasets(is.data.frame, first))
})

test_that("variables", {
  expect_no_error(var0 <- variables("a", "a"))
  expect_no_error(var1 <- variables("a", first))
  expect_no_error(var2 <- variables(is.factor, first))
  expect_no_error(var3 <- variables(is.factor, function(x){head(x, 1)}))
  expect_no_error(var4 <- variables(is.matrix, function(x){head(x, 1)}))

})

test_that("values", {
  expect_no_error(val0 <- values("a", "a"))
  expect_no_error(val1 <- values("a", first))
  expect_no_error(val2 <- values(is.factor, first))
  expect_no_error(val3 <- values(is.factor, function(x){head(x, 1)}))
  expect_no_error(val4 <- values(is.matrix, function(x){head(x, 1)}))

})
