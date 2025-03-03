test_that("resolver datasets works", {
  f <- function(x){head(x, 1)}
  first <- function(x){
    if (length(x) > 0) {
      false <- rep(FALSE, length.out = length(x))
      false[1] <- TRUE
      return(false)
    }
    return(FALSE)
  }

  dataset1 <- datasets("df", f)
  dataset2 <- datasets("df", first)
  dataset3 <- datasets(is.matrix, first)
  dataset4 <- datasets("df", mean)
  dataset5 <- datasets(median, mean)
  td <- within(teal.data::teal_data(), {
    df <- data.frame(a = LETTERS[1:5], b = factor(letters[1:5]), c =  factor(letters[1:5]))
    m <- cbind(b = 1:5, c = 10:14)
    m2 <- cbind(a = LETTERS[1:2], b = LETTERS[4:5])
  })
  expect_no_error(resolver(dataset1, td))
  resolver(dataset2, td)
  out <- resolver(dataset3, td)
  expect_length(out$datasets$select, 1L) # Because we use first
  expect_no_error(resolver(dataset4, td))
  expect_error(resolver(dataset5, td))
})

test_that("resolver variables works", {
  first <- function(x){
    if (length(x) > 0) {
      false <- rep(FALSE, length.out = length(x))
      false[1] <- TRUE
      return(false)
    }
    return(FALSE)
  }

  dataset1 <- datasets("df", first)
  dataset2 <- datasets(is.matrix, first)
  dataset3 <- datasets(is.data.frame, first)
  var1 <- variables("a", first)
  var2 <- variables(is.factor, first)
  var3 <- variables(is.factor, function(x){head(x, 1)})
  var4 <- variables(is.matrix, function(x){head(x, 1)})
  td <- within(teal.data::teal_data(), {
    df <- data.frame(a = LETTERS[1:5], b = factor(letters[1:5]), c =  factor(letters[1:5]))
    m <- cbind(b = 1:5, c = 10:14)
    m2 <- cbind(a = LETTERS[1:2], b = LETTERS[4:5])
  })

  resolver(dataset1 & var1, td)
  resolver(dataset1 & var2, td)
  expect_error(resolver(dataset1 & var3, td))

  resolver(dataset2 & var1, td)
  resolver(dataset2 & var2, td)
  resolver(dataset2 & var3, td)

  resolver(dataset3 & var1, td)
  resolver(dataset3 & var2, td)
  resolver(dataset3 & var3, td)
})
