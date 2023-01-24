testthat::test_that("Queue can be initialized", {
  testthat::expect_true(is.environment(Queue$new()))
  testthat::expect_identical(class(Queue$new()), c("Queue", "R6"))
})

testthat::test_that("size method returns number of elements in queue", {
  queue <- Queue$new()
  testthat::expect_identical(queue$size(), 0L)
})

testthat::test_that("push method adds elements to queue", {
  queue <- Queue$new()
  testthat::expect_equal(queue$size(), 0L)
  testthat::expect_no_error(queue$push(7))
  testthat::expect_equal(queue$size(), 1L)
})

testthat::test_that("push method can add multiple elements", {
  queue <- Queue$new()
  testthat::expect_no_error(queue$push(c(1, "2")))
})

testthat::test_that("get method returns elements of queue", {
  queue <- Queue$new()
  queue$push(letters)
  testthat::expect_identical(queue$get(), letters)
})

testthat::test_that("pop method removes first element from queue", {
  queue <- Queue$new()
  queue$push(c(7, 8))
  testthat::expect_equal(queue$pop(), 7)
  testthat::expect_equal(queue$get(), 8)
})

testthat::test_that("remove method removes specified element from queue", {
  queue <- Queue$new()
  queue$push(c(7, 8, 7, 8))
  testthat::expect_no_error(queue$remove(7))
  testthat::expect_equal(queue$get(), c(8, 7, 8))
  testthat::expect_no_error(queue$remove(7))
  testthat::expect_equal(queue$get(), c(8, 8))
  testthat::expect_no_error(queue$remove(7))
  testthat::expect_equal(queue$get(), c(8, 8))
})

testthat::test_that("remove method can remove several elements", {
  queue <- Queue$new()
  queue$push(c(6, 7, 8, 6, 7, 8, 6, 7, 8, 6))
  testthat::expect_no_error(queue$remove(c(7, 7)))
  testthat::expect_equal(queue$get(), c(6, 8, 6, 8, 6, 7, 8, 6))
})

testthat::test_that("empty method removes all elements from queue", {
  queue <- Queue$new()
  queue$push(c(7, 8))
  testthat::expect_no_error(queue$empty())
  testthat::expect_equal(queue, Queue$new())
})

testthat::test_that("print method- displays proper format", {
  queue <- Queue$new()
  queue$push(c(7, 8))
  testthat::expect_identical(
    testthat::capture_output(
      queue$print()
    ),
    testthat::capture_output(
      cat(
        "<Queue>",
        "Size: 2",
        "Elements:",
        "7 8",
        sep = "\n"
      )
    )
  )
})
