testthat::test_that("delayed_choices constructors run without error", {
  testthat::expect_no_error(all_choices())
  testthat::expect_no_error(first_choice())
  testthat::expect_no_error(last_choice())
})

testthat::test_that("delayed_choices constructors return functions", {
  testthat::expect(is.function(all_choices()), "expecting function")
  testthat::expect(is.function(first_choice()), "expecting function")
  testthat::expect(is.function(last_choice()), "expecting function")
})

testthat::test_that("delayed_choices functions return appropriate subsets of atomic input data", {
  testthat::expect_identical(all_choices()(letters), letters)
  testthat::expect_identical(first_choice()(letters), letters[1L])
  testthat::expect_identical(last_choice()(letters), letters[length(letters)])
})

testthat::test_that("delayed_choices functions return passed NULL", {
  testthat::expect_null(all_choices()(NULL))
  testthat::expect_null(first_choice()(NULL))
  testthat::expect_null(last_choice()(NULL))
})

testthat::test_that("delayed_choices functions return passed empty vector", {
  testthat::expect_identical(all_choices()(character(0L)), character(0L))
  testthat::expect_identical(first_choice()(character(0L)), character(0L))
  testthat::expect_identical(last_choice()(character(0L)), character(0L))
})
