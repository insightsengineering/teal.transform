testthat::test_that("delayed_choices constructor runs without error", {
  testthat::expect_no_error(delayed_choices())
  testthat::expect_no_error(delayed_choices("all"))
  testthat::expect_no_error(delayed_choices("first"))
  testthat::expect_no_error(delayed_choices("last"))
  testthat::expect_error(
    delayed_choices("wrong"),
    "should be one of "
  )
})

testthat::test_that("delayed_choices constructor returns funciton", {
  testthat::expect(is.function(delayed_choices()), "expecting function")
  testthat::expect(is.function(delayed_choices("all")), "expecting function")
  testthat::expect(is.function(delayed_choices("first")), "expecting function")
  testthat::expect(is.function(delayed_choices("last")), "expecting function")
})

testthat::test_that("delayed_choices functions return appropriate subsets of atomic input data", {
  all_choices <- delayed_choices()
  testthat::expect_identical(all_choices(letters), letters)
  all_choices <- delayed_choices("all")
  testthat::expect_identical(all_choices(letters), letters)
  first_choice <- delayed_choices("first")
  testthat::expect_identical(first_choice(letters), letters[1L])
  last_choice <- delayed_choices("last")
  testthat::expect_identical(last_choice(letters), letters[length(letters)])
})

testthat::test_that("delayed_choices funcitons return passed NULL", {
  all_choices <- delayed_choices()
  testthat::expect_null(all_choices(NULL))
  all_choices <- delayed_choices("all")
  testthat::expect_null(all_choices(NULL))
  first_choice <- delayed_choices("first")
  testthat::expect_null(first_choice(NULL))
  last_choice <- delayed_choices("last")
  testthat::expect_null(last_choice(NULL))
})

testthat::test_that("delayed_choices funcitons return passed empty vector", {
  all_choices <- delayed_choices()
  testthat::expect_identical(all_choices(character(0L)), character(0L))
  all_choices <- delayed_choices("all")
  testthat::expect_identical(all_choices(character(0L)), character(0L))
  first_choice <- delayed_choices("first")
  testthat::expect_identical(first_choice(character(0L)), character(0L))
  last_choice <- delayed_choices("last")
  testthat::expect_identical(last_choice(character(0L)), character(0L))
})
