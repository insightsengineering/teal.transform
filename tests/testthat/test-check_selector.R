testthat::test_that("check_selector_dataname", {
  testthat::expect_silent(check_selector_dataname("test"))
  testthat::expect_error(check_selector_dataname(c()))
  testthat::expect_error(check_selector_dataname(c("test", "test2")))
})
