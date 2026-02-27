testthat::test_that("interaction_vars is compatible with eval_select", {
  expect_equal(
    unname(
      tidyselect::eval_select(
        interaction_vars("AGE", "RACE"),
        data = teal.data::rADSL
      )
    ),
    which(colnames(teal.data::rADSL) %in% c("AGE", "RACE"))
  )
})

testthat::test_that("interaction_vars stores interactions in environment", {
  old <- select_env$interaction_vars
  withr::defer(select_env$interaction_vars <- old)
  select_env$interaction_vars <- NULL

  tidyselect::eval_select(
    c(interaction_vars(AGE, RACE), interaction_vars(AGE, COUNTRY)),
    data = teal.data::rADSL
  )
  expect_equal(
    select_env$interaction_vars,
    list(c("AGE", "RACE"), c("AGE", "COUNTRY"))
    )
})
