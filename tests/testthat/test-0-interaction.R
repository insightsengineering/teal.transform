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
  old <- select_env$custom_operators
  withr::defer(select_env$custom_operators <- old)
  select_env$custom_operators <- NULL

  tidyselect::eval_select(
    c(interaction_vars(AGE, RACE), interaction_vars(AGE, COUNTRY)),
    data = teal.data::rADSL
  )
  expect_equal(
    select_env$custom_operators,
    list(c("AGE", "RACE"), c("AGE", "COUNTRY"))
    )
})
