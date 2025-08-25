testthat::test_that("merge_expr", {
  jk <- teal.data::join_keys()
  merge_expr(
    selectors = list(
      x = spec(
        datasets(choices = "test", selected = "test"),
        variables(choices = letters, selected = letters)
      )
    ),
    output_name = "elo",
    join_fun = "foo",
    join_keys = jk,
    TRUE
  )
})
