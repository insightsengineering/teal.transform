testthat::test_that("delayed_data_extract_spec can be constructed with delayed_datanames", {
  testthat::expect_no_error(
    data_extract_spec(
      dataname = delayed_datanames()
    )
  )
  testthat::expect_no_error(
    data_extract_spec(
      dataname = delayed_datanames(),
      select = select_spec(
        choices = variable_choices(
          data = delayed_datanames(),
          subset = function(data) names(Filter(is.numeric, data))
        ),
        selected = first_choice()
      )
    )
  )
})

testthat::test_that("delayed_data_extract_spec cannot be constructed with mixed delayed_datanames", {
  testthat::expect_error(
    data_extract_spec(
      dataname = delayed_datanames("ADSL"),
      select = select_spec(
        choices = variable_choices(
          data = delayed_datanames("ADAE"),
          subset = function(data) names(Filter(is.numeric, data))
        ),
        selected = first_choice()
      )
    ),
    "delayed_datanames used must be identical"
  )
})

testthat::test_that("delayed_data_extract_spec cannot be constructed with delayed_datanames mixed with specific datasets", { # nolint: line_length.
  testthat::expect_error(
    data_extract_spec(
      dataname = "ADSL",
      select = select_spec(
        choices = variable_choices(
          data = delayed_datanames("ADSL"),
          subset = function(data) names(Filter(is.numeric, data))
        ),
        selected = first_choice()
      )
    ),
    "delayed_datanames must not be mixed with specific datanames"
  )
})

data <- teal.data::cdisc_data(
  ADSL = teal.data::rADSL,
  ADAE = teal.data::rADAE
)

des_current1 <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    choices = variable_choices(
      data = "ADSL",
      subset = function(data) names(Filter(is.numeric, data))
    ),
    selected = first_choice()
  )
)

des_current2 <- data_extract_spec(
  dataname = "ADAE",
  select = select_spec(
    choices = variable_choices(
      data = "ADAE",
      subset = function(data) names(Filter(is.numeric, data))
    ),
    selected = first_choice()
  )
)

des_delayed <- data_extract_spec(
  dataname = delayed_datanames(),
  select = select_spec(
    choices = variable_choices(
      data = delayed_datanames(),
      subset = function(data) names(Filter(is.numeric, data))
    ),
    selected = first_choice()
  )
)

des_resolved <- list(
  des_current1,
  des_current2
)



testthat::test_that("single current ddes is unchanged", {
  testthat::expect_identical(
    des_current1 |> resolve_delayed_datanames(names(data)),
    des_current1
  )
})

testthat::test_that("single delayed ddes is resolved into list of length(names(data))", {
  testthat::expect_equal(
    des_delayed |> resolve_delayed_datanames(names(data)),
    des_resolved,
    check.environment = FALSE
  )
})

testthat::test_that("resolved des replaces parent level in nested list of length 1", {
  # this reproduces what happens in data_extract_multiple_srv.list
  testthat::expect_equal(
    list(des_current1, des_delayed) |> resolve_delayed_datanames(names(data)),
    list(des_current1, des_resolved),
    check.environment = FALSE
  )
  testthat::expect_equal(
    list(list(des_current1), list(des_delayed)) |> resolve_delayed_datanames(names(data)),
    list(list(des_current1), des_resolved),
    check.environment = FALSE
  )
})

testthat::test_that("ddes with specified datasets resolves to intersection of those and the available ones", {
  des_delayed_subset <- data_extract_spec(
    dataname = delayed_datanames(c("ADSL", "ADEX")),
    select = select_spec(
      choices = variable_choices(
        data = delayed_datanames(c("ADSL", "ADEX")),
        subset = function(data) names(Filter(is.numeric, data))
      ),
      selected = first_choice()
    )
  )
  testthat::expect_equal(
    des_delayed_subset |> resolve_delayed_datanames(names(data)),
    list(des_current1),
    check.environment = FALSE
  )
})
