f <- function(x) {
  head(x, 1)
}

test_that("resolver datasets works", {
  df_head <- datasets("df")
  df_first <- datasets("df")
  matrices <- datasets(where(is.matrix))
  df_mean <- datasets("df", where(mean))
  median_mean <- datasets(where(median), where(mean))
  td <- within(teal.data::teal_data(), {
    df <- data.frame(a = LETTERS[1:5], b = factor(letters[1:5]), c = factor(letters[1:5]))
    m <- cbind(b = 1:5, c = 10:14)
    m2 <- cbind(a = LETTERS[1:2], b = LETTERS[4:5])
  })
  expect_no_error(resolver(df_head, td))
  expect_no_error(resolver(df_first, td))
  out <- resolver(matrices, td)
  expect_length(out$select, 1L) # Because we use 1
  expect_error(expect_warning(resolver(df_mean, td)))
  expect_error(resolver(median_mean, td))
})

test_that("resolver variables works", {
  df <- datasets("df")
  matrices <- datasets(where(is.matrix))
  data_frames <- datasets(where(is.data.frame))
  var_a <- variables("a")
  factors <- variables(where(is.factor))
  factors_head <- variables(where(is.factor), where(function(x) {
    head(x, 1)
  }))
  var_matrices_head <- variables(where(is.matrix), where(function(x) {
    head(x, 1)
  }))
  td <- within(teal.data::teal_data(), {
    df <- data.frame(a = LETTERS[1:5], b = factor(letters[1:5]), c = factor(letters[1:5]))
    m <- cbind(b = 1:5, c = 10:14)
    m2 <- cbind(a = LETTERS[1:2], b = LETTERS[4:5])
  })

  expect_no_error(resolver(c(df, var_a), td))
  expect_no_error(resolver(c(df, factors), td))
  expect_error(resolver(c(df, factors_head), td))
  expect_error(resolver(c(df, var_matrices_head), td))


  expect_no_error(resolver(c(matrices, var_a), td))
  expect_error(resolver(c(matrices, factors), td))
  expect_error(resolver(c(matrices, factors_head), td))
  expect_error(resolver(c(matrices, var_matrices_head), td))

  expect_no_error(resolver(c(data_frames, var_a), td))
  expect_no_error(resolver(c(data_frames, factors), td))
  expect_error(resolver(c(data_frames, factors_head), td))
  expect_error(resolver(c(data_frames, var_matrices_head), td))
})

test_that("resolver values works", {
  df <- datasets("df")
  matrices <- datasets(where(is.matrix))
  data_frames <- datasets(where(is.data.frame))
  var_a <- variables("a")
  factors <- variables(is.factor)
  factors_head <- variables(where(is.factor), where(function(x) {
    head(x, 1)
  }))
  var_matrices_head <- variables(where(is.matrix), where(function(x) {
    head(x, 1)
  }))
  val_A <- values("A")
  td <- within(teal.data::teal_data(), {
    df <- data.frame(a = LETTERS[1:5], b = factor(letters[1:5]), c = factor(letters[1:5]))
    m <- cbind(b = 1:5, c = 10:14)
    m2 <- cbind(a = LETTERS[1:2], b = LETTERS[4:5])
  })
  expect_no_error(resolver(c(df, var_a, val_A), td))
})

test_that("names and variables are reported", {
  td <- within(teal.data::teal_data(), {
    df <- data.frame(
      A = as.factor(letters[1:5]),
      Ab = LETTERS[1:5],
      Abc = c(LETTERS[1:4], letters[1])
    )
    df2 <- data.frame(
      A = 1:5,
      B = 1:5
    )
    m <- matrix()
  })
  d_df <- datasets("df")
  upper_variables <- variables(where(function(x) {
    x == toupper(x)
  }))
  df_upper_variables <- c(d_df, upper_variables)
  expect_error(resolver(df_upper_variables, td))
  # This should select A and Ab:
  #      A because the name is all capital letters and
  #      Ab values is all upper case.
  # expect_length(out$variables$names, 2)
  v_all_upper <- variables(where(function(x) {
    all(x == toupper(x))
  }))
  df_all_upper_variables <- c(d_df, v_all_upper)
  expect_no_error(out <- resolver(df_all_upper_variables, td))
  expect_no_error(out <- resolver(c(datasets("df2"), v_all_upper), td))
  expect_length(out$variables$names, 2L)
  expect_no_error(out <- resolver(datasets(function(x) {
    is.data.frame(x) && all(colnames(x) == toupper(colnames(x)))
  }), td))
  expect_length(out$names, 1L)
  expect_no_error(out <- resolver(datasets(where(function(x) {
    is.data.frame(x) || any(colnames(x) == toupper(colnames(x)))
  })), td))
  expect_length(out$names, 2L)
})

test_that("update_spec resolves correctly", {
  td <- within(teal.data::teal_data(), {
    df <- data.frame(
      A = as.factor(letters[1:5]),
      Ab = LETTERS[1:5]
    )
    df_n <- data.frame(
      C = 1:5,
      Ab = as.factor(letters[1:5])
    )
  })
  data_frames_factors <- c(datasets(where(is.data.frame)), variables(where(is.factor)))
  expect_false(is.null(attr(data_frames_factors$datasets$names, "original")))
  expect_false(is.null(attr(data_frames_factors$datasets$select, "original")))
  expect_false(is.null(attr(data_frames_factors$variables$names, "original")))
  expect_false(is.null(attr(data_frames_factors$variables$select, "original")))

  expect_no_error(resolver(data_frames_factors, td))
})

test_that("OR specifications resolves correctly", {
  td <- within(teal.data::teal_data(), {
    df <- data.frame(A = 1:5, B = LETTERS[1:5])
    m <- cbind(A = 1:5, B = 5:10)
  })
  var_a <- variables("A")
  df_a <- c(datasets(where(is.data.frame)), var_a)
  matrix_a <- c(datasets(where(is.matrix)), var_a)
  df_or_m_var_a <- list(df_a, matrix_a)
  out <- resolver(df_or_m_var_a, td)
  expect_true(all(vapply(out, is.specification, logical(1L))))
})

test_that("OR specifications fail correctly", {
  td <- within(teal.data::teal_data(), {
    df <- data.frame(A = 1:5, B = LETTERS[1:5])
    m <- cbind(A = 1:5, B = 5:10)
  })
  var_a <- variables("A")
  df_a <- c(datasets(where(is.data.frame)), var_a)
  matrix_a <- c(datasets(where(is.matrix)), var_a)
  df_or_m_var_a <- list(df_a, matrix_a)
  out <- resolver(df_or_m_var_a, td)
  expect_error(update_spec(out, "variables", "B"))
})

test_that("OR update_spec filters specifications", {
  td <- within(teal.data::teal_data(), {
    df <- data.frame(A = 1:5, B = LETTERS[1:5])
    m <- cbind(A = 1:5, B = 5:10)
  })
  var_a <- variables("A")
  df_a <- c(datasets(where(is.data.frame)), var_a)
  matrix_a <- c(datasets(where(is.matrix)), var_a)
  df_or_m_var_a <- list(df_a, matrix_a)
  resolved <- resolver(df_or_m_var_a, td)
  # The second option is not possible to have it as df
  expect_error(update_spec(resolved, "datasets", "df"))
})
