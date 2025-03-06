f <- function(x){head(x, 1)}

test_that("resolver datasets works", {
  df_head <- datasets("df", f)
  df_first <- datasets("df")
  matrices <- datasets(is.matrix)
  df_mean <- datasets("df", mean)
  median_mean <- datasets(median, mean)
  td <- within(teal.data::teal_data(), {
    df <- data.frame(a = LETTERS[1:5], b = factor(letters[1:5]), c =  factor(letters[1:5]))
    m <- cbind(b = 1:5, c = 10:14)
    m2 <- cbind(a = LETTERS[1:2], b = LETTERS[4:5])
  })
  expect_no_error(resolver(df_head, td))
  expect_no_error(resolver(df_first, td))
  out <- resolver(matrices, td)
  expect_length(out$datasets$select, 1L) # Because we use first
  expect_no_error(resolver(df_mean, td))
  expect_error(resolver(median_mean, td))
})

test_that("resolver variables works", {
  df <- datasets("df")
  matrices <- datasets(is.matrix)
  data_frames <- datasets(is.data.frame)
  var_a <- variables("a")
  factors <- variables(is.factor)
  factors_head <- variables(is.factor, function(x){head(x, 1)})
  var_matrices_head <- variables(is.matrix, function(x){head(x, 1)})
  td <- within(teal.data::teal_data(), {
    df <- data.frame(a = LETTERS[1:5], b = factor(letters[1:5]), c =  factor(letters[1:5]))
    m <- cbind(b = 1:5, c = 10:14)
    m2 <- cbind(a = LETTERS[1:2], b = LETTERS[4:5])
  })

  expect_no_error(resolver(df & var_a, td))
  expect_no_error(resolver(df & factors, td))
  expect_error(resolver(df & factors_head, td))
  expect_error(resolver(df & var_matrices_head, td))

  expect_error(resolver(matrices & var_a, td))
  expect_error(resolver(matrices & factors, td))
  expect_error(resolver(matrices & factors_head, td))
  expect_error(resolver(matrices & var_matrices_head, td))

  expect_no_error(resolver(data_frames & var_a, td))
  expect_no_error(resolver(data_frames & factors, td))
  expect_error(resolver(data_frames & factors_head, td))
  expect_error(resolver(data_frames & var_matrices_head, td))
})

test_that("names and variables are reported", {
  td <- within(teal.data::teal_data(), {
    df <- data.frame(A = as.factor(letters[1:5]),
                     Ab = LETTERS[1:5],
                     Abc = c(LETTERS[1:4], letters[1]))
    m <- matrix()
  })
  df_upper_variables <- datasets("df") & variables(function(x){x==toupper(x)})
  out <- resolver(df_upper_variables, td)
  # This should select both A because the name is all capital letters and Ab values is all upper case.
  expect_length(out$variables$names, 2)
  df_all_upper_variables <- datasets("df") & variables(function(x){all(x==toupper(x))})
  out <- resolver(df_all_upper_variables, td)
  expect_length(out$variables$names, 2)
})


test_that("update_spec resolves correctly", {
  td <- within(teal.data::teal_data(), {
    df <- data.frame(A = as.factor(letters[1:5]),
                     Ab = LETTERS[1:5])
    df_n <- data.frame(C = 1:5,
                       Ab = as.factor(letters[1:5]))
  })
  data_frames_factors <- datasets(is.data.frame) & variables(is.factor)
  expect_false(is.null(attr(data_frames_factors$datasets$names, "original")))
  expect_false(is.null(attr(data_frames_factors$datasets$select, "original")))
  expect_false(is.null(attr(data_frames_factors$variables$names, "original")))
  expect_false(is.null(attr(data_frames_factors$variables$select, "original")))

  res <- resolver(data_frames_factors, td)
  expect_false(is.null(attr(res$datasets$names, "original")))
  expect_false(is.null(attr(res$datasets$select, "original")))
  expect_false(is.null(attr(res$variables$names, "original")))
  expect_false(is.null(attr(res$variables$select, "original")))

  res2 <- update_spec(res, "datasets", "df_n")
  expect_false(is.null(attr(res2$datasets$names, "original")))
  expect_false(is.null(attr(res2$datasets$select, "original")))
  expect_false(is.null(attr(res2$variables$names, "original")))
  expect_false(is.null(attr(res2$variables$select, "original")))

  expect_no_error(res3 <- resolver(res2, td))
  expect_false(is.null(attr(res3$datasets$names, "original")))
  expect_false(is.null(attr(res3$datasets$select, "original")))
  expect_equal(attr(res3$datasets$names, "original"), attr(data_frames_factors$datasets$names, "original"))
  expect_equal(attr(res3$datasets$select, "original"), attr(data_frames_factors$datasets$select, "original"))
  expect_equal(res3$datasets$select, "df_n", check.attributes = FALSE)
  expect_equal(res3$variables$select, "Ab", check.attributes = FALSE)
  expect_false(is.null(attr(res3$variables$names, "original")))
  expect_false(is.null(attr(res3$variables$select, "original")))
  expect_equal(attr(res3$variables$names, "original"), attr(data_frames_factors$variables$names, "original"))
  expect_equal(attr(res3$variables$select, "original"), attr(data_frames_factors$variables$select, "original"))
})

