data_small <- structure(
  list(
    AGE = structure(c(29L, 29L, 48L, 37L, 36L)),
    SEX = structure(c(1L, 2L, 2L, 1L, 1L), .Label = c("F", "M"), class = "factor"),
    TRTSDTM = structure(
      c(
        1558930539.683, 1572360404.683, 1580052056.683, 1578713100.683, 1584009162.683
      ),
      tzone = "",
      class = c("POSIXct", "POSIXt")
    ),
    DCSREAS = structure(c(1L, NA, NA, 2L, 2L), .Label = c("ADVERSE EVENT", "DEATH"), class = "factor")
  ),
  class = "data.frame", row.names = c(NA, -5L)
)

data_list <- list(ADAMSET = reactive(data_small))

testthat::test_that("get_filter_call throws error if dataset is not a named list of reactives", {
  testthat::expect_error(
    get_filter_call(filter = list(
      list(columns = "SEX", selected = list(NA))
    ), dataname = "ADAMSET", data = list(ADAMSET = data_small)),
    "May only contain the following types: {reactive}, but element 1 has type 'data.frame'.",
    fixed = TRUE
  )
})

testthat::test_that("get_filter_call - data - NAs and one column - one selection", {
  testthat::expect_equal(
    isolate(get_filter_call(filter = list(
      list(columns = "SEX", selected = list(NA))
    ), dataname = "ADAMSET", data = data_list)),
    quote(
      dplyr::filter(is.na(SEX))
    )
  )
})

testthat::test_that("get_filter_call - data - NAs and one column - two selections", {
  testthat::expect_equal(
    isolate(get_filter_call(filter = list(
      list(columns = "SEX", selected = list(NA, "F"))
    ), dataname = "ADAMSET", data = data_list)),
    quote(
      dplyr::filter(SEX %in% c(NA_character_, "F"))
    )
  )
})

testthat::test_that("get_filter_call - data - NAs and two columns", {
  testthat::expect_equal(
    isolate(get_filter_call(filter = list(
      list(columns = c("SEX", "AGE"), selected = list(c("F", "44"), c(NA, "33")))
    ), dataname = "ADAMSET", data = data_list)),
    quote(
      dplyr::filter((SEX == "F" & AGE == "44") | (is.na(SEX) & AGE == "33"))
    )
  )
})

testthat::test_that("get_filter_call - data - some of factor levels and integer", {
  testthat::expect_equal(
    isolate(get_filter_call(filter = list(
      list(columns = "SEX", selected = list("F")),
      list(columns = "AGE", selected = list("42", "35"))
    ), dataname = "ADAMSET", data = data_list)),
    quote(
      dplyr::filter(SEX == "F" & AGE %in% c("42", "35"))
    )
  )
})

testthat::test_that("get_filter_call - data - trunc POSIX and single column", {
  testthat::expect_equal(
    isolate(get_filter_call(filter = list(
      list(columns = "TRTSDTM", selected = list("2020-03-08 06:28:11"))
    ), dataname = "ADAMSET", data = data_list)),
    quote(
      dplyr::filter(trunc(TRTSDTM) == "2020-03-08 06:28:11")
    )
  )
})

testthat::test_that("get_filter_call - data - trunc POSIX and two columns", {
  testthat::expect_equal(
    isolate(get_filter_call(filter = list(
      list(columns = c("TRTSDTM", "AGE"), selected = list(
        c("2020-03-08 06:28:11", "33"),
        c("2020-03-09 06:28:11", NA)
      ))
    ), dataname = "ADAMSET", data = data_list)),
    quote(
      dplyr::filter(
        (trunc(TRTSDTM) == "2020-03-08 06:28:11" & AGE == "33") |
          (trunc(TRTSDTM) == "2020-03-09 06:28:11" & is.na(AGE))
      )
    )
  )
})

testthat::test_that("get_filter_call - data - SEX and two columns, SEX variable is still there", {
  testthat::expect_equal(
    isolate(get_filter_call(filter = list(
      list(columns = c("SEX", "AGE"), selected = list(
        c("F", "33"),
        c("M", NA)
      ))
    ), dataname = "ADAMSET", data = data_list)),
    quote(
      dplyr::filter((SEX == "F" & AGE == "33") | (SEX == "M" & is.na(AGE)))
    )
  )
})

testthat::test_that("get_filter_call - data - three columns", {
  testthat::expect_equal(
    isolate(get_filter_call(filter = list(
      list(columns = c("SEX", "AGE", "DCSREAS"), selected = list(
        c("F", "33", NA),
        c("M", NA, NA)
      ))
    ), dataname = "ADAMSET", data = data_list)),
    quote(
      dplyr::filter((SEX == "F" & AGE == "33" & is.na(DCSREAS)) | (SEX == "M" & is.na(AGE) & is.na(DCSREAS)))
    )
  )
})

testthat::test_that("get_filter_call - data - non empty filter as NA is not selected and there are missings", {
  testthat::expect_equal(
    isolate(get_filter_call(filter = list(
      list(columns = "DCSREAS", selected = c(
        "ADVERSE EVENT",
        "DEATH"
      ))
    ), dataname = "ADAMSET", data = data_list)),
    quote(dplyr::filter(DCSREAS %in% c("ADVERSE EVENT", "DEATH")))
  )
})

testthat::test_that("get_filter_call - FALSE if empty selection for single", {
  testthat::expect_identical(
    isolate(get_filter_call(
      filter = list(
        list(columns = "DCSREAS", selected = c())
      ),
      dataname = "ADAMSET",
      data = data_list
    )),
    quote(dplyr::filter(FALSE))
  )
})

testthat::test_that("get_filter_call - data - all factor levels and integer", {
  testthat::expect_equal(
    isolate(get_filter_call(filter = list(
      list(columns = "SEX", selected = list("F", "M")),
      list(columns = "AGE", selected = list("42", "35"))
    ), dataname = "ADAMSET", data = data_list)),
    quote(
      dplyr::filter(AGE %in% c("42", "35"))
    )
  )
})

testthat::test_that("get_filter_call - data - empty filter as all levels and NA for DCSREAS are selected", {
  testthat::expect_null(
    isolate(get_filter_call(filter = list(
      list(columns = "DCSREAS", selected = c("ADVERSE EVENT", "DEATH", NA))
    ), dataname = "ADAMSET", data = data_list))
  )
})
testthat::test_that("get_filter_call - data - empty as all levels for SEX are selected - no missings", {
  testthat::expect_null(
    isolate(get_filter_call(filter = list(
      list(columns = "SEX", selected = list("F", "M"))
    ), dataname = "ADAMSET", data = data_list))
  )
})

testthat::test_that("get_filter_call - skip if all selected for single variable", {
  testthat::expect_null(
    isolate(get_filter_call(
      filter = list(
        list(
          columns = "DCSREAS",
          selected = unique(as.character(data_small$DCSREAS))
        )
      ),
      dataname = "ADAMSET",
      data = data_list
    ))
  )
})

testthat::test_that("get_filter_call - skip if all selected for multiple variables", {
  testthat::expect_null(
    isolate(get_filter_call(
      filter = list(
        list(
          columns = c("SEX", "AGE"),
          selected = strsplit(unique(paste(data_small$SEX, data_small$AGE, sep = "-")), "-")
        )
      ),
      dataname = "ADAMSET",
      data = data_list
    ))
  )
})

testthat::test_that("get_filter_call - skip if all selected for multiple variables - 3 vars", {
  testthat::expect_null(
    isolate(get_filter_call(
      filter = list(
        list(
          columns = c("SEX", "AGE", "DCSREAS"),
          selected = strsplit(unique(paste(data_small$SEX, data_small$AGE, data_small$DCSREAS, sep = "-")), "-")
        )
      ),
      dataname = "ADAMSET",
      data = data_list
    ))
  )
})
