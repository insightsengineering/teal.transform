testthat::test_that("merge_selectors makes no changes when single selector is provided", {
  x1 <- list(
    dataname = "ADSL",
    filters = NULL,
    select = utils::head(letters, 3),
    keys = c("STUDYID", "USUBJID"),
    reshape = FALSE,
    internal_id = "test1"
  )
  testthat::expect_identical(
    merge_selectors(list(x1)),
    list(list(x1), c("test1" = "test1"))
  )
})

testthat::test_that(
  desc = "merge selectors combines two selectors into one if dataname, filters, keys, reshape are identical",
  code = {
    x1 <- list(
      dataname = "ADSL",
      filters = NULL,
      select = utils::head(letters, 3),
      keys = c("STUDYID", "USUBJID"),
      reshape = FALSE,
      internal_id = "test1"
    )
    x2 <- list(
      dataname = "ADSL",
      filters = NULL,
      select = utils::tail(letters, 3),
      keys = c("STUDYID", "USUBJID"),
      reshape = FALSE,
      internal_id = "test2"
    )

    testthat::expect_equal(
      merge_selectors(list(x1, x2)),
      list(
        list(
          list(
            dataname = "ADSL",
            filters = NULL,
            select = c(utils::head(letters, 3), utils::tail(letters, 3)),
            keys = c("STUDYID", "USUBJID"),
            reshape = FALSE,
            internal_id = "test1"
          )
        ),
        c("test1" = "test1", "test2" = "test1")
      )
    )
  }
)

testthat::test_that("merge selectors combines two selectorsdespite duplicated internal_id", {
  x1 <- list(
    dataname = "ADSL",
    filters = NULL,
    select = utils::head(letters, 3),
    keys = c("STUDYID", "USUBJID"),
    reshape = FALSE,
    internal_id = "test1"
  )
  x2 <- list(
    dataname = "ADSL",
    filters = NULL,
    select = utils::tail(letters, 3),
    keys = c("STUDYID", "USUBJID"),
    reshape = FALSE,
    internal_id = "test1"
  )

  testthat::expect_equal(
    merge_selectors(list(x1, x2)),
    list(
      list(
        list(
          dataname = "ADSL",
          filters = NULL,
          select = c(utils::head(letters, 3), utils::tail(letters, 3)),
          keys = c("STUDYID", "USUBJID"),
          reshape = FALSE,
          internal_id = "test1"
        )
      ),
      c("test1" = "test1", "test1" = "test1")
    )
  )
})

testthat::test_that("Two pairs of selectors combined into two selectors", {
  x1 <- list(
    dataname = "ADSL",
    filters = NULL,
    select = utils::head(letters, 3),
    keys = c("STUDYID", "USUBJID"),
    reshape = FALSE,
    internal_id = "test1"
  )
  x2 <- list(
    dataname = "ADLB",
    filters = NULL,
    select = utils::head(letters, 3),
    keys = c("STUDYID", "USUBJID"),
    reshape = FALSE,
    internal_id = "test2"
  )
  x3 <- list(
    dataname = "ADSL",
    filters = NULL,
    select = tail(letters, 3),
    keys = c("STUDYID", "USUBJID"),
    reshape = FALSE,
    internal_id = "test3"
  )
  x4 <- list(
    dataname = "ADLB",
    filters = NULL,
    select = utils::tail(letters, 3),
    keys = c("STUDYID", "USUBJID"),
    reshape = FALSE,
    internal_id = "test4"
  )
  testthat::expect_equal(
    merge_selectors(list(x1, x2, x3, x4)),
    list(
      list(
        within(x1, {
          select <- c("a", "b", "c", "x", "y", "z")
        }),
        within(x2, {
          select <- c("a", "b", "c", "x", "y", "z")
        })
      ),
      c("test1" = "test1", "test2" = "test2", "test3" = "test1", "test4" = "test2")
    )
  )
})

testthat::test_that("merge does not merge when reshape differs", {
  x1 <- list(
    dataname = "ADSL",
    filters = NULL,
    select = utils::head(letters, 3),
    keys = c("STUDYID", "USUBJID"),
    reshape = FALSE,
    internal_id = "test1"
  )
  x2 <- list(
    dataname = "ADSL",
    filters = NULL,
    select = utils::tail(letters, 3),
    keys = c("STUDYID", "USUBJID"),
    reshape = TRUE,
    internal_id = "test2"
  )

  testthat::expect_equal(
    merge_selectors(list(x1, x2)),
    list(
      list(x1, x2),
      c("test1" = "test1", "test2" = "test2")
    )
  )
})

testthat::test_that("merge does not merge when filters differs", {
  x1 <- list(
    dataname = "ADSL",
    filters = NULL,
    select = utils::head(letters, 3),
    keys = c("STUDYID", "USUBJID"),
    reshape = TRUE,
    internal_id = "test1"
  )
  x2 <- list(
    dataname = "ADSL",
    filters = list(list(columns = "a", selected = list("1", "2"))),
    select = utils::tail(letters, 3),
    keys = c("STUDYID", "USUBJID"),
    reshape = FALSE,
    internal_id = "test2"
  )

  testthat::expect_equal(
    merge_selectors(list(x1, x2)),
    list(
      list(x1, x2),
      c("test1" = "test1", "test2" = "test2")
    )
  )
})

testthat::test_that("get_merge_key_pair works", {
  testthat::expect_identical(
    get_merge_key_pair(
      list(
        dataname = "ADSL",
        filters = NULL,
        select = utils::head(letters, 3),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "test1"
      ),
      list(
        dataname = "ADSL",
        filters = NULL,
        select = utils::tail(letters, 3),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "test2"
      ),
      c("STUDYID", "USUBJID")
    ),
    rlang::set_names(c("STUDYID", "USUBJID"))
  )

  # this is useful when merging two subsets of long datasets -> value columns next to each other
  # subtract from keys single filter on "from" selector
  testthat::expect_identical(
    get_merge_key_pair(
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = "OS", multiple = FALSE)),
        select = utils::head(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test1"
      ),
      list(
        dataname = "ADRS",
        filters = NULL,
        select = utils::tail(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test2"
      ),
      c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    ),
    rlang::set_names(setdiff(c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"), "PARAMCD"))
  )
  # ignore "to" selector's filter
  testthat::expect_identical(
    get_merge_key_pair(
      list(
        dataname = "ADRS",
        filters = NULL,
        select = utils::head(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test1"
      ),
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = "OS", multiple = FALSE)),
        select = utils::tail(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test2"
      ),
      c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    ),
    rlang::set_names(c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )
  # subtract from keys single filter on "from" selector
  testthat::expect_identical(
    get_merge_key_pair(
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = "OS", multiple = FALSE)),
        select = utils::head(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test1"
      ),
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = "OS", multiple = FALSE)),
        select = utils::tail(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test2"
      ),
      c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    ),
    rlang::set_names(setdiff(c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"), "PARAMCD"))
  )
  # subtract if multiple filter and single selection
  testthat::expect_identical(
    get_merge_key_pair(
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = "OS", multiple = TRUE)),
        select = utils::head(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test1"
      ),
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = "OS", multiple = FALSE)),
        select = utils::tail(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test2"
      ),
      c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    ),
    rlang::set_names(setdiff(c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"), "PARAMCD"))
  )
  # ignore if multiple filter with multiple selection
  testthat::expect_identical(
    get_merge_key_pair(
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = c("OS", "PFS"), multiple = TRUE)),
        select = utils::head(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test1"
      ),
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = c("OS", "PFS"), multiple = FALSE)),
        select = utils::tail(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test2"
      ),
      c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    ),
    rlang::set_names(c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )
  # subtract only keys filter cols
  testthat::expect_identical(
    get_merge_key_pair(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "SEX", selected = "F", multiple = FALSE),
          list(columns = "PARAMCD", selected = "OS", multiple = FALSE),
          list(columns = "RACE", selected = "ASIAN", multiple = FALSE)
        ),
        select = utils::head(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test1"
      ),
      list(
        dataname = "ADRS",
        filters = NULL,
        select = utils::tail(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test2"
      ),
      c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    ),
    rlang::set_names(setdiff(c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"), "PARAMCD"))
  )

  # do not subtract from primary key if not reshape and multiple filter column
  testthat::expect_identical(
    get_merge_key_pair(
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = c("OS", "PFS"), multiple = TRUE)),
        select = utils::head(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test1"
      ),
      list(
        dataname = "ADRS",
        filters = NULL,
        select = utils::tail(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test2"
      ),
      c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    ),
    rlang::set_names(c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )
  testthat::expect_identical(
    get_merge_key_pair(
      list(
        dataname = "ADRS",
        filters = NULL,
        select = utils::head(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test1"
      ),
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = c("OS", "PFS"), multiple = TRUE)),
        select = utils::tail(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test2"
      ),
      c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    ),
    rlang::set_names(c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )
  testthat::expect_identical(
    get_merge_key_pair(
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = c("OS", "PFS"), multiple = TRUE)),
        select = utils::head(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test1"
      ),
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = c("OS", "PFS"), multiple = TRUE)),
        select = utils::tail(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test2"
      ),
      c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    ),
    rlang::set_names(c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  ## reshape cases
  # subtract by filter column on "from" selector
  testthat::expect_identical(
    get_merge_key_pair(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "SEX", selected = "F", multiple = TRUE),
          list(columns = "PARAMCD", selected = c("OS", "PFS"), multiple = TRUE),
          list(columns = "RACE", selected = "ASIAN", multiple = TRUE)
        ),
        select = utils::head(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test1"
      ),
      list(
        dataname = "ADRS",
        filters = NULL,
        select = utils::tail(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test2"
      ),
      c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    ),
    rlang::set_names(c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  ## reshape cases
  # subtract by filter column on "from" selector
  testthat::expect_identical(
    get_merge_key_pair(
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = "OS", multiple = TRUE)),
        select = utils::head(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "test1"
      ),
      list(
        dataname = "ADRS",
        filters = NULL,
        select = utils::tail(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "test2"
      ),
      c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    ),
    rlang::set_names(setdiff(c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"), "PARAMCD"))
  )
  # no changes because only "from" filter is used
  testthat::expect_identical(
    get_merge_key_pair(
      list(
        dataname = "ADRS",
        filters = NULL,
        select = utils::head(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "test1"
      ),
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = "OS", multiple = TRUE)),
        select = utils::tail(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "test2"
      ),
      c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    ),
    rlang::set_names(c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )
  # subtract by filter column on "from" selector
  testthat::expect_identical(
    get_merge_key_pair(
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = "OS", multiple = TRUE)),
        select = utils::head(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "test1"
      ),
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = "OS", multiple = TRUE)),
        select = utils::tail(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "test2"
      ),
      c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    ),
    rlang::set_names(setdiff(c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"), "PARAMCD"))
  )
  # subtract by filter key column on "from" selector
  testthat::expect_identical(
    get_merge_key_pair(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "SEX", selected = "F", multiple = TRUE),
          list(columns = "PARAMCD", selected = "OS", multiple = TRUE),
          list(columns = "RACE", selected = "ASIAN", multiple = TRUE)
        ),
        select = utils::head(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "test1"
      ),
      list(
        dataname = "ADRS",
        filters = NULL,
        select = utils::tail(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "test2"
      ),
      c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    ),
    rlang::set_names(setdiff(c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"), "PARAMCD"))
  )

  testthat::expect_identical(
    get_merge_key_pair(
      list(
        dataname = "ADLB",
        filters = list(list(columns = "PARAMCD", selected = "ALT", multiple = FALSE)),
        select = utils::head(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test1"
      ),
      list(
        dataname = "ADLB",
        filters = NULL,
        select = utils::tail(letters, 3),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test2"
      ),
      c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    ),
    rlang::set_names(setdiff(c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"), "PARAMCD"))
  )
})


testthat::test_that("get_merge_call", {
  x <- list(
    list(
      dataname = "ADSL",
      filters = NULL,
      select = utils::head(letters, 3),
      keys = c("STUDYID", "USUBJID"),
      reshape = FALSE,
      internal_id = "test1"
    )
  )
  testthat::expect_identical(
    get_merge_call(x),
    list(quote(ANL <- ANL_1))
  )

  x <- list(
    list(
      dataname = "ADSL",
      filters = NULL,
      select = utils::head(letters, 3),
      keys = c("STUDYID", "USUBJID"),
      reshape = FALSE,
      internal_id = "test1"
    ),
    list(
      dataname = "ADSL",
      filters = NULL,
      select = utils::tail(letters, 3),
      keys = c("STUDYID", "USUBJID"),
      reshape = FALSE,
      internal_id = "test2"
    )
  )
  jk <- teal.data::join_keys(teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")))
  testthat::expect_identical(
    get_merge_call(x, jk),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  )

  x <- list(
    list(
      dataname = "ADRS",
      filters = NULL,
      select = utils::head(letters, 3),
      keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
      reshape = FALSE,
      internal_id = "test1"
    ),
    list(
      dataname = "ADRS",
      filters = NULL,
      select = utils::tail(letters, 3),
      keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
      reshape = FALSE,
      internal_id = "test2"
    )
  )
  jk <- teal.data::join_keys(teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")))
  testthat::expect_identical(
    get_merge_call(x, jk),
    list(
      quote(ANL <- ANL_1),
      quote(
        ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
      )
    )
  )

  x <- list(
    list(
      dataname = "ADSL",
      filters = NULL,
      select = utils::head(letters, 3),
      keys = c("STUDYID", "USUBJID"),
      reshape = FALSE,
      internal_id = "test1"
    ),
    list(
      dataname = "ADRS",
      filters = NULL,
      select = utils::head(letters, 3),
      keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
      reshape = FALSE,
      internal_id = "test2"
    ),
    list(
      dataname = "ADRS",
      filters = NULL,
      select = utils::tail(letters, 3),
      keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
      reshape = FALSE,
      internal_id = "test3"
    )
  )
  jk <- teal.data::join_keys(
    teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADSL", "ADRS", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )
  testthat::expect_identical(
    get_merge_call(x, jk),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))),
      quote(
        ANL <- dplyr::full_join(ANL, ANL_3, by = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
      )
    )
  )
})

testthat::test_that("get_relabel_call", {
  # removes duplicates by name (duplicated variables)
  testthat::expect_identical(
    get_relabel_call(c(
      AGE = "Age",
      SEX = "Sex",
      AVAL = "Continuous Variable",
      AGE = "Age",
      SEX2 = "Sex",
      AVAL = "Continuous Variable2"
    )),
    as.call(
      parse(
        text =
          'teal.data::col_relabel(
          AGE = "Age",
          SEX = "Sex",
          AVAL = "Continuous Variable",
          SEX2 = "Sex")',
        keep.source = FALSE
      )
    )[[1]]
  )


  # empty vector
  testthat::expect_identical(
    get_relabel_call(character(0)),
    NULL
  )

  # unnamed vector
  testthat::expect_identical(
    get_relabel_call(c("a", "b")),
    NULL
  )
})

testthat::test_that("are_all_keys_provided returns TRUE if merged_selector_list has a single element", {
  merged_selector_list_mock <- list(list(dataname = "dataset1"))

  testthat::expect_true(are_needed_keys_provided(NULL, merged_selector_list_mock))
})

testthat::test_that(
  "are_all_keys_provided returns FALSE if slices come from the same dataset and the dataset has no join_keys",
  code = {
    merged_selector_list_mock <- list(
      list(dataname = "dataset1"),
      list(dataname = "dataset1")
    )

    join_keys <- teal.data::join_keys()

    testthat::expect_false(are_needed_keys_provided(join_keys, merged_selector_list_mock))
  }
)

testthat::test_that(
  "are_all_keys_provided returns TRUE if two slices come from the same dataset and the dataset has the join keys",
  code = {
    merged_selector_list_mock <- list(
      list(dataname = "dataset1"),
      list(dataname = "dataset1")
    )

    join_keys <- teal.data::join_keys(
      teal.data::join_key("dataset1", "dataset1", c("key" = "key"))
    )

    testthat::expect_true(are_needed_keys_provided(join_keys, merged_selector_list_mock))
  }
)

testthat::test_that("are_all_keys_provided returns TRUE if no slices are passed", {
  testthat::expect_true(are_needed_keys_provided(list(), list()))
})

testthat::test_that(
  "are_all_keys_provided returns TRUE if three slices from different datasets are provided and have the join keys",
  code = {
    merged_selector_list_mock <- list(
      list(dataname = "dataset1"),
      list(dataname = "dataset2"),
      list(dataname = "dataset3")
    )

    join_keys <- teal.data::join_keys(
      teal.data::join_key("dataset1", "dataset2", c("key" = "key")),
      teal.data::join_key("dataset2", "dataset3", c("key" = "key")),
      teal.data::join_key("dataset1", "dataset3", c("key" = "key"))
    )

    testthat::expect_true(are_needed_keys_provided(join_keys, merged_selector_list_mock))
  }
)

testthat::test_that("validate_keys_sufficient returns TRUE for empty inputs", {
  testthat::expect_true(validate_keys_sufficient(list(), list()))
})

testthat::test_that("validate_keys_sufficient raises an error for two slices with no join keys", {
  merged_selector_list_mock <- list(
    list(dataname = "dataset1"),
    list(dataname = "dataset2")
  )

  testthat::expect_error(validate_keys_sufficient(list(), merged_selector_list_mock))
})
