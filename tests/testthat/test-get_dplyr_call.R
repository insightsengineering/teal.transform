testthat::test_that("get_select_call", {
  testthat::expect_equal(
    get_select_call(select = c("A", "B", "D")),
    quote(dplyr::select(A, B, D))
  )

  testthat::expect_equal(
    get_select_call(select = "D"),
    quote(dplyr::select(D))
  )
  testthat::expect_equal(
    get_select_call(select = NULL),
    NULL
  )
})

testthat::test_that("get_filter_call", {
  testthat::expect_equal(
    get_filter_call(filter = list(list(columns = "SEX", selected = list("F", "M")))),
    quote(dplyr::filter(SEX %in% c("F", "M")))
  )

  testthat::expect_equal(
    get_filter_call(filter = list(
      list(columns = "SEX", selected = list("F", "M")),
      list(columns = "VAR", selected = list("LEVEL1", "LEVEL2"))
    )),
    quote(
      dplyr::filter(SEX %in% c("F", "M") & VAR %in% c("LEVEL1", "LEVEL2"))
    )
  )

  testthat::expect_equal(
    get_filter_call(
      filter = list(
        list(
          columns = c("SEX", "RACE", "BMRKR2"),
          selected = list(c("F", "ASIAN", "HIGHT"), c("M", "ASIAN", "HIGH"))
        )
      )
    ),
    quote(dplyr::filter(
      (SEX == "F" & RACE == "ASIAN" & BMRKR2 == "HIGHT") |
        (SEX == "M" & RACE == "ASIAN" & BMRKR2 == "HIGH")
    ))
  )

  testthat::expect_equal(
    get_filter_call(NULL),
    NULL
  )
})

testthat::test_that("get_rename_call", {
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
      select = letters,
      keys = c("STUDYID", "USUBJID"),
      reshape = FALSE,
      internal_id = "test2"
    ),
    list(
      dataname = "ADSL",
      filters = NULL,
      select = utils::tail(letters, 3),
      keys = c("STUDYID", "USUBJID"),
      reshape = FALSE,
      internal_id = "test3"
    ),
    list(
      dataname = "ADSL",
      filters = NULL,
      select = c("aa", "bb"),
      keys = c("STUDYID", "USUBJID"),
      reshape = FALSE,
      internal_id = "test4"
    )
  )

  testthat::expect_equal(
    get_rename_call(x, 1L),
    quote(dplyr::rename(test1.a = a, test1.b = b, test1.c = c))
  )
  testthat::expect_equal(
    get_rename_call(x, 2L),
    quote(dplyr::rename(test2.a = a, test2.b = b, test2.c = c, test2.x = x, test2.y = y, test2.z = z))
  )
  testthat::expect_equal(
    get_rename_call(x, 3L),
    quote(dplyr::rename(test3.x = x, test3.y = y, test3.z = z))
  )
  testthat::expect_equal(get_rename_call(x, 4L), NULL)
})

testthat::test_that("get_reshape_call", {
  x <- list(
    list(
      dataname = "ADLB",
      filters = list(list(
        columns = c("PARAMCD", "AVISIT"),
        selected = list(c("ALBCV", "SCREENING")),
        multiple = FALSE
      )),
      select = "AVAL",
      keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
      reshape = TRUE,
      internal_id = "test"
    )
  )

  testthat::expect_equal(
    get_reshape_call(x, 1L),
    c(
      quote(tidyr::pivot_longer(cols = "AVAL", names_to = "MEASURE", values_to = "VALUE")),
      quote(tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT)),
      quote(tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE"))
    )
  )

  x <- list(
    list(
      dataname = "ADLB",
      filters = list(list(
        columns = c("PARAMCD", "AVISIT"),
        selected = list(c("ALBCV", "SCREENING"), c("ALBCV", "BASELINE")),
        multiple = TRUE
      )),
      select = "AVAL",
      keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
      reshape = TRUE,
      internal_id = "test"
    )
  )

  testthat::expect_equal(
    get_reshape_call(x, 1L),
    c(
      quote(tidyr::pivot_longer(cols = "AVAL", names_to = "MEASURE", values_to = "VALUE")),
      quote(tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT)),
      quote(tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE"))
    )
  )
})

testthat::test_that("get_dplyr_call - single filter and single select", {
  # no filters no filter call, select keys by default
  testthat::expect_equal(
    get_dplyr_call(
      list(list(
        dataname = "ADSL",
        filters = NULL,
        select = character(0),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "test1"
      )),
      join_keys = teal.data::join_keys(teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")))
    ),
    quote(ADSL %>% dplyr::select(STUDYID, USUBJID))
  )

  # filter call, default select
  testthat::expect_equal(
    get_dplyr_call(
      list(list(
        dataname = "ADSL",
        filters = list(list(columns = "SEX", selected = list("F", "M"))),
        select = character(0),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "test1"
      )),
      join_keys = teal.data::join_keys(teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")))
    ),
    quote(
      ADSL %>%
        dplyr::filter(SEX %in% c("F", "M")) %>%
        dplyr::select(STUDYID, USUBJID)
    )
  )

  # filter call, select $selected with default filters
  testthat::expect_equal(
    get_dplyr_call(
      list(list(
        dataname = "ADSL",
        filters = list(list(columns = "SEX", selected = list("F", "M"))),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "test1"
      )),
      join_keys = teal.data::join_keys(teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")))
    ),
    quote(
      ADSL %>%
        dplyr::filter(SEX %in% c("F", "M")) %>%
        dplyr::select(STUDYID, USUBJID, AVAL)
    )
  )

  # <non-key var> filtered-out, not dropped from selection
  testthat::expect_equal(
    get_dplyr_call(
      list(list(
        dataname = "ADSL",
        filters = list(list(columns = "SEX", selected = list("F"))),
        select = c("AVAL", "SEX"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "test1"
      )),
      join_keys = teal.data::join_keys(teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")))
    ),
    quote(
      ADSL %>%
        dplyr::filter(SEX == "F") %>%
        dplyr::select(STUDYID, USUBJID, AVAL, SEX)
    )
  )

  # <non-key var> filtered-out, not dropped from selection
  # drop_keys = TRUE applies only to keys
  testthat::expect_equal(
    get_dplyr_call(
      list(list(
        dataname = "ADSL",
        filters = list(list(columns = "SEX", selected = list("F"), drop_keys = TRUE)),
        select = c("AVAL", "SEX"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "test1"
      )),
      join_keys = teal.data::join_keys(teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")))
    ),
    quote(
      ADSL %>%
        dplyr::filter(SEX == "F") %>%
        dplyr::select(STUDYID, USUBJID, AVAL, SEX)
    )
  )

  # <key var> filtered-out, dropped from selection
  testthat::expect_equal(
    get_dplyr_call(
      list(list(
        dataname = "ADSL",
        filters = list(list(columns = "STUDYID", selected = list("ANY"), drop_keys = TRUE)),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "test1"
      )),
      join_keys = teal.data::join_keys(teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")))
    ),
    quote(
      ADSL %>%
        dplyr::filter(STUDYID == "ANY") %>%
        dplyr::select(USUBJID, AVAL)
    )
  )

  # <key var> filtered-out, dropped from selection
  testthat::expect_equal(
    get_dplyr_call(
      list(list(
        dataname = "ADSL",
        filters = list(list(columns = "STUDYID", selected = list("ANY"), drop_keys = FALSE)),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "test1"
      )),
      join_keys = teal.data::join_keys(teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")))
    ),
    quote(
      ADSL %>%
        dplyr::filter(STUDYID == "ANY") %>%
        dplyr::select(STUDYID, USUBJID, AVAL)
    )
  )
})

testthat::test_that("get_dplyr_call - multiple filter(s) or multiple select(s)", {
  # no filters, duplicated select is prefixed by selector name
  testthat::expect_equal(
    get_dplyr_call(
      list(
        list(
          dataname = "ADSL",
          filters = NULL,
          select = c("COL_1", "COL_2"),
          keys = c("STUDYID", "USUBJID"),
          reshape = FALSE,
          internal_id = "test1"
        ),
        list(
          dataname = "ADSL",
          filters = NULL,
          select = c("COL_2", "COL_3"),
          keys = c("STUDYID", "USUBJID"),
          reshape = FALSE,
          internal_id = "test2"
        )
      ),
      idx = 1L,
      join_keys = teal.data::join_keys(teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")))
    ),
    quote(
      ADSL %>%
        dplyr::select(STUDYID, USUBJID, COL_1, COL_2) %>%
        dplyr::rename(test1.COL_2 = COL_2)
    )
  )

  # multiple variable filters, single selection, reshape by all keys even if filtered out (to keep proper label)
  testthat::expect_equal(
    get_dplyr_call(
      list(
        list(
          dataname = "ADLB",
          filters = list(
            list(
              columns = c("PARAMCD", "AVISIT"),
              selected = list(c("ALBCV", "SCREENING"), c("ALBCV", "BASELINE")),
              multiple = TRUE
            )
          ),
          select = "AVAL",
          keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
          reshape = TRUE,
          internal_id = "test1"
        )
      ),
      join_keys = teal.data::join_keys(teal.data::join_key("ADLB", "ADLB", c("STUDYID", "USUBJID")))
    ),
    quote(
      ADLB %>%
        dplyr::filter((PARAMCD == "ALBCV" & AVISIT == "SCREENING") | (PARAMCD == "ALBCV" & AVISIT == "BASELINE")) %>%
        dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL) %>%
        tidyr::pivot_longer(cols = "AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
        tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT) %>%
        tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
    )
  )

  # multiple variable filters, single select - one key filtered out and dropped from select
  testthat::expect_equal(
    get_dplyr_call(
      list(list(
        dataname = "ADLB",
        filters = list(list(
          columns = c("PARAMCD", "AVISIT"),
          selected = list(c("ALBCV", "SCREENING"), c("ALBCV", "BASELINE")),
          multiple = TRUE
        )),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "test1"
      )),
      join_keys = teal.data::join_keys(teal.data::join_key("ADLB", "ADLB", c("STUDYID", "USUBJID")))
    ),
    quote(
      ADLB %>%
        dplyr::filter((PARAMCD == "ALBCV" & AVISIT == "SCREENING") | (PARAMCD == "ALBCV" & AVISIT == "BASELINE")) %>%
        dplyr::select(STUDYID, USUBJID, AVAL)
    )
  )
})
