# Different join keys ------
testthat::test_that("Different join types", {
  merged_selectors1 <- merge_selectors(
    list(
      list(
        dataname = "ADSL",
        filters = NULL,
        select = "AGE",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADSL2",
        filters = NULL,
        select = c("SEX", "STRATA"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x2"
      )
    )
  )
  jk1 <- teal.data::join_keys(
    teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADSL2", "ADSL2", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADSL", "ADSL2", c("STUDYID", "USUBJID"))
  )


  testthat::expect_identical(
    get_merge_call(merged_selectors1[[1]], jk1, merge_function = "dplyr::left_join"),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::left_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors1[[1]], jk1, merge_function = "dplyr::inner_join"),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::inner_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors1[[1]], jk1, merge_function = "merge.data.frame"),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- merge.data.frame(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  )

  testthat::expect_error(
    get_merge_call(merged_selectors1[[1]], jk1, merge_function = "base::merge"),
    ".... is not TRUE"
  )

  testthat::expect_error(
    get_merge_call(merged_selectors1[[1]], jk1, merge_function = "non_existing_function"),
    "'non_existing_function' not found"
  )
})

### Single wide -----
testthat::test_that("Single wide dataset", {
  # __1. one selector - single select -----
  merged_selectors1 <- merge_selectors(
    list(
      list(
        dataname = "ADSL",
        filters = NULL,
        select = "AGE",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        select = "AGE",
        internal_id = "x1"
      )
    )
  )[[1]]
  jk1 <- teal.data::join_keys(
    teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    get_dplyr_call(merged_selectors1, join_keys = jk1),
    quote(
      ADSL %>%
        dplyr::select(STUDYID, USUBJID, AGE)
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors1, jk1),
    list(quote(ANL <- ANL_1))
  )

  # __2. one selector - multiple select -----
  merged_selectors2 <- merge_selectors(
    list(
      list(
        dataname = "ADSL",
        filters = NULL,
        select = c("USUBJID", "AGE", "SEX"), # adding USUBJID doesn"t affect result - keys are selected always
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x1"
      )
    )
  )[[1]]
  jk2 <- teal.data::join_keys(
    teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    get_dplyr_call(merged_selectors2, join_keys = jk2),
    quote(
      ADSL %>%
        dplyr::select(STUDYID, USUBJID, AGE, SEX)
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors2, jk2),
    list(quote(ANL <- ANL_1))
  )

  # __3. two selectors -------
  merged_selectors3 <- merge_selectors(
    list(
      list(
        dataname = "ADSL",
        filters = NULL,
        select = "AGE",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADSL",
        filters = NULL,
        select = c("SEX", "AGE", "BMRKR1"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk3 <- teal.data::join_keys(
    teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    get_dplyr_call(merged_selectors3, join_keys = jk3),
    quote(
      ADSL %>%
        dplyr::select(STUDYID, USUBJID, AGE, SEX, BMRKR1)
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors3, jk3),
    list(quote(ANL <- ANL_1))
  )

  # __4. multiple selectors -------
  merged_selectors4 <- merge_selectors(
    list(
      list(
        dataname = "ADSL",
        filters = NULL,
        select = "AGE",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADSL",
        filters = NULL,
        select = c("SEX", "AGE", "BMRKR1"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x2"
      ),
      list(
        dataname = "ADSL",
        filters = NULL,
        select = c("STRATA", "ARMCD", "BMRKR1"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x3"
      ),
      list(
        dataname = "ADSL",
        filters = NULL,
        select = c("COUNTRY"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x4"
      )
    )
  )[[1]]
  jk4 <- teal.data::join_keys(
    teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    get_dplyr_call(merged_selectors4, join_keys = jk4),
    quote(
      ADSL %>%
        dplyr::select(STUDYID, USUBJID, AGE, SEX, BMRKR1, STRATA, ARMCD, COUNTRY)
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors4, jk4),
    list(quote(ANL <- ANL_1))
  )

  # __5. multiple selectors with empty selections ------
  merged_selectors5 <- merge_selectors(
    list(
      list(
        dataname = "ADSL",
        filters = NULL,
        select = character(0),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADSL",
        filters = NULL,
        select = character(0),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x2"
      ),
      list(
        dataname = "ADSL",
        filters = NULL,
        select = c("STRATA", "ARMCD", "BMRKR1"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x3"
      ),
      list(
        dataname = "ADSL",
        filters = NULL,
        select = c("COUNTRY"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x4"
      )
    )
  )[[1]]
  jk5 <- teal.data::join_keys(
    teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    get_dplyr_call(merged_selectors5, join_keys = jk5),
    quote(
      ADSL %>%
        dplyr::select(STUDYID, USUBJID, STRATA, ARMCD, BMRKR1, COUNTRY)
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors5, jk5),
    list(quote(ANL <- ANL_1))
  )

  # __6. one selector - single select - filter -----
  merged_selectors6 <- merge_selectors(
    list(
      list(
        dataname = "ADSL",
        filters = list(list(columns = "SEX", selected = "F", multiple = TRUE, drop_keys = FALSE)),
        select = "AGE",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x1"
      )
    )
  )[[1]]
  jk6 <- teal.data::join_keys(
    teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    get_dplyr_call(merged_selectors6, join_keys = jk6),
    quote(
      ADSL %>%
        dplyr::filter(SEX == "F") %>%
        dplyr::select(STUDYID, USUBJID, AGE)
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors6, jk6),
    list(quote(ANL <- ANL_1))
  )

  # __7. one selector - single select - filter on key -----
  merged_selectors7 <- merge_selectors(
    list(
      list(
        dataname = "ADSL",
        filters = list(list(columns = "STUDYID", selected = "STUDY1", multiple = TRUE, drop_keys = FALSE)),
        select = "AGE",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x1"
      )
    )
  )[[1]]
  jk7 <- teal.data::join_keys(
    teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    get_dplyr_call(merged_selectors7, join_keys = jk7),
    quote(
      ADSL %>%
        dplyr::filter(STUDYID == "STUDY1") %>%
        dplyr::select(STUDYID, USUBJID, AGE)
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors7, jk7),
    list(quote(ANL <- ANL_1))
  )

  # __8. one selector - drop keys -----
  merged_selectors8 <- merge_selectors(
    list(
      list(
        dataname = "ADSL",
        filters = list(list(columns = "STUDYID", selected = "STUDY1", multiple = TRUE, drop_keys = TRUE)),
        select = "AGE",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x1"
      )
    )
  )[[1]]
  jk8 <- teal.data::join_keys(
    teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    get_dplyr_call(merged_selectors8, join_keys = jk8),
    quote(
      ADSL %>%
        dplyr::filter(STUDYID == "STUDY1") %>%
        dplyr::select(USUBJID, AGE)
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors8, jk8),
    list(quote(ANL <- ANL_1))
  )

  # __9. one selector - key dropped but selected -----
  merged_selectors9 <- merge_selectors(
    list(
      list(
        dataname = "ADSL",
        filters = list(list(columns = "STUDYID", selected = "STUDY1", multiple = TRUE, drop_keys = TRUE)),
        select = c("AGE", "STUDYID"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x1"
      )
    )
  )[[1]]
  jk9 <- teal.data::join_keys(
    teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    get_dplyr_call(merged_selectors9, join_keys = jk9),
    quote(
      ADSL %>%
        dplyr::filter(STUDYID == "STUDY1") %>%
        dplyr::select(USUBJID, AGE, STUDYID)
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors9, jk9),
    list(quote(ANL <- ANL_1))
  )
})

### Multiple wide -----
testthat::test_that("Multiple wide dataset", {
  # __1. two selectors ------
  merged_selectors1 <- merge_selectors(
    list(
      list(
        dataname = "ADSL",
        filters = NULL,
        select = "AGE",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADSL2",
        filters = NULL,
        select = c("SEX", "STRATA"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x2"
      )
    )
  )
  jk1 <- teal.data::join_keys(
    teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADSL2", "ADSL2", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADSL", "ADSL2", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    list(
      get_dplyr_call(merged_selectors1[[1]], 1L, jk1),
      get_dplyr_call(merged_selectors1[[1]], 2L, jk1)
    ),
    list(
      quote(
        ADSL %>%
          dplyr::select(STUDYID, USUBJID, AGE)
      ),
      quote(
        ADSL2 %>%
          dplyr::select(STUDYID, USUBJID, SEX, STRATA)
      )
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors1[[1]], jk1),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  )

  # __2. Two selectors (non-empty and empty) ------
  merged_selectors2 <- merge_selectors(
    list(
      list(
        dataname = "ADSL",
        filters = NULL,
        select = "AGE",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADSL2",
        filters = NULL,
        select = character(0),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk2 <- teal.data::join_keys(
    teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADSL", "ADSL2", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    list(
      get_dplyr_call(merged_selectors2, 1L, jk2),
      get_dplyr_call(merged_selectors2, 2L, jk2)
    ),
    list(
      quote(
        ADSL %>%
          dplyr::select(STUDYID, USUBJID, AGE)
      ),
      quote(
        ADSL2 %>%
          dplyr::select(STUDYID, USUBJID)
      )
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors2, jk2),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  )

  # __3. multiple selectors ------
  merged_selectors3 <- merge_selectors(
    list(
      list(
        dataname = "ADSL",
        filters = NULL,
        select = "AGE",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADSL2",
        filters = NULL,
        select = c("AGE", "SEX"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x2"
      ),
      list(
        dataname = "ADSL3",
        filters = NULL,
        select = c("AGE", "SEX", "STRATA"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x3"
      ),
      list(
        dataname = "ADSL2",
        filters = NULL,
        select = c("AGE", "STRATA", "BMRKR1"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x4"
      )
    )
  )[[1]]
  jk3 <- teal.data::join_keys(
    teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADSL", "ADSL2", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADSL", "ADSL3", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    list(
      get_dplyr_call(merged_selectors3, 1L, jk3),
      get_dplyr_call(merged_selectors3, 2L, jk3),
      get_dplyr_call(merged_selectors3, 3L, jk3)
    ),
    list(
      quote(
        ADSL %>%
          dplyr::select(STUDYID, USUBJID, AGE) %>%
          dplyr::rename(x1.AGE = AGE)
      ),
      quote(
        ADSL2 %>%
          dplyr::select(STUDYID, USUBJID, AGE, SEX, STRATA, BMRKR1) %>%
          dplyr::rename(x2.AGE = AGE, x2.SEX = SEX, x2.STRATA = STRATA)
      ),
      quote(
        ADSL3 %>%
          dplyr::select(STUDYID, USUBJID, AGE, SEX, STRATA) %>%
          dplyr::rename(x3.AGE = AGE, x3.SEX = SEX, x3.STRATA = STRATA)
      )
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors3, jk3),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))),
      quote(ANL <- dplyr::full_join(ANL, ANL_3, by = c("STUDYID", "USUBJID")))
    )
  )

  # __4. multiple selectors (with empty) ------
  merged_selectors4 <- merge_selectors(
    list(
      list(
        dataname = "ADSL",
        filters = NULL,
        select = "AGE",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADSL2",
        filters = NULL,
        select = c("AGE", "SEX"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x2"
      ),
      list(
        dataname = "ADSL3",
        filters = NULL,
        select = character(0),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x3"
      ),
      list(
        dataname = "ADSL2",
        filters = NULL,
        select = character(0),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x4"
      )
    )
  )[[1]]
  jk4 <- teal.data::join_keys(
    teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADSL", "ADSL2", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADSL", "ADSL3", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    list(
      get_dplyr_call(merged_selectors4, 1L, jk4),
      get_dplyr_call(merged_selectors4, 2L, jk4),
      get_dplyr_call(merged_selectors4, 3L, jk4)
    ),
    list(
      quote(
        ADSL %>%
          dplyr::select(STUDYID, USUBJID, AGE) %>%
          dplyr::rename(x1.AGE = AGE)
      ),
      quote(
        ADSL2 %>%
          dplyr::select(STUDYID, USUBJID, AGE, SEX) %>%
          dplyr::rename(x2.AGE = AGE)
      ),
      quote(
        ADSL3 %>%
          dplyr::select(STUDYID, USUBJID)
      )
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors4, jk4),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))),
      quote(ANL <- dplyr::full_join(ANL, ANL_3, by = c("STUDYID", "USUBJID")))
    )
  )

  # __5. multiple selectors - including keys ------
  merged_selectors5 <- merge_selectors(
    list(
      list(
        dataname = "ADSL",
        filters = NULL,
        select = c("STUDYID", "AGE"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADSL2",
        filters = NULL,
        select = c("STUDYID", "AGE", "SEX"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x2"
      ),
      list(
        dataname = "ADSL",
        filters = NULL,
        select = "STUDYID",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x3"
      ),
      list(
        dataname = "ADSL3",
        filters = NULL,
        select = c("USUBJID", "AGE", "SEX", "STRATA"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x4"
      ),
      list(
        dataname = "ADSL2",
        filters = NULL,
        select = c("AGE", "STRATA", "BMRKR1"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x5"
      )
    )
  )[[1]]
  jk5 <- teal.data::join_keys(
    teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADSL", "ADSL2", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADSL", "ADSL3", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    list(
      get_dplyr_call(merged_selectors5, 1L, jk5),
      get_dplyr_call(merged_selectors5, 2L, jk5),
      get_dplyr_call(merged_selectors5, 3L, jk5)
    ),
    list(
      quote(
        ADSL %>%
          dplyr::select(STUDYID, USUBJID, AGE) %>%
          dplyr::rename(x1.AGE = AGE)
      ),
      quote(
        ADSL2 %>%
          dplyr::select(STUDYID, USUBJID, AGE, SEX, STRATA, BMRKR1) %>%
          dplyr::rename(x2.AGE = AGE, x2.SEX = SEX, x2.STRATA = STRATA)
      ),
      quote(
        ADSL3 %>%
          dplyr::select(STUDYID, USUBJID, AGE, SEX, STRATA) %>%
          dplyr::rename(x4.AGE = AGE, x4.SEX = SEX, x4.STRATA = STRATA)
      )
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors5, jk5),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))),
      quote(ANL <- dplyr::full_join(ANL, ANL_3, by = c("STUDYID", "USUBJID")))
    )
  )
})

### Single long -----
testthat::test_that("Single long dataset", {
  # __1a. single variable ----
  merged_selectors1 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE, drop_keys = FALSE)),
        select = "AGE",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      )
    )
  )[[1]]
  jk1 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_identical(
    get_dplyr_call(merged_selectors1, join_keys = jk1),
    quote(
      ADRS %>%
        dplyr::filter(PARAMCD == "BESRSPI") %>%
        dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AGE)
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors1, jk1),
    list(quote(ANL <- ANL_1))
  )

  # __1b. single variable - drop_keys = TRUE ----
  merged_selectors1 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE, drop_keys = TRUE)),
        select = "AGE",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      )
    )
  )[[1]]
  jk1 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_identical(
    get_dplyr_call(merged_selectors1, join_keys = jk1),
    quote(
      ADRS %>%
        dplyr::filter(PARAMCD == "BESRSPI") %>%
        dplyr::select(STUDYID, USUBJID, AVISIT, AGE)
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors1, jk1),
    list(quote(ANL <- ANL_1))
  )

  # __2. multiple variables ----
  merged_selectors2 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE, drop_keys = TRUE),
          list(columns = "AVISIT", selected = c("BASELINE", "SCREENING"), multiple = TRUE, drop_keys = TRUE)
        ),
        select = c("AGE", "SEX"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      )
    )
  )[[1]]
  jk2 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    get_dplyr_call(merged_selectors2, join_keys = jk2),
    quote(
      ADRS %>%
        dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("BASELINE", "SCREENING")) %>%
        dplyr::select(STUDYID, USUBJID, AVISIT, AGE, SEX)
    )
  ) # AVISIT should be in select - filter is not complete (two levels selected)

  testthat::expect_identical(
    get_merge_call(merged_selectors2, jk2),
    list(quote(ANL <- ANL_1))
  )

  # __3.a two selectors same filters (one filter not complete) ----
  merged_selectors3 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE, drop_keys = FALSE),
          list(columns = "AVISIT", selected = c("BASELINE", "SCREENING"), multiple = TRUE, drop_keys = FALSE)
        ),
        select = "AGE",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE, drop_keys = FALSE),
          list(columns = "AVISIT", selected = c("BASELINE", "SCREENING"), multiple = TRUE, drop_keys = FALSE)
        ),
        select = c("SEX", "AGE", "BMRKR1"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk3 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    deparse(get_dplyr_call(merged_selectors3, join_keys = jk3), 120),
    deparse(quote(
      ADRS %>%
        dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("BASELINE", "SCREENING")) %>%
        dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AGE, SEX, BMRKR1)
    ), 120)
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors3),
    list(quote(ANL <- ANL_1))
  )

  # __3.b two selectors same filters (one filter not complete) ----
  merged_selectors3 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE, drop_keys = TRUE),
          list(columns = "AVISIT", selected = c("BASELINE", "SCREENING"), multiple = TRUE, drop_keys = TRUE)
        ),
        select = "AGE",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE, drop_keys = FALSE),
          list(columns = "AVISIT", selected = c("BASELINE", "SCREENING"), multiple = TRUE, drop_keys = FALSE)
        ),
        select = c("SEX", "AGE", "BMRKR1"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk3 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  # failure:

  testthat::expect_equal(
    list(
      get_dplyr_call(merged_selectors3, idx = 1L, join_keys = jk3),
      get_dplyr_call(merged_selectors3, idx = 2L, join_keys = jk3)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("BASELINE", "SCREENING")) %>%
          dplyr::select(STUDYID, USUBJID, AVISIT, AGE) %>%
          dplyr::rename(x1.AGE = AGE)
      ),
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("BASELINE", "SCREENING")) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, SEX, AGE, BMRKR1) %>%
          dplyr::rename(x2.AGE = AGE)
      )
    )
  )

  # failure - difference in keys caused an difference between the same datasets!
  testthat::expect_identical(
    get_merge_call(merged_selectors3, jk3),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID", "AVISIT")))
    )
  )

  # __ 4. two selectors different filters (complete on two keys)------
  merged_selectors4 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE, drop_keys = TRUE),
          list(columns = "AVISIT", selected = "BASELINE", multiple = TRUE, drop_keys = TRUE)
        ),
        select = "AGE",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE, drop_keys = TRUE),
          list(columns = "AVISIT", selected = "SCREENING", multiple = TRUE, drop_keys = TRUE)
        ),
        select = c("SEX", "AGE", "BMRKR1"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk4 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    list(
      get_dplyr_call(merged_selectors4, 1L, jk4),
      get_dplyr_call(merged_selectors4, 2L, jk4)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT == "BASELINE") %>%
          dplyr::select(STUDYID, USUBJID, AGE) %>%
          dplyr::rename(x1.AGE = AGE)
      ),
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT == "SCREENING") %>%
          dplyr::select(STUDYID, USUBJID, SEX, AGE, BMRKR1) %>%
          dplyr::rename(x2.AGE = AGE)
      )
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors4, jk4),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  )

  # __ 5.two selectors different filters (on 2 keys/on 1 key)-----
  merged_selectors5 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = c("BASELINE"), multiple = TRUE)
        ),
        select = "AGE",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE)),
        select = c("SEX", "AGE", "BMRKR1"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk5 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    list(
      get_dplyr_call(merged_selectors5, 1L, jk5),
      get_dplyr_call(merged_selectors5, 2L, jk5)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT == "BASELINE") %>%
          dplyr::select(STUDYID, USUBJID, AGE) %>%
          dplyr::rename(x1.AGE = AGE)
      ),
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI") %>%
          dplyr::select(STUDYID, USUBJID, AVISIT, SEX, AGE, BMRKR1) %>%
          dplyr::rename(x2.AGE = AGE)
      )
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors5, jk5),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  )

  # __ 6. two selectors different filters -----
  merged_selectors6 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = c("BASELINE", "SCREENING"), multiple = TRUE)
        ),
        select = "AGE",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = c("SCREENING", "FOLLOW UP"), multiple = TRUE)
        ),
        select = c("SEX", "AGE", "BMRKR1"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk6 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    list(
      deparse(get_dplyr_call(merged_selectors6, 1L, jk6), 120),
      deparse(get_dplyr_call(merged_selectors6, 2L, jk6), 120)
    ),
    list(
      deparse(quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("BASELINE", "SCREENING")) %>%
          dplyr::select(STUDYID, USUBJID, AVISIT, AGE) %>%
          dplyr::rename(x1.AGE = AGE)
      ), 120),
      deparse(quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("SCREENING", "FOLLOW UP")) %>%
          dplyr::select(STUDYID, USUBJID, AVISIT, SEX, AGE, BMRKR1) %>%
          dplyr::rename(x2.AGE = AGE)
      ), 120)
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors6, jk6),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID", "AVISIT")))
    )
  )

  # __ 7. two selectors different filters - drop PARAMCD when filtered-out on different levels -----
  merged_selectors7 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = c("BASELINE", "SCREENING"), multiple = TRUE),
          list(columns = "SEX", selected = c("F", "M"), multiple = TRUE)
        ),
        select = "AGE",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "INVET", multiple = TRUE),
          list(columns = "AVISIT", selected = c("SCREENING", "FOLLOW UP"), multiple = TRUE)
        ),
        select = c("SEX", "AGE", "BMRKR1"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk7 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    list(
      get_dplyr_call(merged_selectors7, 1L, jk7),
      get_dplyr_call(merged_selectors7, 2L, jk7)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("BASELINE", "SCREENING") & SEX %in% c("F", "M")) %>%
          dplyr::select(STUDYID, USUBJID, AVISIT, AGE) %>%
          dplyr::rename(x1.AGE = AGE)
      ),
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "INVET" & AVISIT %in% c("SCREENING", "FOLLOW UP")) %>%
          dplyr::select(STUDYID, USUBJID, AVISIT, SEX, AGE, BMRKR1) %>%
          dplyr::rename(x2.AGE = AGE)
      )
    )
  ) # AVISIT should be included in select

  testthat::expect_identical(
    get_merge_call(merged_selectors7, jk7),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID", "AVISIT")))
    )
  )


  # __ 8. multiple selectors different filters ------
  merged_selectors8 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "INVET", multiple = TRUE),
          list(columns = "AVISIT", selected = c("BASELINE", "SCREENING"), multiple = TRUE),
          list(columns = "SEX", selected = c("F", "M"), multiple = TRUE)
        ),
        select = "AGE",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = "SCREENING", multiple = TRUE)
        ),
        select = character(0),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x2"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = c("SCREENING", "FOLLOW UP"), multiple = TRUE)
        ),
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x3"
      )
    )
  )[[1]]
  jk8 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    list(
      get_dplyr_call(merged_selectors8, 1L, jk8),
      get_dplyr_call(merged_selectors8, 2L, jk8),
      get_dplyr_call(merged_selectors8, 3L, jk8)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "INVET" & AVISIT %in% c("BASELINE", "SCREENING") & SEX %in% c("F", "M")) %>%
          dplyr::select(STUDYID, USUBJID, AVISIT, AGE) %>%
          dplyr::rename(x1.AGE = AGE)
      ),
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT == "SCREENING") %>%
          dplyr::select(STUDYID, USUBJID)
      ),
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("SCREENING", "FOLLOW UP")) %>%
          dplyr::select(STUDYID, USUBJID, AVISIT, AVAL, AGE) %>%
          dplyr::rename(x3.AGE = AGE)
      )
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors8, jk8),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))),
      quote(ANL <- dplyr::full_join(ANL, ANL_3, by = c("STUDYID", "USUBJID", "AVISIT")))
    )
  )

  # __ 9. Reshape one filter -------
  merged_selectors9 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE, drop_keys = TRUE)),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x1"
      )
    )
  )[[1]]
  jk9 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_identical(
    get_dplyr_call(merged_selectors9, join_keys = jk9),
    quote(
      ADRS %>%
        dplyr::filter(PARAMCD == "BESRSPI") %>%
        dplyr::select(STUDYID, USUBJID, AVISIT, PARAMCD, AVAL) %>%
        tidyr::pivot_longer(cols = "AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
        tidyr::unite(KEY, MEASURE, PARAMCD) %>%
        tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
    )
  )

  testthat::expect_identical(
    get_reshape_unite_vals(merged_selectors9[[1]]),
    "BESRSPI"
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors9, jk9),
    list(quote(ANL <- ANL_1))
  )

  # __ 10. Reshape two filters ------
  merged_selectors10 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = c("BASELINE", "SCREENING"), multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x1"
      )
    )
  )[[1]]
  jk10 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    get_dplyr_call(merged_selectors10, join_keys = jk10),
    quote(
      ADRS %>%
        dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("BASELINE", "SCREENING")) %>%
        dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL) %>%
        tidyr::pivot_longer(cols = "AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
        tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT) %>%
        tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
    )
  )

  testthat::expect_identical(
    get_reshape_unite_vals(merged_selectors10[[1]]),
    c("BESRSPI_BASELINE", "BESRSPI_SCREENING")
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors10, jk10),
    list(quote(ANL <- ANL_1))
  )

  # __ 11. Reshape does not include non-key filter ------
  merged_selectors11 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          # drop_keys ignored because of PARAMCD inclusion in reshape
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE, drop_keys = TRUE),
          list(columns = "SEX", selected = c("M", "F"), multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x1"
      )
    )
  )[[1]]
  jk11 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    get_dplyr_call(merged_selectors11, join_keys = jk11),
    quote(
      ADRS %>%
        dplyr::filter(PARAMCD == "BESRSPI" & SEX %in% c("M", "F")) %>%
        dplyr::select(STUDYID, USUBJID, AVISIT, PARAMCD, AVAL) %>%
        tidyr::pivot_longer(cols = "AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
        tidyr::unite(KEY, MEASURE, PARAMCD) %>%
        tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
    )
  )

  testthat::expect_identical(
    get_reshape_unite_vals(merged_selectors11[[1]]),
    "BESRSPI"
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors11, jk11),
    list(quote(ANL <- ANL_1))
  )

  # __ 12. Reshape two selectors - same filters same select -------
  merged_selectors12 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = c("BASELINE", "SCREENING"), multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = c("BASELINE", "SCREENING"), multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk12 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    get_dplyr_call(merged_selectors12, 1L, jk12),
    quote(
      ADRS %>%
        dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("BASELINE", "SCREENING")) %>%
        dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL) %>%
        tidyr::pivot_longer(cols = "AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
        tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT) %>%
        tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
    )
  )

  testthat::expect_identical(
    get_reshape_unite_vals(merged_selectors12[[1]]),
    c("BESRSPI_BASELINE", "BESRSPI_SCREENING")
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors12, jk12),
    list(quote(ANL <- ANL_1))
  )

  # __13. Reshape two selectors - different filters ------
  merged_selectors13 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "INVET", multiple = TRUE),
          list(columns = "AVISIT", selected = c("BASELINE", "SCREENING"), multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = c("SCREENING", "FOLLOW UP"), multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk13 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    list(
      get_dplyr_call(merged_selectors13, 1L, jk13),
      get_dplyr_call(merged_selectors13, 2L, jk13)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "INVET" & AVISIT %in% c("BASELINE", "SCREENING")) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL) %>%
          dplyr::rename(x1.AVAL = AVAL) %>%
          tidyr::pivot_longer(cols = "x1.AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      ),
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("SCREENING", "FOLLOW UP")) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL) %>%
          dplyr::rename(x2.AVAL = AVAL) %>%
          tidyr::pivot_longer(cols = "x2.AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      )
    )
  )

  testthat::expect_identical(
    list(get_reshape_unite_vals(merged_selectors13[[1]]), get_reshape_unite_vals(merged_selectors13[[2]])),
    list(c("INVET_BASELINE", "INVET_SCREENING"), c("BESRSPI_SCREENING", "BESRSPI_FOLLOW UP"))
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors13, jk13),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  )

  # __14. Reshape two datasets - different filters, one with empty select ------
  # reshape = TRUE and select = character(0) throws error - pivot longer
  merged_selectors14 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = c("BASELINE", "SCREENING"), multiple = TRUE)
        ),
        select = character(0),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        select = character(0),
        internal_id = "x1"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = c("SCREENING", "FOLLOW UP"), multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk14 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    list(
      get_dplyr_call(merged_selectors14, 1L, jk14),
      get_dplyr_call(merged_selectors14, 2L, jk14)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("BASELINE", "SCREENING")) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT) %>%
          tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      ),
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("SCREENING", "FOLLOW UP")) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL) %>%
          tidyr::pivot_longer(cols = "AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      )
    )
  )

  testthat::expect_identical(
    list(get_reshape_unite_vals(merged_selectors14[[1]]), get_reshape_unite_vals(merged_selectors14[[2]])),
    list(c("BESRSPI_BASELINE", "BESRSPI_SCREENING"), c("BESRSPI_SCREENING", "BESRSPI_FOLLOW UP"))
  )


  testthat::expect_identical(
    get_merge_call(merged_selectors14, jk14),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  )

  # __15. Reshape - key filter + non-key filter--------
  # comment: if no key-filters selected -> reshape should be set to FALSE
  merged_selectors15 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(list(columns = "SEX", selected = c("M", "F"), multiple = TRUE)),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = c("SCREENING", "FOLLOW UP"), multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk15 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    list(
      get_dplyr_call(merged_selectors15, 1L, jk15),
      get_dplyr_call(merged_selectors15, 2L, jk15)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter(SEX %in% c("M", "F")) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL) %>%
          dplyr::rename(x1.AVAL = AVAL) %>%
          tidyr::pivot_longer(cols = "x1.AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      ),
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("SCREENING", "FOLLOW UP")) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL) %>%
          dplyr::rename(x2.AVAL = AVAL) %>%
          tidyr::pivot_longer(cols = "x2.AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      )
    )
  )

  testthat::expect_identical(
    list(get_reshape_unite_vals(merged_selectors15[[1]]), get_reshape_unite_vals(merged_selectors15[[2]])),
    list(character(0), c("BESRSPI_SCREENING", "BESRSPI_FOLLOW UP"))
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors15, jk15),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  )

  # __16. Reshape - multiple filters, one combined --------
  merged_selectors16 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "AVISIT", selected = c("BASELINE", "SCREENING"), multiple = TRUE),
          list(columns = "SEX", selected = c("F", "M"), multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "INVET", multiple = TRUE),
          list(columns = "AVISIT", selected = "SCREENING", multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x2"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = c("SCREENING", "FOLLOW UP"), multiple = TRUE)
        ),
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x3"
      )
    )
  )[[1]]
  jk16 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    list(
      deparse(get_dplyr_call(merged_selectors16, 1L, jk16), 120),
      deparse(get_dplyr_call(merged_selectors16, 2L, jk16), 120),
      deparse(get_dplyr_call(merged_selectors16, 3L, jk16), 140)
    ),
    list(
      deparse(quote(
        ADRS %>%
          dplyr::filter(AVISIT %in% c("BASELINE", "SCREENING") & SEX %in% c("F", "M")) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL) %>%
          dplyr::rename(x1.AVAL = AVAL) %>%
          tidyr::pivot_longer(cols = "x1.AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, AVISIT) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      ), 120),
      deparse(quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "INVET" & AVISIT == "SCREENING") %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL) %>%
          dplyr::rename(x2.AVAL = AVAL) %>%
          tidyr::pivot_longer(cols = "x2.AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      ), 120),
      deparse(quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("SCREENING", "FOLLOW UP")) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL, AGE) %>%
          dplyr::rename(x3.AVAL = AVAL) %>%
          tidyr::pivot_longer(cols = c("x3.AVAL", "AGE"), names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      ), 140)
    )
  )

  testthat::expect_identical(
    list(
      get_reshape_unite_vals(merged_selectors16[[1]]),
      get_reshape_unite_vals(merged_selectors16[[2]]),
      get_reshape_unite_vals(merged_selectors16[[3]])
    ),
    list(c("BASELINE", "SCREENING"), c("INVET_SCREENING"), c("BESRSPI_SCREENING", "BESRSPI_FOLLOW UP"))
  )


  testthat::expect_identical(
    get_merge_call(merged_selectors16, jk16),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))),
      quote(ANL <- dplyr::full_join(ANL, ANL_3, by = c("STUDYID", "USUBJID")))
    )
  )

  # __17. Reshape - multiple filters, one combined --------
  merged_selectors17 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = c("BESRSPI", "INVET"), multiple = TRUE),
          list(columns = "AVISIT", selected = c("BASELINE", "SCREENING"), multiple = TRUE),
          list(columns = "SEX", selected = c("F", "M"), multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE, drop_keys = TRUE),
          list(columns = "AVISIT", selected = "SCREENING", multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x2"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE, drop_keys = TRUE),
          list(columns = "AVISIT", selected = c("SCREENING", "FOLLOW UP"), multiple = TRUE)
        ),
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x3"
      )
    )
  )[[1]]
  jk17 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    list(
      deparse(get_dplyr_call(merged_selectors17, 1L, jk17), 200),
      deparse(get_dplyr_call(merged_selectors17, 2L, jk17), 200),
      deparse(get_dplyr_call(merged_selectors17, 3L, jk17), 300)
    ),
    list(
      deparse(quote(
        ADRS %>%
          dplyr::filter(
            PARAMCD %in% c("BESRSPI", "INVET") & AVISIT %in% c("BASELINE", "SCREENING") & SEX %in% c("F", "M")
          ) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL) %>%
          dplyr::rename(x1.AVAL = AVAL) %>%
          tidyr::pivot_longer(cols = "x1.AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      ), 200),
      deparse(quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT == "SCREENING") %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL) %>%
          dplyr::rename(x2.AVAL = AVAL) %>%
          tidyr::pivot_longer(cols = "x2.AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      ), 200),
      deparse(quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("SCREENING", "FOLLOW UP")) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL, AGE) %>%
          dplyr::rename(x3.AVAL = AVAL) %>%
          tidyr::pivot_longer(cols = c("x3.AVAL", "AGE"), names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      ), 300)
    )
  )

  testthat::expect_identical(
    list(
      get_reshape_unite_vals(merged_selectors17[[1]]),
      get_reshape_unite_vals(merged_selectors17[[2]]),
      get_reshape_unite_vals(merged_selectors17[[3]])
    ),
    list(
      c("BESRSPI_BASELINE", "INVET_BASELINE", "BESRSPI_SCREENING", "INVET_SCREENING"),
      c("BESRSPI_SCREENING"),
      c("BESRSPI_SCREENING", "BESRSPI_FOLLOW UP")
    )
  )


  testthat::expect_identical(
    get_merge_call(merged_selectors17, jk17),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))),
      quote(ANL <- dplyr::full_join(ANL, ANL_3, by = c("STUDYID", "USUBJID")))
    )
  )

  # __18. Don"t Reshape key as value -------
  merged_selectors18 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE)),
        select = c("AVAL", "STUDYID"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x1"
      )
    )
  )[[1]]
  jk18 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_identical(
    get_dplyr_call(merged_selectors18, join_keys = jk18),
    quote(
      ADRS %>%
        dplyr::filter(PARAMCD == "BESRSPI") %>%
        dplyr::select(STUDYID, USUBJID, AVISIT, PARAMCD, AVAL) %>%
        tidyr::pivot_longer(cols = "AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
        tidyr::unite(KEY, MEASURE, PARAMCD) %>%
        tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
    )
  )

  testthat::expect_identical(
    get_reshape_unite_vals(merged_selectors18[[1]]),
    "BESRSPI"
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors18, jk18),
    list(quote(ANL <- ANL_1))
  )


  # __20. multiple selectors different filters - keys in select ------
  merged_selectors20 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = c("BASELINE", "SCREENING"), multiple = TRUE),
          list(columns = "SEX", selected = c("F", "M"), multiple = TRUE)
        ),
        select = c("STUDYID", "PARAMCD", "AVISIT", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = "SCREENING", multiple = TRUE)
        ),
        select = c("STUDYID", "PARAMCD", "AVISIT"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x2"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = c("SCREENING", "FOLLOW UP"), multiple = TRUE)
        ),
        select = c("STUDYID", "PARAMCD", "AVISIT", "AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x3"
      )
    )
  )[[1]]
  jk20 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    list(
      get_dplyr_call(merged_selectors20, 1L, jk20),
      get_dplyr_call(merged_selectors20, 2L, jk20),
      get_dplyr_call(merged_selectors20, 3L, jk20)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("BASELINE", "SCREENING") & SEX %in% c("F", "M")) %>%
          dplyr::select(STUDYID, USUBJID, AVISIT, PARAMCD, AGE) %>%
          dplyr::rename(x1.PARAMCD = PARAMCD, x1.AGE = AGE)
      ),
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT == "SCREENING") %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT) %>%
          dplyr::rename(x2.PARAMCD = PARAMCD, x2.AVISIT = AVISIT)
      ),
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("SCREENING", "FOLLOW UP")) %>%
          dplyr::select(STUDYID, USUBJID, AVISIT, PARAMCD, AVAL, AGE) %>%
          dplyr::rename(x3.PARAMCD = PARAMCD, x3.AGE = AGE)
      )
    )
  ) # AVISIT should be included in select

  testthat::expect_identical(
    get_merge_call(merged_selectors20, jk20),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))),
      quote(ANL <- dplyr::full_join(ANL, ANL_3, by = c("STUDYID", "USUBJID", "AVISIT")))
    )
  )

  # __21. multiple variables (concatenated) - drop_keys = TRUE ----
  merged_selectors21 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(
            columns = c("PARAMCD", "AVISIT"),
            selected = list(c("BESRSPI", "SCREENING")),
            multiple = TRUE,
            drop_keys = TRUE
          ),
          list(
            columns = "SEX",
            selected = "F",
            multiple = TRUE,
            drop_keys = TRUE
          )
        ),
        select = c("AGE", "SEX"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      )
    )
  )[[1]]
  jk21 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    get_dplyr_call(merged_selectors21, join_keys = jk21),
    quote(
      ADRS %>%
        dplyr::filter((PARAMCD == "BESRSPI" & AVISIT == "SCREENING") & SEX == "F") %>%
        dplyr::select(STUDYID, USUBJID, AGE, SEX) # attribute of the data_extract
    )
  ) # AVISIT should be in select - filter is not complete (two levels selected)

  testthat::expect_identical(
    get_merge_call(merged_selectors21, jk21),
    list(quote(ANL <- ANL_1))
  )

  # __22. multiple selectors different filters - keys in select ------
  merged_selectors22 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(
            columns = c("PARAMCD", "AVISIT"),
            selected = list(c("BESRSPI", "SCREENING")),
            multiple = TRUE,
            drop_keys = TRUE
          ),
          list(
            columns = "SEX",
            selected = "F",
            multiple = TRUE,
            drop_keys = TRUE
          )
        ),
        select = c("AGE", "PARAMCD"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(
            columns = c("PARAMCD", "AVISIT"),
            selected = list(c("BESRSPI", "BASELINE")),
            multiple = TRUE,
            drop_keys = TRUE
          ),
          list(
            columns = "SEX",
            selected = "M",
            multiple = TRUE,
            drop_keys = TRUE
          )
        ),
        select = c("AGE", "AVISIT"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x2"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(
            columns = c("PARAMCD", "AVISIT"),
            selected = list(c("BESRSPI", "SCREENING")),
            multiple = TRUE,
            drop_keys = TRUE
          ),
          list(
            columns = "SEX",
            selected = "F",
            multiple = TRUE,
            drop_keys = TRUE
          )
        ),
        select = c("COUNTRY", "AVISIT"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x3"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(
            columns = c("PARAMCD", "AVISIT"),
            selected = list(c("INVET", "BASELINE")),
            multiple = TRUE,
            drop_keys = TRUE
          ),
          list(
            columns = "SEX",
            selected = "F",
            multiple = TRUE,
            drop_keys = TRUE
          )
        ),
        select = c("STUDYID", "AVAL", "AGE", "PARAMCD", "AVISIT"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x4"
      )
    )
  )[[1]]
  jk22 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    list(
      deparse1(get_dplyr_call(merged_selectors22, 1L, jk22)),
      deparse1(get_dplyr_call(merged_selectors22, 2L, jk22)),
      deparse1(get_dplyr_call(merged_selectors22, 3L, jk22))
    ),
    list(
      # x1 and x3 are from the identical dataset - select(s) are combined
      # so output are just three ANLs instead of four
      deparse1(quote(
        ADRS %>%
          dplyr::filter((PARAMCD == "BESRSPI" & AVISIT == "SCREENING") & SEX == "F") %>%
          dplyr::select(STUDYID, USUBJID, AGE, PARAMCD, COUNTRY, AVISIT) %>%
          dplyr::rename(x1.AGE = AGE, x1.PARAMCD = PARAMCD, x1.AVISIT = AVISIT)
      )),
      deparse1(quote(
        ADRS %>%
          dplyr::filter((PARAMCD == "BESRSPI" & AVISIT == "BASELINE") & SEX == "M") %>%
          dplyr::select(STUDYID, USUBJID, AGE, AVISIT) %>%
          dplyr::rename(x2.AGE = AGE, x2.AVISIT = AVISIT)
      )),
      deparse1(quote(
        ADRS %>%
          dplyr::filter((PARAMCD == "INVET" & AVISIT == "BASELINE") & SEX == "F") %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL, AGE) %>%
          dplyr::rename(x4.AGE = AGE) %>%
          tidyr::pivot_longer(cols = c("AVAL", "x4.AGE"), names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      ))
    )
  ) # AVISIT should be included in select

  testthat::expect_identical(
    get_merge_call(merged_selectors22, jk22),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))),
      quote(ANL <- dplyr::full_join(ANL, ANL_3, by = c("STUDYID", "USUBJID")))
    )
  )
})

### Multiple long datasets -----
testthat::test_that("Multiple long datasets", {
  # __1. two selectors - empty filters ------
  merged_selectors1 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = NULL,
        select = "AVAL",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADTTE",
        filters = NULL,
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk1 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADTTE", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    list(
      get_dplyr_call(merged_selectors1, 1L, jk1),
      get_dplyr_call(merged_selectors1, 2L, jk1)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::select(STUDYID, USUBJID, AVAL) %>%
          dplyr::rename(x1.AVAL = AVAL)
      ),
      quote(
        ADTTE %>%
          dplyr::select(STUDYID, USUBJID, AVAL, AGE) %>%
          dplyr::rename(x2.AVAL = AVAL)
      )
    )
  )

  testthat::expect_identical(
    get_merge_call(merged_selectors1, jk1),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  ) # gonna be cartesian


  # __2. two selectors - different keys order ------
  merged_selectors2 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = NULL,
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADTTE",
        filters = NULL,
        select = c("AVAL", "AGE"),
        keys = c("USUBJID", "STUDYID", "PARAMCD"),
        reshape = FALSE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk2 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADTTE", stats::setNames(c("STUDYID", "USUBJID"), c("USUBJID", "STUDYID")))
  )

  testthat::expect_identical(
    list(
      get_dplyr_call(merged_selectors2, 1L, jk2),
      get_dplyr_call(merged_selectors2, 2L, jk2)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::select(USUBJID, STUDYID, AVAL) %>%
          dplyr::rename(x1.AVAL = AVAL)
      ),
      quote(
        ADTTE %>%
          dplyr::select(STUDYID, USUBJID, AVAL, AGE) %>%
          dplyr::rename(x2.AVAL = AVAL)
      )
    )
  )

  testthat::expect_equal(
    get_merge_call(merged_selectors2, jk2),
    list(
      quote(ANL <- ANL_1),
      quote(
        ANL <- dplyr::full_join(ANL, ANL_2, by = c(USUBJID = "STUDYID", STUDYID = "USUBJID")) %>%
          dplyr::mutate(USUBJID = STUDYID, STUDYID = USUBJID)
      )
    )
  ) # gonna be cartesian

  # __3. two selectors - one filteres ------
  merged_selectors3 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = "SCREENING", multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        select = "AVAL",
        internal_id = "x1"
      ),
      list(
        dataname = "ADTTE",
        filters = NULL,
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD"),
        reshape = FALSE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk3 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADTTE", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    list(
      get_dplyr_call(merged_selectors3, 1L, jk3),
      get_dplyr_call(merged_selectors3, 2L, jk3)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT == "SCREENING") %>%
          dplyr::select(STUDYID, USUBJID, AVAL) %>%
          dplyr::rename(x1.AVAL = AVAL)
      ),
      quote(
        ADTTE %>%
          dplyr::select(STUDYID, USUBJID, AVAL, AGE) %>%
          dplyr::rename(x2.AVAL = AVAL)
      )
    )
  )

  testthat::expect_equal(
    get_merge_call(merged_selectors3, jk3),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  ) # gonna be cartesian

  # __4. two selectors - two filtered ------
  merged_selectors4 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = "SCREENING", multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADTTE",
        filters = list(list(columns = "PARAMCD", selected = c("EFS", "PFS"), multiple = TRUE)),
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD"),
        reshape = FALSE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk4 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADTTE", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    list(
      get_dplyr_call(merged_selectors4, 1L, jk4),
      deparse(get_dplyr_call(merged_selectors4, 2L, jk4), 120)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT == "SCREENING") %>%
          dplyr::select(STUDYID, USUBJID, AVAL) %>%
          dplyr::rename(x1.AVAL = AVAL)
      ),
      deparse(quote(
        ADTTE %>%
          dplyr::filter(PARAMCD %in% c("EFS", "PFS")) %>%
          dplyr::select(STUDYID, USUBJID, AVAL, AGE) %>%
          dplyr::rename(x2.AVAL = AVAL)
      ), 120)
    )
  )

  testthat::expect_equal(
    get_merge_call(merged_selectors4, jk4),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  ) # gonna be cartesian


  # __5. two selectors - two filtered + empty selecton ------
  merged_selectors5 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = "SCREENING", multiple = TRUE)
        ),
        select = character(0),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADTTE",
        filters = list(list(columns = "PARAMCD", selected = c("EFS", "PFS"), multiple = TRUE)),
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD"),
        reshape = FALSE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk5 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADTTE", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    list(
      deparse(get_dplyr_call(merged_selectors5, 1L, jk5), 120),
      deparse(get_dplyr_call(merged_selectors5, 2L, jk5), 120)
    ),
    list(
      deparse(quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT == "SCREENING") %>%
          dplyr::select(STUDYID, USUBJID)
      ), 120),
      deparse(quote(
        ADTTE %>%
          dplyr::filter(PARAMCD %in% c("EFS", "PFS")) %>%
          dplyr::select(STUDYID, USUBJID, AVAL, AGE)
      ), 120)
    )
  )

  testthat::expect_equal(
    get_merge_call(merged_selectors5, jk5),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  ) # gonna be cartesian


  # __6. two selectors - one reshaped ------
  merged_selectors6 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = "SCREENING", multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADTTE",
        filters = list(list(columns = "PARAMCD", selected = c("EFS", "PFS"), multiple = TRUE)),
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD"),
        reshape = TRUE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk6 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADTTE", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    list(
      get_dplyr_call(merged_selectors6, 1L, jk6),
      deparse(get_dplyr_call(merged_selectors6, 2L, jk6), width.cutoff = 120)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT == "SCREENING") %>%
          dplyr::select(STUDYID, USUBJID, AVAL) %>%
          dplyr::rename(x1.AVAL = AVAL)
      ),
      deparse(quote(
        ADTTE %>%
          dplyr::filter(PARAMCD %in% c("EFS", "PFS")) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVAL, AGE) %>%
          dplyr::rename(x2.AVAL = AVAL) %>%
          tidyr::pivot_longer(cols = c("x2.AVAL", "AGE"), names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      ), width.cutoff = 120)
    )
  )

  testthat::expect_identical(
    list(get_reshape_unite_vals(merged_selectors6[[1]]), get_reshape_unite_vals(merged_selectors6[[2]])),
    list("BESRSPI_SCREENING", c("EFS", "PFS"))
  )

  testthat::expect_equal(
    get_merge_call(merged_selectors6, jk6),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  )

  # __7. two selectors - both reshaped ------
  merged_selectors7 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = "SCREENING", multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADTTE",
        filters = list(list(columns = "PARAMCD", selected = c("EFS", "PFS"), multiple = TRUE)),
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD"),
        reshape = TRUE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk7 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADTTE", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    list(
      get_dplyr_call(merged_selectors7, 1L, jk7),
      deparse(get_dplyr_call(merged_selectors7, 2L, jk7), width.cutoff = 120)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT == "SCREENING") %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL) %>%
          dplyr::rename(x1.AVAL = AVAL) %>%
          tidyr::pivot_longer(cols = "x1.AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      ),
      deparse(quote(
        ADTTE %>%
          dplyr::filter(PARAMCD %in% c("EFS", "PFS")) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVAL, AGE) %>%
          dplyr::rename(x2.AVAL = AVAL) %>%
          tidyr::pivot_longer(cols = c("x2.AVAL", "AGE"), names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      ), width.cutoff = 120)
    )
  )

  testthat::expect_identical(
    list(get_reshape_unite_vals(merged_selectors7[[1]]), get_reshape_unite_vals(merged_selectors7[[2]])),
    list("BESRSPI_SCREENING", c("EFS", "PFS"))
  )

  testthat::expect_equal(
    get_merge_call(merged_selectors7, jk7),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  )

  # __8. ADSL + two selectors - both reshaped ------
  merged_selectors8 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = "SCREENING", multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADTTE",
        filters = list(list(columns = "PARAMCD", selected = c("EFS", "PFS"), multiple = TRUE)),
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD"),
        reshape = TRUE,
        internal_id = "x2"
      ),
      list(
        dataname = "ADSL",
        filters = NULL,
        select = "AGE",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x3"
      )
    )
  )[[1]]
  jk8 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADTTE", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADRS", "ADSL", c("STUDYID", "USUBJID"))
  )

  testthat::expect_identical(
    list(
      deparse(get_dplyr_call(merged_selectors8, 1L, jk8), width.cutoff = 120),
      deparse(get_dplyr_call(merged_selectors8, 2L, jk8), width.cutoff = 120),
      deparse(get_dplyr_call(merged_selectors8, 3L, jk8), width.cutoff = 120)
    ),
    list(
      deparse(
        quote(
          ADRS %>%
            dplyr::filter(PARAMCD == "BESRSPI" & AVISIT == "SCREENING") %>%
            dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL) %>%
            dplyr::rename(x1.AVAL = AVAL) %>%
            tidyr::pivot_longer(cols = "x1.AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
            tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT) %>%
            tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
        ),
        width.cutoff = 120
      ),
      deparse(
        quote(
          ADTTE %>%
            dplyr::filter(PARAMCD %in% c("EFS", "PFS")) %>%
            dplyr::select(STUDYID, USUBJID, PARAMCD, AVAL, AGE) %>%
            dplyr::rename(x2.AVAL = AVAL, x2.AGE = AGE) %>%
            tidyr::pivot_longer(cols = c("x2.AVAL", "x2.AGE"), names_to = "MEASURE", values_to = "VALUE") %>%
            tidyr::unite(KEY, MEASURE, PARAMCD) %>%
            tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
        ),
        width.cutoff = 120
      ),
      deparse(
        quote(
          ADSL %>%
            dplyr::select(STUDYID, USUBJID, AGE) %>%
            dplyr::rename(x3.AGE = AGE)
        ),
        width.cutoff = 120
      )
    )
  )

  testthat::expect_identical(
    list(
      get_reshape_unite_vals(merged_selectors8[[1]]),
      get_reshape_unite_vals(merged_selectors8[[2]]),
      get_reshape_unite_vals(merged_selectors8[[3]])
    ),
    list(
      "BESRSPI_SCREENING",
      c("EFS", "PFS"),
      character(0)
    )
  )

  testthat::expect_equal(
    get_merge_call(merged_selectors8, jk8),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))),
      quote(ANL <- dplyr::full_join(ANL, ANL_3, by = c("STUDYID", "USUBJID")))
    )
  )


  # __9. ADRS + ADTTE + ADSL + ADRS ------
  merged_selectors9 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE)),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADTTE",
        filters = list(list(columns = "PARAMCD", selected = c("EFS", "PFS"), multiple = TRUE)),
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD"),
        reshape = TRUE,
        internal_id = "x2"
      ),
      list(
        dataname = "ADSL",
        filters = NULL,
        select = "AGE",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x3"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = c("SCREENING", "BASELINE"), multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x4"
      )
    )
  )[[1]]
  jk9 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADTTE", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADRS", "ADSL", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    list(
      deparse(get_dplyr_call(merged_selectors9, 1L, jk9), 120),
      deparse(get_dplyr_call(merged_selectors9, 2L, jk9), 120),
      deparse(get_dplyr_call(merged_selectors9, 3L, jk9), 120),
      deparse(get_dplyr_call(merged_selectors9, 4L, jk9), 120)
    ),
    list(
      deparse(quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI") %>%
          dplyr::select(STUDYID, USUBJID, AVISIT, AVAL) %>%
          dplyr::rename(x1.AVAL = AVAL)
      ), 120),
      deparse(quote(
        ADTTE %>%
          dplyr::filter(PARAMCD %in% c("EFS", "PFS")) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVAL, AGE) %>%
          dplyr::rename(x2.AVAL = AVAL, x2.AGE = AGE) %>%
          tidyr::pivot_longer(cols = c("x2.AVAL", "x2.AGE"), names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      ), 120),
      deparse(quote(
        ADSL %>%
          dplyr::select(STUDYID, USUBJID, AGE) %>%
          dplyr::rename(x3.AGE = AGE)
      ), 120),
      deparse(quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT %in% c("SCREENING", "BASELINE")) %>%
          dplyr::select(STUDYID, USUBJID, AVISIT, AVAL) %>%
          dplyr::rename(x4.AVAL = AVAL)
      ), 120)
    )
  )

  testthat::expect_identical(
    list(
      get_reshape_unite_vals(merged_selectors9[[1]]),
      get_reshape_unite_vals(merged_selectors9[[2]]),
      get_reshape_unite_vals(merged_selectors9[[3]]),
      get_reshape_unite_vals(merged_selectors9[[4]])
    ),
    list(
      "BESRSPI",
      c("EFS", "PFS"),
      character(0),
      c("BESRSPI_SCREENING", "BESRSPI_BASELINE")
    )
  )

  testthat::expect_equal(
    get_merge_call(merged_selectors9, jk9),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))),
      quote(ANL <- dplyr::full_join(ANL, ANL_3, by = c("STUDYID", "USUBJID"))),
      quote(ANL <- dplyr::full_join(ANL, ANL_4, by = c("STUDYID", "USUBJID", "AVISIT")))
    )
  )
  # __10. ADRS + ADTTE + ADSL + ADRS (same as first) ------
  merged_selectors10 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE)),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADTTE",
        filters = list(list(columns = "PARAMCD", selected = c("EFS", "PFS"), multiple = TRUE)),
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD"),
        reshape = TRUE,
        internal_id = "x2"
      ),
      list(
        dataname = "ADSL",
        filters = NULL,
        select = "AGE",
        keys = c("STUDYID", "USUBJID"),
        reshape = FALSE,
        internal_id = "x3"
      ),
      list(
        dataname = "ADRS",
        filters = list(list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE)),
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x4"
      )
    )
  )[[1]]
  jk10 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADTTE", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADRS", "ADSL", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )


  testthat::expect_equal(
    list(
      get_dplyr_call(merged_selectors10, 1L, jk10),
      deparse(get_dplyr_call(merged_selectors10, 2L, jk10), 120),
      get_dplyr_call(merged_selectors10, 3L, jk10)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI") %>%
          dplyr::select(STUDYID, USUBJID, AVAL, AGE) %>%
          dplyr::rename(x1.AVAL = AVAL, x1.AGE = AGE)
      ),
      deparse(quote(
        ADTTE %>%
          dplyr::filter(PARAMCD %in% c("EFS", "PFS")) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVAL, AGE) %>%
          dplyr::rename(x2.AVAL = AVAL, x2.AGE = AGE) %>%
          tidyr::pivot_longer(cols = c("x2.AVAL", "x2.AGE"), names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      ), 120),
      quote(
        ADSL %>%
          dplyr::select(STUDYID, USUBJID, AGE) %>%
          dplyr::rename(x3.AGE = AGE)
      )
    )
  )

  testthat::expect_equal(
    get_merge_call(merged_selectors10, jk10),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))),
      quote(ANL <- dplyr::full_join(ANL, ANL_3, by = c("STUDYID", "USUBJID")))
    )
  )

  ### __11. Mixed ----
  merged_selectors11 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = "PARAMCD", selected = "BESRSPI", multiple = TRUE),
          list(columns = "AVISIT", selected = "SCREENING", multiple = TRUE),
          list(columns = "SEX", selected = "F", multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADTTE",
        filters = list(
          list(columns = "RACE", selected = "ASIAN", multiple = TRUE),
          list(columns = "PARAMCD", selected = c("EFS", "PFS"), multiple = TRUE)
        ),
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD"),
        reshape = FALSE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk11 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADTTE", c("STUDYID", "USUBJID"))
  )


  testthat::expect_equal(
    list(
      get_dplyr_call(merged_selectors11, 1L, jk11),
      deparse(get_dplyr_call(merged_selectors11, 2L, jk11), 120)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter(PARAMCD == "BESRSPI" & AVISIT == "SCREENING" & SEX == "F") %>%
          dplyr::select(STUDYID, USUBJID, AVAL) %>%
          dplyr::rename(x1.AVAL = AVAL)
      ),
      deparse(quote(
        ADTTE %>%
          dplyr::filter(RACE == "ASIAN" & PARAMCD %in% c("EFS", "PFS")) %>%
          dplyr::select(STUDYID, USUBJID, AVAL, AGE) %>%
          dplyr::rename(x2.AVAL = AVAL)
      ), 120)
    )
  )

  testthat::expect_identical(
    list(
      get_reshape_unite_vals(merged_selectors11[[1]]),
      get_reshape_unite_vals(merged_selectors11[[2]])
    ),
    list("BESRSPI_SCREENING", c("EFS", "PFS"))
  )

  testthat::expect_equal(
    get_merge_call(merged_selectors11, jk11),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  )
})

# Multiple long - combined/concatenated filters -------
testthat::test_that("Multiple long - combined/concatenated filters", {
  ### __12.a Combined/concatenated filters ----
  merged_selectors12 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(
            columns = c("PARAMCD", "AVISIT"),
            selected = list(c("BESRSPI", "SCREENING")),
            multiple = TRUE,
            drop_keys = TRUE
          ),
          list(
            columns = "SEX",
            selected = "F",
            multiple = TRUE,
            drop_keys = TRUE
          )
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADTTE",
        filters = list(
          list(columns = "RACE", selected = "ASIAN", multiple = TRUE, drop_keys = TRUE),
          list(columns = "PARAMCD", selected = c("EFS", "PFS"), multiple = TRUE, drop_keys = TRUE)
        ),
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD"),
        reshape = FALSE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk12 <- teal.data::join_keys(
    teal.data::join_key("ADRS", keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")),
    teal.data::join_key("ADTTE", "ADRS", c("STUDYID", "USUBJID", "PARAMCD"))
  )

  # PARAMCD is included if drop_keys = TRUE - because merge is on different datasets
  testthat::expect_equal(
    list(
      get_dplyr_call(merged_selectors12, 1L, jk12),
      deparse(get_dplyr_call(merged_selectors12, 2L, jk12), 120)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter((PARAMCD == "BESRSPI" & AVISIT == "SCREENING") & SEX == "F") %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVAL) %>%
          dplyr::rename(x1.AVAL = AVAL)
      ),
      deparse(quote(
        ADTTE %>%
          dplyr::filter(RACE == "ASIAN" & PARAMCD %in% c("EFS", "PFS")) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVAL, AGE) %>%
          dplyr::rename(x2.AVAL = AVAL)
      ), 120)
    )
  )

  testthat::expect_identical(
    list(
      get_reshape_unite_vals(merged_selectors12[[1]]),
      get_reshape_unite_vals(merged_selectors12[[2]])
    ),
    list("BESRSPI_SCREENING", c("EFS", "PFS"))
  )

  testthat::expect_equal(
    get_merge_call(merged_selectors12, jk12),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID", "PARAMCD")))
    )
  )

  ### __13. Reshape on combined/concatenated filters ----
  merged_selectors13 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = c("PARAMCD", "AVISIT"), selected = list(c("BESRSPI", "SCREENING")), multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADTTE",
        filters = list(list(columns = "PARAMCD", selected = c("EFS", "PFS"), multiple = TRUE)),
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD"),
        reshape = TRUE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk13 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADTTE", c("STUDYID", "USUBJID"))
  )


  testthat::expect_identical(
    list(
      deparse(get_dplyr_call(merged_selectors13, 1L, jk13), width.cutoff = 120),
      deparse(get_dplyr_call(merged_selectors13, 2L, jk13), width.cutoff = 120)
    ),
    list(
      deparse(quote(
        ADRS %>%
          dplyr::filter((PARAMCD == "BESRSPI" & AVISIT == "SCREENING")) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL) %>%
          dplyr::rename(x1.AVAL = AVAL) %>%
          tidyr::pivot_longer(cols = "x1.AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD, AVISIT) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      ), width.cutoff = 120),
      deparse(quote(
        ADTTE %>%
          dplyr::filter(PARAMCD %in% c("EFS", "PFS")) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVAL, AGE) %>%
          dplyr::rename(x2.AVAL = AVAL) %>%
          tidyr::pivot_longer(cols = c("x2.AVAL", "AGE"), names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      ), width.cutoff = 120)
    )
  )

  testthat::expect_identical(
    list(
      get_reshape_unite_vals(merged_selectors13[[1]]),
      get_reshape_unite_vals(merged_selectors13[[2]])
    ),
    list(
      "BESRSPI_SCREENING",
      c("EFS", "PFS")
    )
  )

  testthat::expect_equal(
    get_merge_call(merged_selectors13, jk13),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  )

  ### __14. Combined/concatenated filters ----
  merged_selectors14 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = c("PARAMCD", "AVISIT"), selected = list(c("BESRSPI", "SCREENING")), multiple = TRUE),
          list(columns = "SEX", selected = "F", multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADTTE",
        filters = list(
          list(columns = "RACE", selected = "ASIAN", multiple = TRUE),
          list(columns = "PARAMCD", selected = c("EFS", "PFS"), multiple = TRUE)
        ),
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD"),
        reshape = FALSE,
        internal_id = "x2"
      ),
      list(
        dataname = "ADRS",
        filters = list(
          list(columns = c("PARAMCD", "AVISIT"), selected = list(c("OVRINV", "FOLLOW UP")), multiple = TRUE)
        ),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x3"
      )
    )
  )[[1]]
  jk14 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADTTE", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    list(
      get_dplyr_call(merged_selectors14, 1L, jk14),
      deparse(get_dplyr_call(merged_selectors14, 2L, jk14), 120),
      get_dplyr_call(merged_selectors14, 3L, jk14)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter((PARAMCD == "BESRSPI" & AVISIT == "SCREENING") & SEX == "F") %>%
          dplyr::select(STUDYID, USUBJID, AVAL) %>%
          dplyr::rename(x1.AVAL = AVAL)
      ),
      deparse(quote(
        ADTTE %>%
          dplyr::filter(RACE == "ASIAN" & PARAMCD %in% c("EFS", "PFS")) %>%
          dplyr::select(STUDYID, USUBJID, AVAL, AGE) %>%
          dplyr::rename(x2.AVAL = AVAL)
      ), 120),
      quote(
        ADRS %>%
          dplyr::filter((PARAMCD == "OVRINV" & AVISIT == "FOLLOW UP")) %>%
          dplyr::select(STUDYID, USUBJID, AVAL) %>%
          dplyr::rename(x3.AVAL = AVAL)
      )
    )
  )

  testthat::expect_equal(
    get_merge_call(merged_selectors14, jk14),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))),
      quote(ANL <- dplyr::full_join(ANL, ANL_3, by = c("STUDYID", "USUBJID")))
    )
  )

  ### __15. Key concatented with non-key filter ----
  merged_selectors15 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(list(
          columns = c("PARAMCD", "AVISIT", "SEX"),
          selected = list(c("BESRSPI", "END OF INDUCTION", "F"), c("BESRSPI", "END OF INDUCTION", "M")),
          multiple = TRUE
        )),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADTTE",
        filters = list(
          list(columns = "RACE", selected = "ASIAN", multiple = TRUE),
          list(columns = "PARAMCD", selected = c("EFS", "PFS"), multiple = TRUE)
        ),
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD"),
        reshape = FALSE,
        internal_id = "x2"
      ),
      list(
        dataname = "ADRS",
        filters = list(list(
          columns = c("PARAMCD", "AVISIT"),
          selected = list(c("OVRINV", "FOLLOW UP")),
          multiple = TRUE
        )),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = FALSE,
        internal_id = "x3"
      )
    )
  )[[1]]
  jk15 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADTTE", c("STUDYID", "USUBJID")),
    teal.data::join_key("ADRS", "ADRS", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
  )

  testthat::expect_equal(
    list(
      get_dplyr_call(merged_selectors15, 1L, jk15),
      deparse(get_dplyr_call(merged_selectors15, 2L, jk15), 120),
      get_dplyr_call(merged_selectors15, 3L, jk15)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter(
            (PARAMCD == "BESRSPI" & AVISIT == "END OF INDUCTION" & SEX == "F") |
              (PARAMCD == "BESRSPI" & AVISIT == "END OF INDUCTION" & SEX == "M")
          ) %>%
          dplyr::select(STUDYID, USUBJID, AVAL) %>%
          dplyr::rename(x1.AVAL = AVAL)
      ),
      deparse(quote(
        ADTTE %>%
          dplyr::filter(RACE == "ASIAN" & PARAMCD %in% c("EFS", "PFS")) %>%
          dplyr::select(STUDYID, USUBJID, AVAL, AGE) %>%
          dplyr::rename(x2.AVAL = AVAL)
      ), 120),
      quote(
        ADRS %>%
          dplyr::filter((PARAMCD == "OVRINV" & AVISIT == "FOLLOW UP")) %>%
          dplyr::select(STUDYID, USUBJID, AVAL) %>%
          dplyr::rename(x3.AVAL = AVAL)
      )
    )
  )

  testthat::expect_equal(
    get_merge_call(merged_selectors15, jk15),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))),
      quote(ANL <- dplyr::full_join(ANL, ANL_3, by = c("STUDYID", "USUBJID")))
    )
  )

  ### __16. Reshape on concatented with non-key filter ----
  merged_selectors16 <- merge_selectors(
    list(
      list(
        dataname = "ADRS",
        filters = list(list(
          columns = c("PARAMCD", "SEX"),
          selected = list(c("BESRSPI", "F"), c("BESRSPI", "M")),
          multiple = TRUE
        )),
        select = "AVAL",
        keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
        reshape = TRUE,
        internal_id = "x1"
      ),
      list(
        dataname = "ADTTE",
        filters = list(
          list(columns = "RACE", selected = "ASIAN", multiple = TRUE),
          list(columns = "PARAMCD", selected = c("EFS", "PFS"), multiple = TRUE)
        ),
        select = c("AVAL", "AGE"),
        keys = c("STUDYID", "USUBJID", "PARAMCD"),
        reshape = FALSE,
        internal_id = "x2"
      )
    )
  )[[1]]
  jk16 <- teal.data::join_keys(
    teal.data::join_key("ADRS", "ADTTE", c("STUDYID", "USUBJID"))
  )

  testthat::expect_equal(
    list(
      get_dplyr_call(merged_selectors16, 1L, jk16),
      deparse(get_dplyr_call(merged_selectors16, 2L, jk16), 120)
    ),
    list(
      quote(
        ADRS %>%
          dplyr::filter((PARAMCD == "BESRSPI" & SEX == "F") | (PARAMCD == "BESRSPI" & SEX == "M")) %>%
          dplyr::select(STUDYID, USUBJID, PARAMCD, AVAL) %>%
          dplyr::rename(x1.AVAL = AVAL) %>%
          tidyr::pivot_longer(cols = "x1.AVAL", names_to = "MEASURE", values_to = "VALUE") %>%
          tidyr::unite(KEY, MEASURE, PARAMCD) %>%
          tidyr::pivot_wider(names_from = "KEY", values_from = "VALUE")
      ),
      deparse(quote(
        ADTTE %>%
          dplyr::filter(RACE == "ASIAN" & PARAMCD %in% c("EFS", "PFS")) %>%
          dplyr::select(STUDYID, USUBJID, AVAL, AGE) %>%
          dplyr::rename(x2.AVAL = AVAL)
      ), 120)
    )
  )

  testthat::expect_equal(
    get_merge_call(merged_selectors16, jk16),
    list(
      quote(ANL <- ANL_1),
      quote(ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID")))
    )
  )
})

# Universal example ------
testthat::test_that("Universal example", {
  X <- data.frame(A = c(1, 1:3), B = 2:5, D = 1:4, E = letters[1:4], G = letters[6:9]) # nolint object_name_linter.
  Y <- data.frame(A = c(1, 1, 2), B = 2:4, C = c(4, 4:5), E = letters[4:6], F = letters[5:7], G = letters[1:3]) # nolint object_name_linter.
  Z <- data.frame(C = c(4, 4:6), D = 1:4, E = letters[4:7], F = letters[6:9], G = letters[1:4]) # nolint object_name_linter.

  selector_list <- list(
    list(
      dataname = "X",
      filters = NULL,
      select = "E",
      keys = c("A", "B"),
      reshape = FALSE,
      internal_id = "x"
    ),
    list(
      dataname = "Y",
      filters = NULL,
      select = "G",
      keys = c("A", "C"),
      reshape = FALSE,
      internal_id = "y"
    ),
    list(
      dataname = "Z",
      filters = NULL,
      select = c("F", "G"),
      keys = c("C", "D"),
      reshape = FALSE,
      internal_id = "z"
    )
  )

  data_list <- list(X = reactive(X), Y = reactive(Y), Z = reactive(Z))
  join_keys <- teal.data::join_keys(
    teal.data::join_key("X", "Y", c("A", "B")),
    teal.data::join_key("X", "Z", c("D")),
    teal.data::join_key("Y", "Z", c("C"))
  )

  merged_datasets <- isolate(
    merge_datasets(
      selector_list = selector_list,
      datasets = data_list,
      join_keys = join_keys,
      merge_function = "dplyr::left_join",
      anl_name = "ANL"
    )
  )

  testthat::expect_identical(
    paste(merged_datasets$expr),
    paste(
      c(
        "ANL_1 <- X %>% dplyr::select(A, B, D, E)",
        "ANL_2 <- Y %>% dplyr::select(A, B, C, G) %>% dplyr::rename(y.G = G)",
        "ANL_3 <- Z %>% dplyr::select(D, C, F, G) %>% dplyr::rename(z.G = G)",
        "ANL <- ANL_1",
        "ANL <- dplyr::left_join(ANL, ANL_2, by = c(\"A\", \"B\"))",
        "ANL <- dplyr::left_join(ANL, ANL_3, by = c(\"D\", \"C\"))"
      )
    )
  )
})
