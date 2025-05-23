ADSL <- teal.data::rADSL
ADTTE <- teal.data::rADTTE
data_list <- list(ADSL = reactive(ADSL), ADTTE = reactive(ADTTE))
key_list <- list(ADSL = c("STUDYID", "USUBJID"), ADTTE = c("STUDYID", "USUBJID", "PARAMCD"))

vc_hard <- variable_choices("ADSL", subset = c("STUDYID", "USUBJID"))
vc_hard_exp <- structure(
  list(data = "ADSL", subset = c("STUDYID", "USUBJID"), key = NULL),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

vc_hard_short <- variable_choices("ADSL", subset = "STUDYID")
vc_hard_short_exp <- structure(
  list(data = "ADSL", subset = "STUDYID", key = NULL),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

vc_fun <- variable_choices("ADSL", subset = function(data) colnames(data)[1:2])
vc_fun_exp <- structure(
  list(data = "ADSL", subset = function(data) colnames(data)[1:2], key = NULL),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

vc_fun_short <- variable_choices("ADSL", subset = function(data) colnames(data)[1])
vc_fun_short_exp <- structure(
  list(data = "ADSL", subset = function(data) colnames(data)[1], key = NULL),
  class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
)

testthat::test_that("data_extract_spec throws when select is not select_spec or NULL", {
  testthat::expect_error(data_extract_spec("toyDataset", select = c("A", "B")))
})

testthat::test_that("data_extract_spec works with valid input", {
  # the dataset does not exist, so we just check if the combinations are accepted
  # we add 1 to the var names to avoid confusion with their respective functions

  select_spec1 <- select_spec(
    label = "Select variable:",
    choices = c("SEX", "RACE"),
    selected = "SEX",
    multiple = FALSE,
    ordered = FALSE,
    fixed = FALSE
  )
  data_extract_spec1 <- testthat::expect_silent(data_extract_spec(
    "toyDataset",
    select = select_spec1
  ))
  testthat::expect_identical(data_extract_spec1$select, select_spec1)
  testthat::expect_identical(class(data_extract_spec1), "data_extract_spec")

  testthat::expect_identical(
    testthat::expect_silent(data_extract_spec(
      "toyDataset",
      select = select_spec1
    )),
    testthat::expect_silent(data_extract_spec(
      "toyDataset",
      select = select_spec1,
      filter = NULL
    ))
  )

  # with filter
  select_spec1 <- select_spec(
    label = "Select variable:",
    choices = c("AVAL", "CNSR"),
    selected = "AVAL",
    multiple = FALSE,
    ordered = FALSE,
    fixed = FALSE
  )
  filter_spec1 <- filter_spec(
    label = "Select parameter:",
    vars = "PARAMCD",
    choices = c("OS", "PFS"),
    selected = "PFS",
    multiple = FALSE
  )
  filter_spec1$dataname <- "ADTTE"

  filter_spec2 <- filter_spec(
    label = "Select parameter:",
    vars = "AVISIT",
    choices = c("BASELINE", "SCREENIG"),
    selected = "BASELINE",
    multiple = FALSE
  )
  filter_spec2$dataname <- "ADTTE"

  data_extract_spec1 <- testthat::expect_silent(data_extract_spec(
    dataname = "ADTTE",
    select = select_spec1,
    filter = filter_spec1
  ))
  testthat::expect_identical(data_extract_spec1$select, select_spec1)

  testthat::expect_identical(data_extract_spec1$filter, list(filter_spec1))

  data_extract_spec2 <- testthat::expect_silent(data_extract_spec(
    dataname = "ADTTE",
    select = select_spec1,
    filter = list(filter_spec1, filter_spec2)
  ))

  testthat::expect_identical(data_extract_spec2$select, select_spec1)
  testthat::expect_identical(data_extract_spec2$filter, list(filter_spec1, filter_spec2))

  # with reshape (only makes sense when filter is there)
  filter_spec1 <- filter_spec(
    label = "Select parameter:",
    vars = "PARAMCD",
    choices = c("OS", "PFS", "OS2"),
    selected = c("OS", "PFS"),
    multiple = TRUE
  )
  filter_spec1$dataname <- "ADTTE"
  data_extract_spec1 <- testthat::expect_silent(data_extract_spec(
    dataname = "ADTTE",
    select = select_spec1,
    filter = filter_spec1,
    reshape = TRUE
  ))
  testthat::expect_identical(data_extract_spec1$select, select_spec1)
  testthat::expect_identical(data_extract_spec1$filter, list(filter_spec1))
  testthat::expect_identical(data_extract_spec1$reshape, TRUE)
})

testthat::test_that("delayed data_extract_spec works", {
  set.seed(1)
  ADSL <- data.frame(
    USUBJID = letters[1:10],
    SEX = sample(c("F", "M", "U"), 10, replace = TRUE),
    BMRKR1 = rnorm(10),
    BMRKR2 = sample(c("L", "M", "H"), 10, replace = TRUE),
    stringsAsFactors = FALSE
  )
  attr(ADSL, "keys") <- c("STUDYID", "USUBJID")

  filter_normal <- filter_spec(
    vars = variable_choices(ADSL, "SEX"),
    sep = "-",
    choices = value_choices(ADSL, "SEX", "SEX"),
    selected = "F",
    multiple = FALSE
  )

  filter_delayed <- filter_spec(
    vars = variable_choices("ADSL", "SEX"),
    sep = "-",
    choices = value_choices("ADSL", "SEX", "SEX"),
    selected = "F",
    multiple = FALSE
  )

  select_normal <- select_spec(
    choices = variable_choices(ADSL, c("BMRKR1", "BMRKR2")),
    selected = "BMRKR1",
    multiple = FALSE,
    fixed = FALSE,
    ordered = FALSE
  )

  select_delayed <- select_spec(
    choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
    selected = "BMRKR1",
    multiple = FALSE,
    fixed = FALSE,
    ordered = FALSE
  )

  expected_spec <- data_extract_spec(
    dataname = "ADSL",
    select = select_normal,
    filter = filter_normal
  )

  # obtained via delayed approach
  delayed_spec <- data_extract_spec(
    dataname = "ADSL",
    select = select_delayed,
    filter = filter_delayed
  )

  mix1 <- data_extract_spec(
    dataname = "ADSL",
    select = select_delayed,
    filter = filter_normal
  )

  mix2 <- data_extract_spec(
    dataname = "ADSL",
    select = select_normal,
    filter = filter_delayed
  )

  mix3 <- data_extract_spec(
    dataname = "ADSL",
    select = select_delayed,
    filter = list(filter_delayed, filter_normal)
  )

  testthat::expect_equal(class(delayed_spec), c("delayed_data_extract_spec", "delayed_data", "data_extract_spec"))
  testthat::expect_equal(class(mix1), c("delayed_data_extract_spec", "delayed_data", "data_extract_spec"))
  testthat::expect_equal(class(mix2), c("delayed_data_extract_spec", "delayed_data", "data_extract_spec"))
  testthat::expect_equal(class(mix3), c("delayed_data_extract_spec", "delayed_data", "data_extract_spec"))

  testthat::expect_equal(names(expected_spec), names(delayed_spec))
  testthat::expect_equal(names(expected_spec), names(mix1))
  testthat::expect_equal(names(expected_spec), names(mix2))
  testthat::expect_equal(names(expected_spec), names(mix3))

  data_list <- list(ADSL = reactive(ADSL))
  key_list <- list(ADSL = c("STUDYID", "USUBJID"))

  isolate({
    testthat::expect_identical(expected_spec, resolve(delayed_spec, data_list, key_list))
    testthat::expect_identical(expected_spec, resolve(mix1, data_list, key_list))
    testthat::expect_identical(expected_spec, resolve(mix2, data_list, key_list))

    mix3_res <- resolve(mix3, data_list, key_list)
  })
  testthat::expect_identical(expected_spec$filter[[1]], mix3_res$filter[[1]])
  testthat::expect_identical(expected_spec$filter[[1]], mix3_res$filter[[2]])
  mix3_res$filter <- NULL
  expected_spec$filter <- NULL
  testthat::expect_identical(expected_spec, mix3_res)
})

testthat::test_that("delayed version of data_extract_spec", {
  # hard-coded subset
  obj <- data_extract_spec(
    "ADSL",
    select = select_spec(vc_hard, selected = vc_hard_short, multiple = FALSE),
    filter = filter_spec(
      vars = variable_choices("ADSL", subset = "ARMCD"),
      choices = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
      selected = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      multiple = FALSE
    )
  )

  res_obj <- isolate(resolve(obj, datasets = data_list, keys = key_list))
  exp_obj <- data_extract_spec(
    "ADSL",
    select = select_spec(variable_choices(ADSL, c("STUDYID", "USUBJID"), key = c("STUDYID", "USUBJID")),
      selected = variable_choices(ADSL, "STUDYID", key = c("STUDYID", "USUBJID"))
    ),
    filter = filter_spec(
      vars = variable_choices(ADSL, subset = "ARMCD", key = c("STUDYID", "USUBJID")),
      choices = value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
      selected = value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      multiple = FALSE
    )
  )

  testthat::expect_equal(res_obj$select, exp_obj$select)
  testthat::expect_equal(res_obj$filter[[1]]$choices, exp_obj$filter[[1]]$choices)
  testthat::expect_equal(res_obj$filter[[1]]$selected, exp_obj$filter[[1]]$selected)

  # functional subset
  obj <- data_extract_spec(
    "ADSL",
    select = select_spec(vc_fun, selected = vc_fun_short, multiple = FALSE),
    filter = filter_spec(
      vars = variable_choices("ADSL", subset = "ARMCD"),
      choices = value_choices(
        "ADSL",
        var_choices = "ARMCD",
        var_label = "ARM",
        subset = function(data) c("ARM A", "ARM B")
      ),
      selected = value_choices(
        "ADSL",
        var_choices = "ARMCD",
        var_label = "ARM",
        subset = function(data) "ARM A"
      ),
      multiple = FALSE
    )
  )

  res_obj <- isolate(resolve(obj, datasets = data_list, keys = key_list))
  exp_obj <- data_extract_spec(
    "ADSL",
    select = select_spec(variable_choices(ADSL, c("STUDYID", "USUBJID"), key = c("STUDYID", "USUBJID")),
      selected = variable_choices(ADSL, "STUDYID", key = c("STUDYID", "USUBJID"))
    ),
    filter = filter_spec(
      vars = variable_choices(ADSL, subset = "ARMCD", key = c("STUDYID", "USUBJID")),
      choices = value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
      selected = value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      multiple = FALSE
    )
  )

  testthat::expect_equal(res_obj$select, exp_obj$select)
  testthat::expect_equal(res_obj$filter[[1]]$choices, exp_obj$filter[[1]]$choices)
  testthat::expect_equal(res_obj$filter[[1]]$selected, exp_obj$filter[[1]]$selected)
})

testthat::test_that("data_extract_spec allows both select and filter parameters to be NULL", {
  testthat::expect_no_error(des <- data_extract_spec("ADSL"))
})

testthat::test_that("data_extract_spec returns filter_spec with multiple set to TRUE", {
  des <- data_extract_spec("ADSL")
  testthat::expect_equal(class(des$filter[[1]]), c("delayed_filter_spec", "filter_spec", "delayed_data"))
  testthat::expect_equal(length(des$filter), 1)
  testthat::expect_true(des$filter[[1]]$multiple)
})

testthat::test_that("data_extract_spec returns select_spec with multiple set to TRUE", {
  des <- data_extract_spec("ADSL")
  testthat::expect_identical(
    names(des$select),
    names(formals(select_spec))
  )
  testthat::expect_equal(class(des$select$choices), c("delayed_variable_choices", "delayed_data", "choices_labeled"))
  testthat::expect_true(des$select$multiple)
  testthat::expect_null(des$select$selected)
  testthat::expect_null(des$select$always_selected)
  testthat::expect_false(des$select$fixed)
  testthat::expect_equal(des$select$label, "Select")
})

# with resolve_delayed
testthat::test_that("delayed data_extract_spec works - resolve_delayed", {
  set.seed(1)
  ADSL <- data.frame(
    USUBJID = letters[1:10],
    SEX = sample(c("F", "M", "U"), 10, replace = TRUE),
    BMRKR1 = rnorm(10),
    BMRKR2 = sample(c("L", "M", "H"), 10, replace = TRUE),
    stringsAsFactors = FALSE
  )
  attr(ADSL, "keys") <- c("STUDYID", "USUBJID")

  filter_normal <- filter_spec(
    vars = variable_choices(ADSL, "SEX"),
    sep = "-",
    choices = value_choices(ADSL, "SEX", "SEX"),
    selected = "F",
    multiple = FALSE
  )

  filter_delayed <- filter_spec(
    vars = variable_choices("ADSL", "SEX"),
    sep = "-",
    choices = value_choices("ADSL", "SEX", "SEX"),
    selected = "F",
    multiple = FALSE
  )

  select_normal <- select_spec(
    choices = variable_choices(ADSL, c("BMRKR1", "BMRKR2")),
    selected = "BMRKR1",
    multiple = FALSE,
    fixed = FALSE,
    ordered = FALSE
  )

  select_delayed <- select_spec(
    choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
    selected = "BMRKR1",
    multiple = FALSE,
    fixed = FALSE,
    ordered = FALSE
  )

  expected_spec <- data_extract_spec(
    dataname = "ADSL",
    select = select_normal,
    filter = filter_normal
  )

  # obtained via delayed approach
  delayed_spec <- data_extract_spec(
    dataname = "ADSL",
    select = select_delayed,
    filter = filter_delayed
  )

  mix1 <- data_extract_spec(
    dataname = "ADSL",
    select = select_delayed,
    filter = filter_normal
  )

  mix2 <- data_extract_spec(
    dataname = "ADSL",
    select = select_normal,
    filter = filter_delayed
  )

  mix3 <- data_extract_spec(
    dataname = "ADSL",
    select = select_delayed,
    filter = list(filter_delayed, filter_normal)
  )

  testthat::expect_equal(class(delayed_spec), c("delayed_data_extract_spec", "delayed_data", "data_extract_spec"))
  testthat::expect_equal(class(mix1), c("delayed_data_extract_spec", "delayed_data", "data_extract_spec"))
  testthat::expect_equal(class(mix2), c("delayed_data_extract_spec", "delayed_data", "data_extract_spec"))
  testthat::expect_equal(class(mix3), c("delayed_data_extract_spec", "delayed_data", "data_extract_spec"))

  testthat::expect_equal(names(expected_spec), names(delayed_spec))
  testthat::expect_equal(names(expected_spec), names(mix1))
  testthat::expect_equal(names(expected_spec), names(mix2))
  testthat::expect_equal(names(expected_spec), names(mix3))


  isolate({
    data_list <- list(ADSL = reactive(ADSL))
    testthat::expect_identical(expected_spec, resolve_delayed(delayed_spec, data_list))
    testthat::expect_identical(expected_spec, resolve_delayed(mix1, data_list))
    testthat::expect_identical(expected_spec, resolve_delayed(mix2, data_list))

    mix3_res <- resolve_delayed(mix3, data_list)
  })

  testthat::expect_identical(expected_spec$filter[[1]], mix3_res$filter[[1]])
  testthat::expect_identical(expected_spec$filter[[1]], mix3_res$filter[[2]])
  mix3_res$filter <- NULL
  expected_spec$filter <- NULL
  testthat::expect_identical(expected_spec, mix3_res)
})

testthat::test_that("delayed version of data_extract_spec - resolve_delayed", {
  data_list <- list(ADSL = reactive(ADSL))
  keys_list <- list(ADSL = c("STUDYID", "USUBJID"))
  # hard-coded subset
  obj <- data_extract_spec(
    "ADSL",
    select = select_spec(vc_hard, selected = vc_hard_short, multiple = FALSE),
    filter = filter_spec(
      vars = variable_choices("ADSL", subset = "ARMCD"),
      choices = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
      selected = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      multiple = FALSE
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = data_list, keys = keys_list))
  exp_obj <- data_extract_spec(
    "ADSL",
    select = select_spec(variable_choices(ADSL, c("STUDYID", "USUBJID"), key = c("STUDYID", "USUBJID")),
      selected = variable_choices(ADSL, "STUDYID", key = c("STUDYID", "USUBJID"))
    ),
    filter = filter_spec(
      vars = variable_choices(ADSL, subset = "ARMCD", key = c("STUDYID", "USUBJID")),
      choices = value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
      selected = value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      multiple = FALSE
    )
  )

  testthat::expect_equal(res_obj$select, exp_obj$select)
  testthat::expect_equal(res_obj$filter[[1]]$choices, exp_obj$filter[[1]]$choices)
  testthat::expect_equal(res_obj$filter[[1]]$selected, exp_obj$filter[[1]]$selected)


  # functional subset
  obj <- data_extract_spec(
    "ADSL",
    select = select_spec(vc_fun, selected = vc_fun_short, multiple = FALSE),
    filter = filter_spec(
      vars = variable_choices("ADSL", subset = "ARMCD"),
      choices = value_choices(
        "ADSL",
        var_choices = "ARMCD",
        var_label = "ARM",
        subset = function(data) c("ARM A", "ARM B")
      ),
      selected = value_choices(
        "ADSL",
        var_choices = "ARMCD",
        var_label = "ARM",
        subset = function(data) "ARM A"
      ),
      multiple = FALSE
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = data_list, keys = keys_list))
  exp_obj <- data_extract_spec(
    "ADSL",
    select = select_spec(variable_choices(ADSL, c("STUDYID", "USUBJID"), key = c("STUDYID", "USUBJID")),
      selected = variable_choices(ADSL, "STUDYID", key = c("STUDYID", "USUBJID"))
    ),
    filter = filter_spec(
      vars = variable_choices(ADSL, subset = "ARMCD", key = c("STUDYID", "USUBJID")),
      choices = value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
      selected = value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      multiple = FALSE
    )
  )

  testthat::expect_equal(res_obj$select, exp_obj$select)
  testthat::expect_equal(res_obj$filter[[1]]$choices, exp_obj$filter[[1]]$choices)
  testthat::expect_equal(res_obj$filter[[1]]$selected, exp_obj$filter[[1]]$selected)
})
