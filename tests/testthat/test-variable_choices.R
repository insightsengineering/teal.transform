ADSL <- teal.transform::rADSL # nolint
ADTTE <- teal.transform::rADTTE # nolint

test_that("Can create variable_choices with datasets with no or missing labels", {
  example_data <- data.frame(USUBJID = 1:2, STUDYID = 1:1)

  # no labels given
  choice_1 <- variable_choices(example_data, fill = TRUE)
  expect_equal(names(choice_1), c("USUBJID: USUBJID", "STUDYID: STUDYID"))

  # one missing label
  missing_one_label_data <- example_data
  teal.data::formatters_var_labels(missing_one_label_data) <- c(as.character(NA), "Label")
  choice_2 <- variable_choices(missing_one_label_data, fill = FALSE)
  expect_equal(names(choice_2), c("USUBJID: Label Missing", "STUDYID: Label"))

  # all missing label
  missing_two_label_data <- example_data
  teal.data::formatters_var_labels(missing_two_label_data) <- c(as.character(NA), as.character(NA))
  choice_2 <- variable_choices(missing_two_label_data, fill = FALSE)
  expect_equal(names(choice_2), c("USUBJID: Label Missing", "STUDYID: Label Missing"))
})

test_that("delayed version of variable_choices", {
  # hard-coded subset
  obj <- variable_choices("ADSL", subset = c("SEX", "ARMCD", "COUNTRY"))
  expect_equal(
    obj,
    structure(
      list(data = "ADSL", subset = c("SEX", "ARMCD", "COUNTRY"), key = NULL),
      class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
    )
  )

  data_list <- list(ADSL = reactive(ADSL), ADTTE = reactive(ADTTE))
  key_list <- list(ADSL = teal.data::get_cdisc_keys("ADSL"), ADTTE = teal.data::get_cdisc_keys("ADTTE"))

  res_obj <- isolate(resolve(obj, datasets = data_list, keys = key_list))
  expect_equal(
    res_obj,
    variable_choices(ADSL, subset = c("SEX", "ARMCD", "COUNTRY"))
  )

  # functional subset
  obj <- variable_choices("ADSL", subset = function(data) colnames(data)[1:2])
  expect_equal(
    obj,
    structure(
      list(data = "ADSL", subset = function(data) colnames(data)[1:2], key = NULL),
      class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve(obj, datasets = data_list, keys = key_list))
  expect_equal(
    res_obj,
    variable_choices(ADSL, subset = colnames(ADSL)[1:2], key = teal.data::get_cdisc_keys("ADSL"))
  )

  # non-null key value
  obj <- variable_choices("ADSL", key = c("USUBJID", "STUDYID"))
  expect_equal(
    obj,
    structure(
      list(data = "ADSL", subset = NULL, key = c("USUBJID", "STUDYID")),
      class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve(obj, datasets = data_list, keys = key_list))
  expect_equal(
    res_obj,
    variable_choices(ADSL, key = c("USUBJID", "STUDYID"))
  )
})

# with resolve_delayed
data <- teal.data::cdisc_data(
  teal.data::cdisc_dataset("ADSL", ADSL),
  teal.data::cdisc_dataset("ADTTE", ADTTE)
)

ds <- teal.slice::init_filtered_data(data)

test_that("delayed version of variable_choices - resolve_delayed", {
  # hard-coded subset
  obj <- variable_choices("ADSL", subset = c("SEX", "ARMCD", "COUNTRY"))
  expect_equal(
    obj,
    structure(
      list(data = "ADSL", subset = c("SEX", "ARMCD", "COUNTRY"), key = NULL),
      class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  expect_equal(
    res_obj,
    variable_choices(ADSL, subset = c("SEX", "ARMCD", "COUNTRY"))
  )


  # functional subset
  obj <- variable_choices("ADSL", subset = function(data) colnames(data)[1:2])
  expect_equal(
    obj,
    structure(
      list(data = "ADSL", subset = function(data) colnames(data)[1:2], key = NULL),
      class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  expect_equal(
    res_obj,
    variable_choices(ADSL, subset = colnames(ADSL)[1:2], key = teal.data::get_cdisc_keys("ADSL"))
  )

  # non-null key value
  obj <- variable_choices("ADSL", key = c("USUBJID", "STUDYID"))
  expect_equal(
    obj,
    structure(
      list(data = "ADSL", subset = NULL, key = c("USUBJID", "STUDYID")),
      class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  expect_equal(
    res_obj,
    variable_choices(ADSL, key = c("USUBJID", "STUDYID"))
  )
})
