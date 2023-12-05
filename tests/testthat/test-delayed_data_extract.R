# Contains integration tests between delayed data loading objects and
# the objects responsible for loading, pulling and filtering the data
ADSL <- teal.transform::rADSL # nolint
ADTTE <- teal.transform::rADTTE # nolint
ADAE <- teal.transform::rADAE # nolint
ADRS <- teal.transform::rADRS # nolint

data_list <- list(ADSL = reactive(ADSL), ADTTE = reactive(ADTTE), ADAE = reactive(ADAE), ADRS = reactive(ADRS))
join_keys <- teal.data::default_cdisc_join_keys[c("ADSL", "ADTTE", "ADAE", "ADRS")]
primary_keys_list <- lapply(join_keys, function(x) x[[1]])

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

# Delayed data extract - single data connector with two scda dataset connectors ----
get_continuous <- function(data) {
  # example function to show selections from delayed data
  names(Filter(is.numeric, data))
}

testthat::test_that("Delayed data extract - single data connector with two scda dataset connectors", {
  x <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices("ADSL", subset = get_continuous)
    )
  )
  y <- data_extract_spec(
    dataname = "ADAE",
    select = select_spec(
      choices = variable_choices("ADAE", subset = get_continuous),
      selected = c("AGE: Age" = "AGE")
    )
  )

  x_expected <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(ADSL, subset = get_continuous, key = teal.data::get_cdisc_keys("ADSL")),
      selected = NULL
    )
  )
  y_expected <- data_extract_spec(
    dataname = "ADAE",
    select = select_spec(
      choices = variable_choices(ADAE, subset = get_continuous, key = teal.data::get_cdisc_keys("ADAE"))
    )
  )
  data_list <- list(ADSL = reactive(ADSL), ADAE = reactive(ADAE))
  primary_keys_list <- list(
    ADSL = teal.data::get_cdisc_keys("ADSL"),
    ADAE = teal.data::get_cdisc_keys("ADAE")
  )
  x_result <- isolate(resolve(x, datasets = data_list, keys = primary_keys_list))
  y_result <- isolate(resolve(y, datasets = data_list, keys = primary_keys_list))
  testthat::expect_identical(x_result, x_expected)
  testthat::expect_identical(y_result, y_expected)
})

# Delayed choices selected - single data connector with two scda dataset connectors ----

testthat::test_that("Delayed choices selected - single data connector with two scda dataset connectors", {
  data_list <- list(ADSL = reactive(ADSL), ADAE = reactive(ADAE))
  primary_keys_list <- list(
    ADSL = teal.data::get_cdisc_keys("ADSL"),
    ADAE = teal.data::get_cdisc_keys("ADAE")
  )
  choices <- variable_choices("ADSL")
  choices_result <- isolate(resolve(choices, datasets = data_list, keys = primary_keys_list))

  choices_expected <- variable_choices(ADSL, key = teal.data::get_cdisc_keys("ADSL"))
  testthat::expect_identical(choices_result, choices_expected)
})

# Delayed data extract - filtered ----

testthat::test_that("Delayed data extract - filtered", {
  data_list <- list(ADSL = reactive(ADSL), ADRS = reactive(ADRS))
  primary_keys_list <- list(
    ADSL = teal.data::get_cdisc_keys("ADSL"),
    ADRS = teal.data::get_cdisc_keys("ADRS")
  )

  x <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices("ADSL", subset = get_continuous)
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = "ARMCD",
      choices = value_choices("ADSL",
        var_choices = "ARMCD",
        var_label = "ARM",
        subset = function(data) levels(data$ARMCD)[1:2]
      ),
      selected = "ARM A",
      multiple = TRUE
    )
  )
  y <- data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices("ADRS", subset = get_continuous),
      selected = c("AGE: Age" = "AGE")
    )
  )

  x_expected <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(ADSL, subset = get_continuous),
      selected = NULL
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = "ARMCD",
      choices = value_choices(ADSL,
        var_choices = "ARMCD",
        var_label = "ARM",
        subset = function(data) levels(data$ARMCD)[1:2]
      ),
      selected = "ARM A",
      multiple = TRUE
    )
  )
  y_expected <- data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(ADRS, subset = get_continuous)
    )
  )

  x_result <- isolate(resolve(x, datasets = data_list, primary_keys_list))
  y_result <- isolate(resolve(y, datasets = data_list, primary_keys_list))
  testthat::expect_identical(x_result, x_expected)
  testthat::expect_identical(y_result, y_expected)
})
