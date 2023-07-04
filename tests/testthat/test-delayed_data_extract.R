# Contains integration tests between delayed data loading objects and
# the objects responsible for loading, pulling and filtering the data
ADSL <- rADSL # nolint
ADTTE <- rADTTE # nolint
data <- teal.data::cdisc_data(
  teal.data::cdisc_dataset("ADSL", ADSL),
  teal.data::cdisc_dataset("ADTTE", ADTTE)
)

ds <- teal.slice::init_filtered_data(data)

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
  idx <- vapply(data, function(x) is.numeric(x) && length(unique(x)) > 6, logical(1))
  colnames(data)[idx]
}

testthat::test_that("Delayed data extract - single data connector with two scda dataset connectors", {
  ADSL <- teal.data::cdisc_dataset(dataname = "ADSL", x = rADSL)
  adae <- teal.data::cdisc_dataset(dataname = "ADAE", x = rADAE)
  data <- teal.data::cdisc_data(ADSL, adae)

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

  for (connector in data$get_connectors()) {
    connector$pull()
  }
  # test delayed data extract
  ds <- teal.slice::init_filtered_data(data)

  adsl <- data$get_dataset("ADSL")$get_raw_data() # nolint
  adae <- data$get_dataset("ADAE")$get_raw_data() # nolint
  x_expected <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(adsl, subset = get_continuous, key = teal.data::get_cdisc_keys("ADSL")),
      selected = NULL
    )
  )
  y_expected <- data_extract_spec(
    dataname = "ADAE",
    select = select_spec(
      choices = variable_choices(adae, subset = get_continuous, key = teal.data::get_cdisc_keys("ADAE"))
    )
  )
  data_list <- sapply(X = ds$datanames(), simplify = FALSE, FUN = function(x) {
    reactive(ds$get_data(dataname = x, filtered = TRUE))
  })
  key_list <- sapply(X = ds$datanames(), simplify = FALSE, FUN = function(x) {
    isolate(ds$get_keys(dataname = x))
  })
  x_result <- isolate(resolve(x, datasets = data_list, keys = key_list))
  y_result <- isolate(resolve(y, datasets = data_list, keys = key_list))
  testthat::expect_identical(x_result, x_expected)
  testthat::expect_identical(y_result, y_expected)
})

# Delayed choices selected - single data connector with two scda dataset connectors ----

testthat::test_that("Delayed choices selected - single data connector with two scda dataset connectors", {
  ADSL <- teal.data::cdisc_dataset(dataname = "ADSL", x = rADSL)
  adae <- teal.data::cdisc_dataset(dataname = "ADAE", x = rADAE)

  data <- teal.data::cdisc_data(ADSL, adae)

  choices <- variable_choices("ADSL")
  for (connector in data$get_connectors()) {
    connector$pull()
  }
  ds <- teal.slice::init_filtered_data(data)

  ADSL <- data$get_dataset("ADSL")$get_raw_data() # nolint
  choices_expected <- variable_choices(ADSL, key = teal.data::get_cdisc_keys("ADSL"))
  data_list <- sapply(X = ds$datanames(), simplify = FALSE, FUN = function(x) {
    reactive(ds$get_data(dataname = x, filtered = TRUE))
  })
  key_list <- sapply(X = ds$datanames(), simplify = FALSE, FUN = function(x) {
    isolate(ds$get_keys(dataname = x))
  })
  choices_result <- isolate(resolve(choices, datasets = data_list, keys = key_list))
  testthat::expect_identical(choices_result, choices_expected)
})

# Delayed data extract - filtered ----

testthat::test_that("Delayed data extract - filtered", {
  ADSL <- teal.data::cdisc_dataset(dataname = "ADSL", x = rADSL)
  adrs <- teal.data::cdisc_dataset(dataname = "ADRS", x = rADRS)
  data <- teal.data::cdisc_data(ADSL, adrs)

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

  ds <- teal.slice::init_filtered_data(data)

  ADSL <- data$get_dataset("ADSL")$get_raw_data() # nolint
  ADRS <- data$get_dataset("ADRS")$get_raw_data() # nolint
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
  data_list <- sapply(X = ds$datanames(), simplify = FALSE, FUN = function(x) {
    reactive(ds$get_data(dataname = x, filtered = TRUE))
  })
  key_list <- sapply(X = ds$datanames(), simplify = FALSE, FUN = function(x) {
    isolate(ds$get_keys(dataname = x))
  })
  x_result <- isolate(resolve(x, datasets = data_list, key_list))
  y_result <- isolate(resolve(y, datasets = data_list, key_list))
  testthat::expect_identical(x_result, x_expected)
  testthat::expect_identical(y_result, y_expected)
})

# Delayed extract filter concatenated - single data connector with two scda dataset connectors ----
testthat::test_that("Delayed extract filter concatenated - single data connector with two scda dataset connectors", {
  ADSL <- teal.data::cdisc_dataset(dataname = "ADSL", x = rADSL)
  adrs <- teal.data::cdisc_dataset(dataname = "ADRS", x = rADRS)
  data <- teal.data::teal_data(ADSL, adrs)

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
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = c("PARAMCD", "AVISIT"),
      choices = value_choices(
        data = "ADRS",
        var_choices = c("PARAMCD", "AVISIT"),
        var_label = c("PARAMCD", "AVISIT"),
        subset = function(data) {
          paste(
            levels(data$PARAMCD),
            levels(data$AVISIT)[4:6],
            sep = " - "
          )
        }
      ),
      selected = "INVET - END OF INDUCTION",
      multiple = TRUE
    )
  )

  ds <- teal.slice::init_filtered_data(data)

  ADSL <- data$get_dataset("ADSL")$get_raw_data() # nolint
  ADRS <- data$get_dataset("ADRS")$get_raw_data() # nolint
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
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = c("PARAMCD", "AVISIT"),
      choices = value_choices(
        data = ADRS,
        var_choices = c("PARAMCD", "AVISIT"),
        var_label = c("PARAMCD", "AVISIT"),
        subset = function(data) {
          paste(
            levels(data$PARAMCD),
            levels(data$AVISIT)[4:6],
            sep = " - "
          )
        }
      ),
      selected = "INVET - END OF INDUCTION",
      multiple = TRUE
    )
  )
  data_list <- sapply(X = ds$datanames(), simplify = FALSE, FUN = function(x) {
    reactive(ds$get_data(dataname = x, filtered = TRUE))
  })
  key_list <- sapply(X = ds$datanames(), simplify = FALSE, FUN = function(x) {
    isolate(ds$get_keys(dataname = x))
  })
  x_result <- isolate(resolve(x, datasets = data_list, key_list))
  y_result <- isolate(resolve(y, datasets = data_list, key_list))
  testthat::expect_identical(x_result, x_expected)
  testthat::expect_identical(y_result, y_expected)
})

# Delayed extract two filters - single data connector with two scda dataset connectors ----
testthat::test_that("Delayed extract two filters - single data connector with two scda dataset connectors", {
  ADSL <- teal.data::cdisc_dataset(dataname = "ADSL", x = rADSL)
  adrs <- teal.data::cdisc_dataset(dataname = "ADRS", x = rADRS)
  data <- teal.data::teal_data(ADSL, adrs)

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
    ),
    filter = list(
      filter_spec(
        label = "Select endpoints:",
        vars = "PARAMCD",
        choices = value_choices(
          data = "ADRS",
          var_choices = "PARAMCD",
          var_label = "PARAMCD",
          subset = function(data) levels(data$PARAMCD)[2:3]
        ),
        selected = "OVRINV",
        multiple = TRUE
      ),
      filter_spec(
        label = "Select endpoints:",
        vars = "AVISIT",
        choices = value_choices(
          data = "ADRS",
          var_choices = "AVISIT",
          var_label = "AVISIT",
          subset = function(data) levels(data$AVISIT)[5:6]
        ),
        selected = "END OF INDUCTION",
        multiple = TRUE
      )
    )
  )

  ds <- teal.slice::init_filtered_data(data)

  ADSL <- data$get_dataset("ADSL")$get_raw_data() # nolint
  ADRS <- data$get_dataset("ADRS")$get_raw_data() # nolint
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
    ),
    filter = list(
      filter_spec(
        label = "Select endpoints:",
        vars = "PARAMCD",
        choices = value_choices(
          data = ADRS,
          var_choices = "PARAMCD",
          var_label = "PARAMCD",
          subset = function(data) levels(data$PARAMCD)[2:3]
        ),
        selected = "OVRINV",
        multiple = TRUE
      ),
      filter_spec(
        label = "Select endpoints:",
        vars = "AVISIT",
        choices = value_choices(
          data = ADRS,
          var_choices = "AVISIT",
          var_label = "AVISIT",
          subset = function(data) levels(data$AVISIT)[5:6]
        ),
        selected = "END OF INDUCTION",
        multiple = TRUE
      )
    )
  )
  data_list <- sapply(X = ds$datanames(), simplify = FALSE, FUN = function(x) {
    reactive(ds$get_data(dataname = x, filtered = TRUE))
  })
  key_list <- sapply(X = ds$datanames(), simplify = FALSE, FUN = function(x) {
    isolate(ds$get_keys(dataname = x))
  })
  x_result <- isolate(resolve(x, datasets = data_list, key_list))
  y_result <- isolate(resolve(y, datasets = data_list, key_list))
  testthat::expect_identical(x_result, x_expected)
  testthat::expect_identical(y_result, y_expected)
})

# Delayed extract - dataset & connector ----
testthat::test_that("Delayed extract - TealData with single dataset and multiple connectors", {
  ADSL <- teal.data::dataset(
    dataname = "ADSL",
    rADSL,
    keys = teal.data::get_cdisc_keys("ADSL"),
    code = "ADSL <- rADSL",
    label = "ADSL"
  )

  adrs <- teal.data::cdisc_dataset(dataname = "ADRS", x = rADRS)
  ADTTE <- teal.data::cdisc_dataset(dataname = "ADTTE", x = rADTTE)
  data <- teal.data::cdisc_data(ADSL, adrs, ADTTE)

  x <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices("ADSL", subset = get_continuous)
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = "ARMCD",
      choices = value_choices(ADSL$get_raw_data(), var_choices = "ARMCD", var_label = "ARM"),
      selected = "ARM A",
      multiple = TRUE
    )
  )
  y <- data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices("ADRS", subset = get_continuous),
      selected = c("AGE: Age" = "AGE")
    ),
    filter = list(
      filter_spec(
        label = "Select endpoints:",
        vars = "PARAMCD",
        choices = value_choices(
          data = "ADRS",
          var_choices = "PARAMCD",
          var_label = "PARAMCD",
          subset = function(data) levels(data$PARAMCD)[2:3]
        ),
        selected = "OVRINV",
        multiple = TRUE
      ),
      filter_spec(
        label = "Select endpoints:",
        vars = "AVISIT",
        choices = value_choices(
          data = "ADRS",
          var_choices = "AVISIT",
          var_label = "AVISIT",
          subset = function(data) levels(data$AVISIT)[5:6]
        ),
        selected = "END OF INDUCTION",
        multiple = TRUE
      )
    )
  )

  ds <- teal.slice::init_filtered_data(data)

  adsl <- data$get_dataset("ADSL")$get_raw_data() # nolint
  adrs <- data$get_dataset("ADRS")$get_raw_data() # nolint
  x_expected <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(adsl, subset = get_continuous, key = teal.data::get_cdisc_keys("ADSL")),
      NULL
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = "ARMCD",
      choices = value_choices(ADSL$get_raw_data(), var_choices = "ARMCD", var_label = "ARM"),
      selected = "ARM A",
      multiple = TRUE
    )
  )
  y_expected <- data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(adrs, subset = get_continuous, key = teal.data::get_cdisc_keys("ADRS"))
    ),
    filter = list(
      filter_spec(
        label = "Select endpoints:",
        vars = "PARAMCD",
        choices = value_choices(
          data = adrs,
          var_choices = "PARAMCD",
          var_label = "PARAMCD",
          subset = function(data) levels(data$PARAMCD)[2:3]
        ),
        selected = "OVRINV",
        multiple = TRUE
      ),
      filter_spec(
        label = "Select endpoints:",
        vars = "AVISIT",
        choices = value_choices(
          data = adrs,
          var_choices = "AVISIT",
          var_label = "AVISIT",
          subset = function(data) levels(data$AVISIT)[5:6]
        ),
        selected = "END OF INDUCTION",
        multiple = TRUE
      )
    )
  )
  data_list <- sapply(X = ds$datanames(), simplify = FALSE, FUN = function(x) {
    reactive(ds$get_data(dataname = x, filtered = TRUE))
  })
  key_list <- sapply(X = ds$datanames(), simplify = FALSE, FUN = function(x) {
    isolate(ds$get_keys(dataname = x))
  })
  x_result <- isolate(resolve(x, datasets = data_list, key_list))
  y_result <- isolate(resolve(y, datasets = data_list, key_list))
  testthat::expect_identical(x_result, x_expected)
  testthat::expect_identical(y_result, y_expected)
})

# with resolve_delayed
testthat::test_that("Delayed data extract - single data connector with two scda dataset connectors - resolve_delayed", {
  ADSL <- teal.data::cdisc_dataset(dataname = "ADSL", x = rADSL)
  adae <- teal.data::cdisc_dataset(dataname = "ADAE", x = rADAE)

  data <- teal.data::cdisc_data(ADSL, adae)

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

  # test delayed data extract
  ds <- teal.slice::init_filtered_data(data)

  ADSL <- data$get_dataset("ADSL")$get_raw_data() # nolint
  ADAE <- data$get_dataset("ADAE")$get_raw_data() # nolint
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
  x_result <- isolate(resolve_delayed(x, datasets = ds))
  y_result <- isolate(resolve_delayed(y, datasets = ds))
  testthat::expect_identical(x_result, x_expected)
  testthat::expect_identical(y_result, y_expected)
})

# Delayed choices selected - single data connector with two scda dataset connectors ----
testthat::test_that(
  desc = "Delayed choices selected - single data connector with two scda dataset connectors - resolve_delayed",
  code = {
    ADSL <- teal.data::cdisc_dataset(dataname = "ADSL", x = rADSL)
    adae <- teal.data::cdisc_dataset(dataname = "ADAE", x = rADAE)
    data <- teal.data::cdisc_data(ADSL, adae)

    choices <- variable_choices("ADSL")
    ds <- teal.slice::init_filtered_data(data)

    ADSL <- data$get_dataset("ADSL")$get_raw_data() # nolint
    choices_expected <- variable_choices(ADSL, key = teal.data::get_cdisc_keys("ADSL"))
    choices_result <- isolate(resolve_delayed(choices, datasets = ds))
    testthat::expect_identical(choices_result, choices_expected)
  }
)

# Delayed data extract - filtered ----

testthat::test_that("Delayed data extract - filtered", {
  ADSL <- teal.data::cdisc_dataset(dataname = "ADSL", x = rADSL)
  adrs <- teal.data::cdisc_dataset(dataname = "ADRS", x = rADRS)
  data <- teal.data::cdisc_data(ADSL, adrs)

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

  ds <- teal.slice::init_filtered_data(data)

  adsl <- data$get_dataset("ADSL")$get_raw_data() # nolint
  adrs <- data$get_dataset("ADRS")$get_raw_data() # nolint
  x_expected <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(adsl, subset = get_continuous),
      selected = NULL
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = "ARMCD",
      choices = value_choices(adsl,
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
      choices = variable_choices(adrs, subset = get_continuous)
    )
  )
  x_result <- isolate(resolve_delayed(x, datasets = ds))
  y_result <- isolate(resolve_delayed(y, datasets = ds))
  testthat::expect_identical(x_result, x_expected)
  testthat::expect_identical(y_result, y_expected)
})

# Delayed extract filter concatenated - single data connector with two scda dataset connectors ----
testthat::test_that(
  desc = "Delayed extract filter concatenated - single data connector with two scda dataset connectors - res_delayed",
  code = {
    ADSL <- teal.data::cdisc_dataset(dataname = "ADSL", x = rADSL)
    adrs <- teal.data::cdisc_dataset(dataname = "ADRS", x = rADRS)
    data <- teal.data::teal_data(ADSL, adrs)

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
      ),
      filter = filter_spec(
        label = "Select endpoints:",
        vars = c("PARAMCD", "AVISIT"),
        choices = value_choices(
          data = "ADRS",
          var_choices = c("PARAMCD", "AVISIT"),
          var_label = c("PARAMCD", "AVISIT"),
          subset = function(data) {
            paste(
              levels(data$PARAMCD),
              levels(data$AVISIT)[4:6],
              sep = " - "
            )
          }
        ),
        selected = "INVET - END OF INDUCTION",
        multiple = TRUE
      )
    )

    ds <- teal.slice::init_filtered_data(data)

    adsl <- data$get_dataset("ADSL")$get_raw_data() # nolint
    adrs <- data$get_dataset("ADRS")$get_raw_data() # nolint
    x_expected <- data_extract_spec(
      dataname = "ADSL",
      select = select_spec(
        choices = variable_choices(adsl, subset = get_continuous),
        selected = NULL
      ),
      filter = filter_spec(
        label = "Select endpoints:",
        vars = "ARMCD",
        choices = value_choices(adsl,
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
        choices = variable_choices(adrs, subset = get_continuous)
      ),
      filter = filter_spec(
        label = "Select endpoints:",
        vars = c("PARAMCD", "AVISIT"),
        choices = value_choices(
          data = adrs,
          var_choices = c("PARAMCD", "AVISIT"),
          var_label = c("PARAMCD", "AVISIT"),
          subset = function(data) {
            paste(
              levels(data$PARAMCD),
              levels(data$AVISIT)[4:6],
              sep = " - "
            )
          }
        ),
        selected = "INVET - END OF INDUCTION",
        multiple = TRUE
      )
    )
    x_result <- isolate(resolve_delayed(x, datasets = ds))
    y_result <- isolate(resolve_delayed(y, datasets = ds))
    testthat::expect_identical(x_result, x_expected)
    testthat::expect_identical(y_result, y_expected)
  }
)

# Delayed extract two filters - single data connector with two scda dataset connectors ----
testthat::test_that(
  desc = "Delayed extract two filters - single data connector with two scda dataset connectors - resolve_delayed",
  code = {
    ADSL <- teal.data::cdisc_dataset(dataname = "ADSL", x = rADSL)
    adrs <- teal.data::cdisc_dataset(dataname = "ADRS", x = rADRS)
    data <- teal.data::teal_data(ADSL, adrs)

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
      ),
      filter = list(
        filter_spec(
          label = "Select endpoints:",
          vars = "PARAMCD",
          choices = value_choices(
            data = "ADRS",
            var_choices = "PARAMCD",
            var_label = "PARAMCD",
            subset = function(data) levels(data$PARAMCD)[2:3]
          ),
          selected = "OVRINV",
          multiple = TRUE
        ),
        filter_spec(
          label = "Select endpoints:",
          vars = "AVISIT",
          choices = value_choices(
            data = "ADRS",
            var_choices = "AVISIT",
            var_label = "AVISIT",
            subset = function(data) levels(data$AVISIT)[5:6]
          ),
          selected = "END OF INDUCTION",
          multiple = TRUE
        )
      )
    )

    ds <- teal.slice::init_filtered_data(data)

    adsl <- data$get_dataset("ADSL")$get_raw_data() # nolint
    adrs <- data$get_dataset("ADRS")$get_raw_data() # nolint
    x_expected <- data_extract_spec(
      dataname = "ADSL",
      select = select_spec(
        choices = variable_choices(adsl, subset = get_continuous),
        selected = NULL
      ),
      filter = filter_spec(
        label = "Select endpoints:",
        vars = "ARMCD",
        choices = value_choices(adsl,
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
        choices = variable_choices(adrs, subset = get_continuous)
      ),
      filter = list(
        filter_spec(
          label = "Select endpoints:",
          vars = "PARAMCD",
          choices = value_choices(
            data = adrs,
            var_choices = "PARAMCD",
            var_label = "PARAMCD",
            subset = function(data) levels(data$PARAMCD)[2:3]
          ),
          selected = "OVRINV",
          multiple = TRUE
        ),
        filter_spec(
          label = "Select endpoints:",
          vars = "AVISIT",
          choices = value_choices(
            data = adrs,
            var_choices = "AVISIT",
            var_label = "AVISIT",
            subset = function(data) levels(data$AVISIT)[5:6]
          ),
          selected = "END OF INDUCTION",
          multiple = TRUE
        )
      )
    )
    x_result <- isolate(resolve_delayed(x, datasets = ds))
    y_result <- isolate(resolve_delayed(y, datasets = ds))
    testthat::expect_identical(x_result, x_expected)
    testthat::expect_identical(y_result, y_expected)
  }
)

# Delayed extract - dataset & connector ----
testthat::test_that("Delayed extract - TealData with single dataset and multiple connectors - resolve_delayed", {
  ADSL <- teal.data::dataset(
    dataname = "ADSL",
    rADSL,
    keys = teal.data::get_cdisc_keys("ADSL"),
    code = "ADSL <- rADSL",
    label = "ADSL"
  )
  adrs <- teal.data::cdisc_dataset(dataname = "ADRS", x = rADRS)
  ADTTE <- teal.data::cdisc_dataset(dataname = "ADTTE", x = rADTTE)
  data <- teal.data::cdisc_data(ADSL, adrs, ADTTE)

  x <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices("ADSL", subset = get_continuous)
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = "ARMCD",
      choices = value_choices(ADSL$get_raw_data(), var_choices = "ARMCD", var_label = "ARM"),
      selected = "ARM A",
      multiple = TRUE
    )
  )
  y <- data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices("ADRS", subset = get_continuous),
      selected = c("AGE: Age" = "AGE")
    ),
    filter = list(
      filter_spec(
        label = "Select endpoints:",
        vars = "PARAMCD",
        choices = value_choices(
          data = "ADRS",
          var_choices = "PARAMCD",
          var_label = "PARAMCD",
          subset = function(data) levels(data$PARAMCD)[2:3]
        ),
        selected = "OVRINV",
        multiple = TRUE
      ),
      filter_spec(
        label = "Select endpoints:",
        vars = "AVISIT",
        choices = value_choices(
          data = "ADRS",
          var_choices = "AVISIT",
          var_label = "AVISIT",
          subset = function(data) levels(data$AVISIT)[5:6]
        ),
        selected = "END OF INDUCTION",
        multiple = TRUE
      )
    )
  )

  ds <- teal.slice::init_filtered_data(data)

  adsl <- data$get_dataset("ADSL")$get_raw_data() # nolint
  adrs <- data$get_dataset("ADRS")$get_raw_data() # nolint
  x_expected <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(adsl, subset = get_continuous, key = teal.data::get_cdisc_keys("ADSL")),
      NULL
    ),
    filter = filter_spec(
      label = "Select endpoints:",
      vars = "ARMCD",
      choices = value_choices(ADSL$get_raw_data(), var_choices = "ARMCD", var_label = "ARM"),
      selected = "ARM A",
      multiple = TRUE
    )
  )
  y_expected <- data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(adrs, subset = get_continuous, key = teal.data::get_cdisc_keys("ADRS"))
    ),
    filter = list(
      filter_spec(
        label = "Select endpoints:",
        vars = "PARAMCD",
        choices = value_choices(
          data = adrs,
          var_choices = "PARAMCD",
          var_label = "PARAMCD",
          subset = function(data) levels(data$PARAMCD)[2:3]
        ),
        selected = "OVRINV",
        multiple = TRUE
      ),
      filter_spec(
        label = "Select endpoints:",
        vars = "AVISIT",
        choices = value_choices(
          data = adrs,
          var_choices = "AVISIT",
          var_label = "AVISIT",
          subset = function(data) levels(data$AVISIT)[5:6]
        ),
        selected = "END OF INDUCTION",
        multiple = TRUE
      )
    )
  )
  x_result <- isolate(resolve_delayed(x, datasets = ds))
  y_result <- isolate(resolve_delayed(y, datasets = ds))
  testthat::expect_identical(x_result, x_expected)
  testthat::expect_identical(y_result, y_expected)
})
