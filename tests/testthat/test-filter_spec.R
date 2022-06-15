library(scda)

choices <- c("val1", "val2", "val3")
choices_d <- c("val1", "val1", "val2", "val3")
choices_f <- as.factor(choices)
choices_l <- as.list(choices)

test_that("Proper argument types", {
  expect_error(filter_spec(vars = list("var"), choices = choices), "Assertion failed.+vars")
  expect_error(filter_spec(vars = "var", choices = choices_l), "Assertion failed.+choices")
  expect_error(filter_spec(vars = "var", choices = choices, selected = list("val2")), "Assertion failed.+selected")
  expect_error(filter_spec(vars = 1, choices = choices, selected = choices[1]), "Assertion failed.+vars")
  expect_error(filter_spec(vars = factor("var"), choices = choices, selected = choices[1]), "Assertion failed.+vars")
  expect_error(filter_spec(vars = "var", choices = choices_f, selected = choices_f[1]), "Assertion failed.+choices")
  expect_error(filter_spec(vars = "var", choices = choices, multiple = 1), "Assertion on 'multiple'")
  expect_error(filter_spec(vars = "var", choices = choices, label = factor("test")), "Assertion on 'label'")

  expect_error(filter_spec(vars = "var", choices = choices_d), "duplicated")
  expect_error(filter_spec(vars = "var", choices = choices, label = c("test", "test2")), "Assertion on 'label'")
  expect_error(filter_spec(vars = "var", choices = choices, sep = c("-", ",")), "Assertion on 'sep'")
})

test_that("Single choice", {
  expect_silent(f1 <- filter_spec(
    vars = "var1",
    choices = choices,
    selected = choices[1],
    multiple = FALSE,
    label = "test"
  ))
  expect_identical(names(f1), c(
    "vars_choices",
    "vars_selected",
    "vars_label",
    "vars_fixed",
    "vars_multiple",
    "choices",
    "selected",
    "label",
    "multiple",
    "fixed",
    "sep",
    "drop_keys",
    "dataname",
    "initialized"
  ))
  expect_identical(f1$choices, choices, choices)
  expect_identical(f1$selected, choices[1])
  expect_false(f1$multiple)
  expect_identical(f1$label, "test")
})

test_that("Multiple choices", {
  expect_error(filter_spec(vars = "var1", choices = choices, selected = choices[1:2], multiple = FALSE))
  expect_silent(f1 <- filter_spec(vars = "var1", choices = choices, selected = choices[1:2], multiple = TRUE))
  expect_silent(f2 <- filter_spec(vars = "var1", choices = choices, selected = choices[1:2]))
  expect_identical(f1, f2)

  expect_true(f1$multiple)
  expect_identical(f1$choices, choices)
  expect_identical(f1$selected, choices[1:2])
})

test_that("Multiple vars", {
  expect_error(filter_spec(vars = c("var1", "var2"), choices = c("val1.1-val2.1", "val1.1-val2.2"), sep = ":"))
  expect_error(filter_spec(vars = c("var1", "var2"), choices = c("val1.1-val2.1", "val1.1-val2.2")))
  expect_error(filter_spec(vars = "var1", choices = c("val-1", "val2", "val3", "val4"), sep = "-"))

  expect_silent(f1 <- filter_spec(
    vars = c("var1", "var2"),
    choices = c("val1.1 - val2.1", "val1.1 - val2.2"),
    sep = " - "
  ))

  expect_silent(f2 <- filter_spec(vars = c("var1", "var2"), choices = c("val1.1 - val2.1", "val1.1 - val2.2")))

  expect_silent(f5 <- filter_spec(
    vars = c("var1", "var2"),
    choices = c(`combo1` = "val1.1 - val2.1", `combo2` = "val1.1 - val2.2")
  ))

  expect_identical(f1, f2)
  expect_true(all(names(f1$choices) != names(f5$choices)))

  expect_identical(f1$vars_choices, c("var1", "var2"))
  expect_identical(f1$choices, c("val1.1 - val2.1", "val1.1 - val2.2"))
  expect_identical(names(f5$choices), c("combo1", "combo2"))
  expect_identical(f1$selected, c("val1.1 - val2.1"))

  # Multiple vars and multiple = TRUE
  choices <- c("val1.1 - val2.1", "val1.1 - val2.2", "val1.1 - val2.3")

  expect_silent(
    f1m <- filter_spec(
      vars = c("var1", "var2"),
      choices = choices,
      selected = choices[1:2],
      multiple = TRUE,
      sep = " - "
    )
  )

  expect_silent(
    f2m <- filter_spec(vars = c("var1", "var2"), choices = choices, selected = choices[1:2], sep = " - ")
  )

  expect_identical(f1m, f2m)

  # correct object structure
  expect_identical(f1m$choices, c("val1.1 - val2.1", "val1.1 - val2.2", "val1.1 - val2.3"))
  expect_identical(f1m$selected, c("val1.1 - val2.1", "val1.1 - val2.2"))

  expect_true(f1m$multiple)
  expect_identical(f1m$label, "Filter by")
})

test_that("Dropping keys attribute", {
  expect_silent(f1 <- filter_spec(
    vars = "var1",
    choices = choices,
    selected = choices[1]
  ))
  expect_false(f1$drop_keys)

  expect_silent(f2 <- filter_spec(
    vars = "var1",
    choices = choices,
    selected = choices[1],
    drop_keys = FALSE
  ))
  expect_false(f2$drop_keys)
})

test_that("delayed filter_spec", {
  set.seed(1)
  ADSL <- data.frame( # nolint
    USUBJID = letters[1:10],
    SEX = sample(c("F", "M", "U"), 10, replace = TRUE),
    stringsAsFactors = FALSE
  )

  expected_spec <- filter_spec(
    vars = variable_choices(ADSL, "SEX"),
    sep = "-",
    choices = value_choices(ADSL, "SEX", "SEX"),
    selected = "F",
    multiple = FALSE
  )

  # spec obtained using delayed approach
  delayed <- filter_spec(
    vars = variable_choices("ADSL", "SEX"),
    sep = "-",
    choices = value_choices("ADSL", "SEX", "SEX"),
    selected = "F",
    multiple = FALSE
  )

  expect_equal(names(expected_spec), names(delayed))

  ds <- teal.slice::init_filtered_data(
    list(ADSL = list(dataset = ADSL, keys = c("USUBJID", "STUDYID"), parent = character(0))),
    cdisc = TRUE
  )
  result_spec <- isolate(resolve_delayed(delayed, ds))
  expect_identical(expected_spec, isolate(resolve_delayed(delayed, ds)))
})


test_that("filter_spec with choices_selected where all selected in choices does not throw an error", {
  valid_cs <- choices_selected(
    choices = stats::setNames(LETTERS[1:5], paste("Letter", LETTERS[1:5])),
    selected = c("A", "B")
  )
  expect_error(filter_spec(vars = valid_cs), regexp = NA)
})


test_that("filter_spec_internal", {
  expect_silent(
    filter_spec_internal(
      vars_choices = letters,
      vars_selected = letters[1]
    )
  )

  expect_silent(
    filter_spec_internal(
      vars_choices = letters,
      vars_selected = letters[1:5]
    )
  )

  expect_silent(
    filter_spec_internal(
      vars_choices = variable_choices("ADSL")
    )
  )
})

test_that("filter_spec_internal contains dataname", {
  ADSL <- synthetic_cdisc_data("latest")$adsl # nolint

  x_filter <- filter_spec_internal(
    vars_choices = variable_choices(ADSL)
  )

  expect_null(x_filter$dataname)

  x <- data_extract_spec(
    dataname = "ADSL",
    filter = x_filter
  )

  expect_equal(x$filter[[1]]$dataname, "ADSL")
})

test_that("delayed filter_spec works", {
  set.seed(1)
  ADSL <- data.frame( # nolint
    USUBJID = letters[1:10],
    SEX = sample(c("F", "M", "U"), 10, replace = TRUE),
    stringsAsFactors = FALSE
  )

  expected_spec <- filter_spec_internal(
    vars_choices = variable_choices(ADSL),
    vars_selected = "SEX"
  )

  # spec obtained using delayed approach
  delayed <- filter_spec_internal(
    vars_choices = variable_choices("ADSL"),
    vars_selected = "SEX"
  )

  expect_equal(
    class(delayed),
    c(
      "delayed_filter_spec",
      "filter_spec",
      "delayed_data"
    )
  )

  expect_equal(names(expected_spec), names(delayed))

  ds <- teal.slice::init_filtered_data(
    list(ADSL = list(dataset = ADSL)),
    cdisc = TRUE
  )

  delayed$dataname <- "ADSL"
  expected_spec$dataname <- "ADSL"
  expect_identical(
    expected_spec,
    isolate(resolve_delayed(delayed, ds))
  )

  expected_spec <- data_extract_spec(
    dataname = "ADSL",
    filter = filter_spec_internal(
      vars_choices = variable_choices(ADSL),
      vars_selected = "SEX"
    )
  )

  delayed <- data_extract_spec(
    dataname = "ADSL",
    filter = filter_spec_internal(
      vars_choices = variable_choices("ADSL"),
      vars_selected = "SEX"
    )
  )

  expect_identical(expected_spec, isolate(resolve_delayed(delayed, ds)))
})


scda_data <- synthetic_cdisc_data("latest")
adsl <- scda_data$adsl
adtte <- scda_data$adtte
data <- teal.data::cdisc_data(
  teal.data::cdisc_dataset("ADSL", adsl),
  teal.data::cdisc_dataset("ADTTE", adtte)
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

testthat::test_that("delayed version of filter_spec", {
  # hard-coded vars & choices & selected
  obj <- filter_spec(
    vars = variable_choices("ADSL", subset = "ARMCD"),
    choices = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
    selected = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
    multiple = FALSE
  )

  testthat::expect_equal(
    obj,
    structure(
      list(
        vars_choices = variable_choices("ADSL", subset = "ARMCD"),
        vars_selected = variable_choices("ADSL", subset = "ARMCD"),
        vars_label = NULL,
        vars_fixed = TRUE,
        vars_multiple = TRUE,
        choices = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
        selected = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
        label = "Filter by",
        multiple = FALSE,
        fixed = FALSE,
        sep = " - ",
        drop_keys = FALSE,
        dataname = NULL,
        initialized = FALSE
      ),
      class = c(
        "delayed_filter_spec",
        "filter_spec",
        "delayed_data"
      )
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))
  exp_obj <- filter_spec(
    vars = variable_choices(adsl, subset = "ARMCD"),
    choices = value_choices(adsl, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B")),
    selected = value_choices(adsl, var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
    multiple = FALSE
  )

  # comparison not implemented, must be done individually
  testthat::expect_equal(res_obj$choices, exp_obj$choices)
  testthat::expect_equal(res_obj$selected, exp_obj$selected)
  testthat::expect_equal(
    res_obj[-match(c("choices", "selected"), names(res_obj))],
    exp_obj[-match(c("choices", "selected"), names(exp_obj))]
  )


  # functional choices & selected
  obj <- filter_spec(
    vars = variable_choices("ADSL", subset = function(data) "ARMCD"),
    choices = value_choices(
      "ADSL",
      var_choices = "ARMCD",
      var_label = "ARM",
      subset = function(data) levels(data$ARMCD)[1:2]
    ),
    selected = value_choices(
      "ADSL",
      var_choices = "ARMCD",
      var_label = "ARM",
      subset = function(data) "ARM A"
    ),
    multiple = FALSE
  )

  testthat::expect_equal(
    obj,
    structure(
      list(
        vars_choices = variable_choices("ADSL", subset = function(data) "ARMCD"),
        vars_selected = variable_choices("ADSL", subset = function(data) "ARMCD"),
        vars_label = NULL,
        vars_fixed = TRUE,
        vars_multiple = TRUE,
        choices = value_choices(
          "ADSL",
          var_choices = "ARMCD",
          var_label = "ARM",
          subset = function(data) levels(data$ARMCD)[1:2]
        ),
        selected = value_choices(
          "ADSL",
          var_choices = "ARMCD",
          var_label = "ARM",
          subset = function(data) "ARM A"
        ),
        label = "Filter by",
        multiple = FALSE,
        fixed = FALSE,
        sep = " - ",
        drop_keys = FALSE,
        dataname = NULL,
        initialized = FALSE
      ),
      class = c(
        "delayed_filter_spec",
        "filter_spec",
        "delayed_data"
      )
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = ds))

  # comparison not implemented, must be done individually
  testthat::expect_equal(res_obj$choices, exp_obj$choices)
  testthat::expect_equal(res_obj$selected, exp_obj$selected)
  testthat::expect_equal(
    res_obj[-match(c("choices", "selected"), names(res_obj))],
    exp_obj[-match(c("choices", "selected"), names(exp_obj))]
  )
})

testthat::test_that("all_choices passed to selected identical to all choices", {
  testthat::expect_equal(
    filter_spec(vars = "test", choices = c(1, 2), selected = c(1, 2)),
    filter_spec(vars = "test", choices = c(1, 2), selected = all_choices())
  )
})