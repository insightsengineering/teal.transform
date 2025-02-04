adsl <- teal.data::rADSL
adtte <- teal.data::rADTTE
data_list <- list(ADSL = reactive(adsl), ADTTE = reactive(adtte))
primary_keys_list <- list(ADSL = c("STUDYID", "USUBJID"), ADTTE = c("STUDYID", "USUBJID", "PARAMCD"))

testthat::test_that("Proper argument types", {
  choices <- c("c1", "c2", "c3")
  selected <- c("c1", "c2")
  testthat::expect_silent(select_spec(choices = choices, selected = selected))

  testthat::expect_error(select_spec(choices = list(list(choices)), selected = selected))
  testthat::expect_error(select_spec(choices = choices, selected = list(list(selected))))
  testthat::expect_error(select_spec(choices = choices, selected = selected, multiple = 1), "Assertion on 'multiple'")
  testthat::expect_error(
    select_spec(choices = choices, selected = selected, multiple = c(TRUE, TRUE)),
    "Assertion on 'multiple'"
  )
  testthat::expect_error(select_spec(choices = choices, selected = selected, fixed = 1), "Assertion on 'fixed'")
  testthat::expect_error(
    select_spec(choices = choices, selected = selected, label = factor("Hello")),
    "Assertion on 'label'"
  )
})

testthat::test_that("Single choice", {
  testthat::expect_silent(
    c1 <- select_spec(
      choices = c("AVAL", "BMRKR1", "AGE"),
      selected = c("AVAL"),
      fixed = FALSE,
      label = "Column"
    )
  )
  testthat::expect_silent(
    c2 <- select_spec(
      choices = c("AVAL", "BMRKR1", "AGE"),
      fixed = FALSE,
      label = "Column"
    )
  )

  testthat::expect_identical(c1, c2)
  testthat::expect_identical(class(c1), "select_spec")
  testthat::expect_identical(c1$choices, setNames(c("AVAL", "BMRKR1", "AGE"), c("AVAL", "BMRKR1", "AGE")))
  testthat::expect_identical(c2$selected, setNames("AVAL", "AVAL"))
  testthat::expect_false(c1$multiple)
  testthat::expect_false(c2$multiple)
  testthat::expect_false(c1$fixed)
  testthat::expect_false(c2$fixed)

  # minimal example
  testthat::expect_silent(c3 <- select_spec(choices = c("AVAL", "BMRKR1", "AGE")))
  testthat::expect_identical(class(c3), "select_spec")
  testthat::expect_identical(c3$choices, setNames(c("AVAL", "BMRKR1", "AGE"), c("AVAL", "BMRKR1", "AGE")))
  testthat::expect_identical(c3$selected, setNames("AVAL", "AVAL"))
  testthat::expect_false(c3$multiple)
  testthat::expect_false(c3$fixed)
  testthat::expect_identical(c3$label, "Select")
})

testthat::test_that("Multiple choices", {
  choices <- c("c1", "c2", "c3")
  selected <- c("c1", "c2")
  testthat::expect_error(
    select_spec(choices = choices, selected = selected, multiple = FALSE),
    "multiple \\|\\| length"
  )

  testthat::expect_silent(c1 <- select_spec(choices = choices, selected = selected, multiple = TRUE))
  testthat::expect_silent(c2 <- select_spec(choices = choices, selected = selected))
  testthat::expect_identical(c1, c2)

  testthat::expect_identical(
    names(c1),
    c("choices", "selected", "multiple", "fixed", "always_selected", "ordered", "label")
  )
  testthat::expect_identical(c1$choices, setNames(choices, choices))
  testthat::expect_identical(c1$selected, setNames(selected, selected))

  testthat::expect_true(c1$multiple)
  testthat::expect_false(c1$fixed)
  testthat::expect_null(c1$always_selected)
  testthat::expect_false(c1$ordered)
  testthat::expect_identical(c1$label, "Select")
})

testthat::test_that("resolve select_spec works", {
  attr(adsl, "keys") <- c("STUDYID", "USUBJID")

  expected_spec <- select_spec(
    choices = variable_choices(adsl, c("BMRKR1", "BMRKR2")),
    selected = "BMRKR1",
    multiple = FALSE,
    fixed = FALSE
  )

  delayed_spec <- select_spec(
    choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
    selected = "BMRKR1",
    multiple = FALSE,
    fixed = FALSE
  )

  testthat::expect_equal(class(delayed_spec), c("delayed_select_spec", "delayed_data", "select_spec"))

  testthat::expect_equal(names(expected_spec), names(delayed_spec))

  testthat::expect_identical(
    expected_spec,
    isolate(resolve(delayed_spec, datasets = data_list, keys = primary_keys_list))
  )
})

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

testthat::test_that("delayed version of select_spec", {
  # hard-coded choices & selected
  obj <- select_spec(vc_hard, selected = vc_hard_short, multiple = FALSE)
  testthat::expect_equal(
    obj,
    structure(
      list(
        choices = vc_hard_exp,
        selected = vc_hard_short_exp,
        multiple = FALSE,
        fixed = FALSE,
        always_selected = NULL,
        ordered = FALSE,
        label = "Select"
      ),
      class = c("delayed_select_spec", "delayed_data", "select_spec")
    )
  )

  res_obj <- isolate(resolve(obj, datasets = data_list, keys = primary_keys_list))
  exp_obj <- select_spec(
    variable_choices(adsl, subset = c("STUDYID", "USUBJID"), key = c("STUDYID", "USUBJID")),
    selected = variable_choices(adsl, "STUDYID", key = c("STUDYID", "USUBJID"))
  )
  testthat::expect_equal(res_obj, exp_obj)

  # functional choices & selected
  obj <- select_spec(vc_fun, selected = vc_fun_short, multiple = FALSE)
  testthat::expect_equal(
    obj,
    structure(
      list(
        choices = vc_fun_exp,
        selected = vc_fun_short,
        multiple = FALSE,
        fixed = FALSE,
        always_selected = NULL,
        ordered = FALSE,
        label = "Select"
      ),
      class = c("delayed_select_spec", "delayed_data", "select_spec")
    )
  )

  res_obj <- isolate(resolve(obj, datasets = data_list, keys = primary_keys_list))
  testthat::expect_equal(res_obj, exp_obj)
})

testthat::test_that("delayed_choices passed to selected selects desired choices", {
  testthat::expect_equal(
    select_spec(choices = letters, selected = letters),
    select_spec(choices = letters, selected = all_choices())
  )
  testthat::expect_equal(
    select_spec(choices = letters, selected = letters[1]),
    select_spec(choices = letters, selected = first_choice())
  )
  testthat::expect_equal(
    select_spec(choices = letters, selected = letters[length(letters)]),
    select_spec(choices = letters, selected = last_choice())
  )
  testthat::expect_equal(
    select_spec(choices = letters, selected = utils::head(letters, 4)),
    select_spec(choices = letters, selected = first_choices(4))
  )
  testthat::expect_equal(
    select_spec(choices = letters, selected = utils::tail(letters, 4)),
    select_spec(choices = letters, selected = last_choices(4))
  )
})

testthat::test_that("multiple is set to TRUE if all_choices() is passed to selected", {
  testthat::expect_true(select_spec(choices = variable_choices("test"), selected = all_choices())$multiple)
  testthat::expect_true(select_spec(choices = variable_choices(iris), selected = all_choices())$multiple)
})

testthat::test_that("default values", {
  test <- select_spec("a")
  testthat::expect_identical(test$selected, c(a = "a"))
  testthat::expect_false(test$multiple)
  testthat::expect_false(test$fixed)
  testthat::expect_null(test$always_selected)
  testthat::expect_false(test$ordered)
  testthat::expect_identical(test$label, "Select")
})

# With resolve_delayed
testthat::test_that("resolve_delayed select_spec works - resolve_delayed", {
  attr(adsl, "keys") <- c("STUDYID", "USUBJID")

  expected_spec <- select_spec(
    choices = variable_choices(adsl, c("BMRKR1", "BMRKR2")),
    selected = "BMRKR1",
    multiple = FALSE,
    fixed = FALSE
  )

  delayed_spec <- select_spec(
    choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
    selected = "BMRKR1",
    multiple = FALSE,
    fixed = FALSE
  )

  testthat::expect_equal(class(delayed_spec), c("delayed_select_spec", "delayed_data", "select_spec"))

  testthat::expect_equal(names(expected_spec), names(delayed_spec))

  testthat::expect_identical(
    expected_spec,
    isolate(resolve_delayed(delayed_spec, datasets = data_list, keys = primary_keys_list))
  )
})


testthat::test_that("delayed version of select_spec - resolve_delayed", {
  # hard-coded choices & selected
  obj <- select_spec(vc_hard, selected = vc_hard_short, multiple = FALSE)
  testthat::expect_equal(
    obj,
    structure(
      list(
        choices = vc_hard_exp,
        selected = vc_hard_short_exp,
        multiple = FALSE,
        fixed = FALSE,
        always_selected = NULL,
        ordered = FALSE,
        label = "Select"
      ),
      class = c("delayed_select_spec", "delayed_data", "select_spec")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = data_list, keys = primary_keys_list))
  exp_obj <- select_spec(
    variable_choices(adsl, subset = c("STUDYID", "USUBJID"), key = c("STUDYID", "USUBJID")),
    selected = variable_choices(adsl, "STUDYID", key = c("STUDYID", "USUBJID"))
  )
  testthat::expect_equal(res_obj, exp_obj)

  # functional choices & selected
  obj <- select_spec(vc_fun, selected = vc_fun_short, multiple = FALSE)
  testthat::expect_equal(
    obj,
    structure(
      list(
        choices = vc_fun_exp,
        selected = vc_fun_short,
        multiple = FALSE,
        fixed = FALSE,
        always_selected = NULL,
        ordered = FALSE,
        label = "Select"
      ),
      class = c("delayed_select_spec", "delayed_data", "select_spec")
    )
  )


  res_obj <- isolate(resolve_delayed(obj, datasets = data_list, keys = primary_keys_list))
  testthat::expect_equal(res_obj, exp_obj)
})
