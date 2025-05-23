adsl <- as.data.frame(as.list(setNames(nm = c("STUDYID", "USUBJID"))))
adtte <- as.data.frame(as.list(setNames(nm = c("STUDYID", "USUBJID", "PARAMCD"))))

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

testthat::test_that("delayed version of choices_selected", {
  # hard-coded choices and selected
  obj <- choices_selected(vc_hard, selected = vc_hard_short)
  testthat::expect_equal(
    obj,
    structure(
      list(choices = vc_hard_exp, selected = vc_hard_short_exp, keep_order = FALSE, fixed = FALSE),
      class = c("delayed_choices_selected", "delayed_data", "choices_selected")
    )
  )

  data_list <- list(ADSL = reactive(adsl), ADTTE = reactive(adtte))
  key_list <- list(ADSL = c("STUDYID", "USUBJID"), ADTTE = c("STUDYID", "USUBJID", "PARAMCD"))

  res_obj <- isolate(resolve(obj, datasets = data_list, keys = key_list))
  exp_obj <- choices_selected(
    variable_choices(adsl, subset = c("STUDYID", "USUBJID"), key = c("STUDYID", "USUBJID")),
    selected = variable_choices(adsl, subset = c("STUDYID"), key = c("STUDYID", "USUBJID"))
  )
  testthat::expect_equal(res_obj, exp_obj, check.attributes = TRUE)

  # functional choices and selected
  obj <- choices_selected(vc_fun, selected = vc_fun_short)
  testthat::expect_equal(
    obj,
    structure(
      list(choices = vc_fun_exp, selected = vc_fun_short_exp, keep_order = FALSE, fixed = FALSE),
      class = c("delayed_choices_selected", "delayed_data", "choices_selected")
    )
  )

  res_obj <- isolate(resolve(obj, datasets = data_list, keys = key_list))
  testthat::expect_equal(res_obj, exp_obj)
})

testthat::test_that("choices_selected throws error when selected is not found in choices", {
  testthat::expect_error(choices_selected(choices = c("a"), selected = "b"), "Must be a subset of \\{'a'\\}")
  testthat::expect_error(
    choices_selected(choices = c("a"), selected = c("a", "b")),
    "Must be a subset of \\{'a'\\}"
  )
  testthat::expect_error(
    choices_selected(choices = c("a"), selected = c("c", "b")),
    "Must be a subset of \\{'a'\\}"
  )
})

testthat::test_that("delayed_choices select the desired choices", {
  testthat::expect_equal(
    choices_selected(choices = letters, selected = letters),
    choices_selected(choices = letters, selected = all_choices())
  )
  testthat::expect_equal(
    choices_selected(choices = letters, selected = letters[1]),
    choices_selected(choices = letters, selected = first_choice())
  )
  testthat::expect_equal(
    choices_selected(choices = letters, selected = letters[length(letters)]),
    choices_selected(choices = letters, selected = last_choice())
  )
  testthat::expect_equal(
    choices_selected(choices = letters, selected = utils::head(letters, 4)),
    choices_selected(choices = letters, selected = first_choices(4))
  )
  testthat::expect_equal(
    choices_selected(choices = letters, selected = utils::tail(letters, 4)),
    choices_selected(choices = letters, selected = last_choices(4))
  )
})

testthat::test_that("choices_selected throws when selected is delayed and choices are not", {
  delayed_selected <- structure("A", class = "delayed_data")
  testthat::expect_error(
    choices_selected(choices = c("A", "B"), selected = delayed_selected),
    regexp = "If 'selected' is of class 'delayed_data', so must be 'choices'"
  )
})

testthat::test_that("choices_selected throws when no_select_keyword is passed to it as a choice", {
  no_select_keyword <- "-- no selection --"
  testthat::expect_error(
    choices_selected(choices = no_select_keyword),
    regexp = "-- no selection -- is not a valid choice as it is used as a keyword"
  )
})

testthat::test_that("is returns choices_selected if passed a choices selected object", {
  testthat::expect_equal(is(choices_selected(choices = "A")), "choices_selected")
})

testthat::test_that("choices_selected remove duplicates", {
  testthat::expect_identical(
    choices_selected(choices = c("A", "A")),
    structure(list(choices = "A", selected = "A", fixed = FALSE), class = "choices_selected")
  )
  testthat::expect_identical(
    choices_selected(choices = setNames(c("A", "A"), c("A", "A"))),
    structure(list(choices = c(A = "A"), selected = c(A = "A"), fixed = FALSE),
      class = "choices_selected"
    )
  )
  testthat::expect_equal(
    choices_selected(
      choices = c(
        "name for A" = "A", "name for A" = "A",
        "Name for nothing" = "", "name for b" = "B", "name for C" = "C"
      ),
      selected = c("A", "A")
    ),
    structure(
      list(
        choices = c(`name for A` = "A", `Name for nothing` = "", `name for b` = "B", `name for C` = "C"),
        selected = "A", fixed = FALSE
      ),
      class = "choices_selected"
    )
  )

  testthat::expect_equal(
    choices_selected(
      structure(c(`STUDYID: Study Identifier` = "STUDYID", `STUDYID: Study Identifier` = "STUDYID"),
        raw_labels = c(STUDYID = "Study Identifier", STUDYID = "Study Identifier"),
        combined_labels = c("STUDYID: Study Identifier", "STUDYID: Study Identifier"),
        class = c("choices_labeled", "character")
      )
    ),
    structure(list(
      choices = structure(c(`STUDYID: Study Identifier` = "STUDYID"),
        raw_labels = c(STUDYID = "Study Identifier"),
        combined_labels = "STUDYID: Study Identifier",
        class = c("choices_labeled", "character")
      ),
      selected = c(`STUDYID: Study Identifier` = "STUDYID"),
      fixed = FALSE
    ), class = "choices_selected")
  )
})

testthat::test_that("delayed version of choices_selected - resolve_delayed", {
  data_list <- list(ADSL = reactive(adsl), ADTTE = reactive(adtte))
  key_list <- list(ADSL = c("STUDYID", "USUBJID"), ADTTE = c("STUDYID", "USUBJID", "PARAMCD"))

  # hard-coded choices and selected
  obj <- choices_selected(vc_hard, selected = vc_hard_short)
  testthat::expect_equal(
    obj,
    structure(
      list(choices = vc_hard_exp, selected = vc_hard_short_exp, keep_order = FALSE, fixed = FALSE),
      class = c("delayed_choices_selected", "delayed_data", "choices_selected")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = data_list, keys = key_list))
  exp_obj <- choices_selected(
    variable_choices(adsl, subset = c("STUDYID", "USUBJID"), key = c("STUDYID", "USUBJID")),
    selected = variable_choices(adsl, subset = c("STUDYID"), key = c("STUDYID", "USUBJID"))
  )
  testthat::expect_equal(res_obj, exp_obj, check.attributes = TRUE)

  # functional choices and selected
  obj <- choices_selected(vc_fun, selected = vc_fun_short)
  testthat::expect_equal(
    obj,
    structure(
      list(choices = vc_fun_exp, selected = vc_fun_short_exp, keep_order = FALSE, fixed = FALSE),
      class = c("delayed_choices_selected", "delayed_data", "choices_selected")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = data_list, keys = key_list))
  testthat::expect_equal(res_obj, exp_obj)
})
