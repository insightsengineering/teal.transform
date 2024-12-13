adsl <- teal.data::rADSL
adtte <- teal.data::rADTTE

data_list <- list(ADSL = reactive(adsl), ADTTE = reactive(adtte))
join_keys <- teal.data::default_cdisc_join_keys[c("ADSL", "ADTTE")]
primary_keys_list <- lapply(join_keys, function(keys) keys[[1]])

testthat::test_that("resolve_delayed_expr works correctly", {
  # function assumptions check
  # 1) single argument called "data"
  testthat::expect_error(
    resolve_delayed_expr(function() {}, ds = adsl, is_value_choices = FALSE),
    regexp = "Assertion on 'x' failed: Must have formal arguments: data.",
    fixed = TRUE
  )
  testthat::expect_error(
    resolve_delayed_expr(function(a) {}, ds = adsl, is_value_choices = FALSE),
    regexp = "Assertion on 'x' failed: Must have formal arguments: data.",
    fixed = TRUE
  )
  testthat::expect_error(
    resolve_delayed_expr(function(data, a) {}, ds = adsl, is_value_choices = FALSE),
    regexp = "Assertion on 'x' failed: Must have exactly 1 formal arg",
    fixed = TRUE
  )

  # function assumptions check
  # 2a) returning character unique vector of length <= ncol(ds)
  testthat::expect_error(
    resolve_delayed_expr(function(data) 1, ds = adsl, is_value_choices = FALSE),
    regexp = "must return a character vector with unique names from the available columns of the dataset"
  )
  testthat::expect_error(
    resolve_delayed_expr(function(data) c("a", "a"), ds = adsl, is_value_choices = FALSE),
    regexp = "must return a character vector with unique names from the available columns of the dataset"
  )
  testthat::expect_error(
    resolve_delayed_expr(function(data) c("a", "b"), ds = adsl[1], is_value_choices = FALSE),
    regexp = "must return a character vector with unique names from the available columns of the dataset"
  )

  # function assumptions check
  # 2b) returning unique vector
  testthat::expect_error(
    resolve_delayed_expr(function(data) c(1, 1), ds = adsl, is_value_choices = TRUE),
    regexp = "must return a vector with unique values from the respective columns of the dataset"
  )

  # function return value check
  testthat::expect_equal(
    resolve_delayed_expr(function(data) c("a", "b"), ds = adsl, is_value_choices = FALSE),
    c("a", "b")
  )
  testthat::expect_equal(resolve_delayed_expr(function(data) 1:2, ds = adsl, is_value_choices = TRUE), 1:2)
})

testthat::test_that("resolve_delayed.FilteredData works correctly", {
  arm_ref_comp <- list(
    ARMCD = list(
      ref = value_choices(adtte, var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      comp = value_choices(adtte, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM B", "ARM C"))
    ),
    ARM = list(
      ref = variable_choices(adsl, subset = "ARM"), comp = variable_choices(adsl, subset = "ARMCD")
    ),
    ARM2 = list(ref = "A: Drug X", comp = c("B: Placebo", "C: Combination"))
  )
  arm_ref_comp_ddl <- list(
    ARMCD = list(
      ref = value_choices("ADTTE", var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      comp = value_choices("ADTTE", var_choices = "ARMCD", var_label = "ARM", subset = c("ARM B", "ARM C"))
    ),
    ARM = list(
      ref = variable_choices("ADSL", subset = "ARM"), comp = variable_choices("ADSL", subset = "ARMCD")
    ),
    ARM2 = list(ref = "A: Drug X", comp = c("B: Placebo", "C: Combination"))
  )
  ddl_resolved <- isolate(resolve_delayed(arm_ref_comp_ddl, datasets = data_list, keys = primary_keys_list))
  testthat::expect_identical(arm_ref_comp, ddl_resolved)
})


testthat::test_that("resolve_delayed.list works correctly with reactive objects", {
  arm_ref_comp <- list(
    ARMCD = list(
      ref = value_choices(adtte, var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      comp = value_choices(adtte, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM B", "ARM C"))
    ),
    ARM = list(
      ref = variable_choices(adsl, subset = "ARM"), comp = variable_choices(adsl, subset = "ARMCD")
    ),
    ARM2 = list(ref = "A: Drug X", comp = c("B: Placebo", "C: Combination"))
  )
  arm_ref_comp_ddl <- list(
    ARMCD = list(
      ref = value_choices("ADTTE", var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      comp = value_choices("ADTTE", var_choices = "ARMCD", var_label = "ARM", subset = c("ARM B", "ARM C"))
    ),
    ARM = list(
      ref = variable_choices("ADSL", subset = "ARM"), comp = variable_choices("ADSL", subset = "ARMCD")
    ),
    ARM2 = list(ref = "A: Drug X", comp = c("B: Placebo", "C: Combination"))
  )
  ddl_resolved <- isolate(
    resolve_delayed(
      arm_ref_comp_ddl,
      data_list,
      keys = primary_keys_list
    )
  )
  testthat::expect_identical(arm_ref_comp, ddl_resolved)
})

testthat::test_that("resolve_delayed.list works correctly with non-reactive objects", {
  data <- list(ADSL = adsl, ADTTE = reactive(adtte))
  arm_ref_comp <- list(
    ARMCD = list(
      ref = value_choices(adtte, var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      comp = value_choices(adtte, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM B", "ARM C"))
    ),
    ARM = list(
      ref = variable_choices(adsl, subset = "ARM"), comp = variable_choices(adsl, subset = "ARMCD")
    ),
    ARM2 = list(ref = "A: Drug X", comp = c("B: Placebo", "C: Combination"))
  )
  arm_ref_comp_ddl <- list(
    ARMCD = list(
      ref = value_choices("ADTTE", var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
      comp = value_choices("ADTTE", var_choices = "ARMCD", var_label = "ARM", subset = c("ARM B", "ARM C"))
    ),
    ARM = list(
      ref = variable_choices("ADSL", subset = "ARM"), comp = variable_choices("ADSL", subset = "ARMCD")
    ),
    ARM2 = list(ref = "A: Drug X", comp = c("B: Placebo", "C: Combination"))
  )
  ddl_resolved <- isolate(
    resolve_delayed(
      arm_ref_comp_ddl,
      data,
      keys = list(ADSL = c("STUDYID", "USUBJID"), ADTTE = c("STUDYID", "USUBJID", "PARAMCD"))
    )
  )
  testthat::expect_identical(arm_ref_comp, ddl_resolved)
})


testthat::test_that("resolving delayed choices removes selected not in choices and give a log output", {
  c_s <- choices_selected(
    choices = variable_choices("IRIS", c("Sepal.Length", "Sepal.Width")),
    selected = variable_choices("IRIS", c("Petal.Length", "Sepal.Width"))
  )

  testthat::expect_warning(
    output <- shiny::isolate({
      resolved_cs <- resolve_delayed(c_s, datasets = list(IRIS = reactive(iris)))
    }),
    "Removing Petal.Length from 'selected' as not in 'choices' when resolving delayed choices_selected"
  )

  testthat::expect_equal(resolved_cs$selected, stats::setNames("Sepal.Width", "Sepal.Width: Sepal.Width"))
})
