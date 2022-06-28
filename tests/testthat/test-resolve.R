library(scda)
scda_data <- synthetic_cdisc_data("latest")
adsl <- scda_data$adsl # nolint
adtte <- scda_data$adtte # nolint

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

testthat::test_that("resolve_delayed_expr works correctly", {
  # function assumptions check
  # 1) single argument called "data"
  testthat::expect_error(
    resolve_delayed_expr(function() {}, ds = adsl, is_value_choices = FALSE), # nolint
    regexp = "Assertion on 'x' failed: Must have formal arguments: data.",
    fixed = TRUE
  )
  testthat::expect_error(
    resolve_delayed_expr(function(a) {}, ds = adsl, is_value_choices = FALSE), # nolint
    regexp = "Assertion on 'x' failed: Must have formal arguments: data.",
    fixed = TRUE
  )
  testthat::expect_error(
    resolve_delayed_expr(function(data, a) {}, ds = adsl, is_value_choices = FALSE), # nolint
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

testthat::test_that("resolve.list works correctly", {
  data_list <- list(ADSL = reactive(adsl), ADTTE = reactive(adtte))
  key_list <- list(ADSL = teal.data::get_cdisc_keys("ADSL"), ADTTE = teal.data::get_cdisc_keys("ADTTE"))

  ddl_resolved <- isolate(resolve(arm_ref_comp_ddl, data_list, key_list))
  testthat::expect_identical(arm_ref_comp, ddl_resolved)
})


testthat::test_that("resolving delayed choices removes selected not in choices and give a log output", {
  iris_dataset <- teal.data::dataset("IRIS", head(iris))

  c_s <- choices_selected(
    choices = variable_choices("IRIS", c("Sepal.Length", "Sepal.Width")),
    selected = variable_choices("IRIS", c("Petal.Length", "Sepal.Width"))
  )

  output <- testthat::capture_output({
    data_list <- list(IRIS = reactive(head(iris)))
    key_list <- list(IRIS = character(0))
    resolved_cs <- isolate(resolve(c_s, data_list, key_list))
  })

  testthat::expect_equal(resolved_cs$selected, stats::setNames("Sepal.Width", "Sepal.Width: Sepal.Width"))
  testthat::expect_true(
    grepl("Removing Petal.Length from 'selected' as not in 'choices' when resolving delayed choices_selected", output)
  )
})

testthat::test_that("resolve throws error with non-reactive data.frames or unnamed list as input to datasets", {
  data_list <- list(ADSL = adsl, ADTTE = adtte)
  key_list <- list(ADSL = teal.data::get_cdisc_keys("ADSL"), ADTTE = teal.data::get_cdisc_keys("ADTTE"))

  testthat::expect_error(
    isolate(resolve(arm_ref_comp_ddl, data_list, key_list)),
    "Assertion on 'datasets' failed: May only contain the following types: {reactive}",
    fixed = TRUE
  )

  data_list2 <- list(reactive(adsl), reactive(adtte))
  testthat::expect_error(
    isolate(resolve(arm_ref_comp_ddl, data_list2, key_list)),
    "Assertion on 'datasets' failed: Must have names."
  )
})

testthat::test_that("resolve throws error with unnamed list or wrong names as input to keys", {
  data_list <- list(ADSL = reactive(adsl), ADTTE = reactive(adtte))
  key_list <- list(teal.data::get_cdisc_keys("ADSL"), teal.data::get_cdisc_keys("ADTTE"))

  testthat::expect_error(
    isolate(resolve(arm_ref_comp_ddl, data_list, key_list)),
    "Assertion on 'keys' failed: Must have names."
  )

  key_list <- list(AA = teal.data::get_cdisc_keys("ADSL"), ADTTE = teal.data::get_cdisc_keys("ADTTE"))

  testthat::expect_error(
    isolate(resolve(arm_ref_comp_ddl, data_list, key_list)),
    "Names must be a permutation of set {'ADSL','ADTTE'}, but has extra elements {'AA'}.",
    fixed = TRUE
  )
})

testthat::test_that("resolve throws error with missing arguments", {
  data_list <- list(ADSL = reactive(adsl), ADTTE = reactive(adtte))
  key_list <- list(ADSL = teal.data::get_cdisc_keys("ADSL"), ADTTE = teal.data::get_cdisc_keys("ADTTE"))

  testthat::expect_error(
    isolate(resolve(arm_ref_comp_ddl, data_list)),
    "argument \"keys\" is missing, with no default"
  )

  testthat::expect_error(
    isolate(resolve(arm_ref_comp_ddl, keys = key_list)),
    "argument \"datasets\" is missing, with no default"
  )
})
