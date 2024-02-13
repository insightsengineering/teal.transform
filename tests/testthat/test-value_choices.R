ADSL <- teal.transform::rADSL
ADTTE <- teal.transform::rADTTE
data_list <- list(ADSL = reactive(ADSL), ADTTE = reactive(ADTTE))
primary_keys_list <- list(ADSL = c("STUDYID", "USUBJID"), ADTTE = c("STUDYID", "USUBJID", "PARAMCD"))

testthat::test_that("Will output warnings when value_choices applied on datasets with missing values and / or labels", {
  data <- data.frame(
    A = c(1, 2, 3),
    B = c(NA, "a", "b"),
    C = rep(NA, 3),
    D = factor(c("x", "y", NA), levels = c("x", "y", "NA")),
    F = c(NA, "NA", "M"),
    G = c(1, 2, NA),
    H = c(TRUE, NA, FALSE),
    I = rep(TRUE, 3),
    J = c("NA", "a", "b")
  )
  testthat::expect_warning(value_choices(data, var_choices = c("F")))
  testthat::expect_warning(value_choices(data, var_choices = c("D")))
  testthat::expect_warning(value_choices(data, var_choices = c("F", "D")))
  testthat::expect_warning(value_choices(data, var_choices = c("A", "D")))
  testthat::expect_warning(value_choices(data, var_choices = c("A", "F")))
  testthat::expect_error(value_choices(data, var_choices = "K"))
  testthat::expect_error(value_choices(data, var_choices = "F", var_label = "K"))
  testthat::expect_no_warning(value_choices(data, var_choices = c("J")))
  testthat::expect_no_warning(value_choices(data, var_choices = c("B")))
})

testthat::test_that("delayed version of value_choices", {
  # hard-coded subset
  obj <- value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B"))
  testthat::expect_equal(
    obj,
    structure(
      list(
        data = "ADSL",
        var_choices = "ARMCD",
        var_label = "ARM",
        subset = c("ARM A", "ARM B"),
        sep = " - "
      ),
      class = c("delayed_value_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve(obj, datasets = data_list, keys = primary_keys_list))
  testthat::expect_equal(
    res_obj,
    value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B"))
  )

  # functional subset
  obj <- value_choices(
    "ADSL",
    var_choices = "ARMCD",
    var_label = "ARM",
    subset = function(data) {
      levels(data$ARMCD)[1:2]
    }
  )
  testthat::expect_equal(
    obj,
    structure(
      list(
        data = "ADSL",
        var_choices = "ARMCD",
        var_label = "ARM",
        subset = function(data) {
          levels(data$ARMCD)[1:2]
        },
        sep = " - "
      ),
      class = c("delayed_value_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve(obj, datasets = data_list, keys = primary_keys_list))
  testthat::expect_equal(
    res_obj,
    value_choices(ADSL,
      var_choices = "ARMCD", var_label = "ARM",
      subset = function(data) {
        levels(data$ARMCD)[1:2]
      }
    )
  )

  # functional subset with multiple columns
  combine_armcd_bmrkr2 <- function(data) {
    apply(
      expand.grid(levels(data$ARMCD)[1:2], levels(data$BMRKR2), stringsAsFactors = FALSE),
      1,
      paste,
      collapse = " - "
    )
  }

  obj <- value_choices(
    "ADSL",
    var_choices = c("ARMCD", "BMRKR2"),
    var_label = c("ARM", "BMRKR2"),
    subset = combine_armcd_bmrkr2
  )
  testthat::expect_equal(
    obj,
    structure(
      list(
        data = "ADSL",
        var_choices = c("ARMCD", "BMRKR2"),
        var_label = c("ARM", "BMRKR2"),
        subset = combine_armcd_bmrkr2, sep = " - "
      ),
      class = c("delayed_value_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve(obj, datasets = data_list, keys = primary_keys_list))
  testthat::expect_equal(
    res_obj,
    value_choices(ADSL,
      var_choices = c("ARMCD", "BMRKR2"), var_label = c("ARM", "BMRKR2"),
      subset = combine_armcd_bmrkr2
    )
  )
})


# With resolve_delayed


testthat::test_that("delayed version of value_choices - resolve_delayed", {
  # hard-coded subset
  obj <- value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B"))
  testthat::expect_equal(
    obj,
    structure(
      list(
        data = "ADSL",
        var_choices = "ARMCD",
        var_label = "ARM",
        subset = c("ARM A", "ARM B"),
        sep = " - "
      ),
      class = c("delayed_value_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = data_list, keys = primary_keys_list))
  testthat::expect_equal(
    res_obj,
    value_choices(ADSL, var_choices = "ARMCD", var_label = "ARM", subset = c("ARM A", "ARM B"))
  )


  # functional subset
  obj <- value_choices(
    "ADSL",
    var_choices = "ARMCD",
    var_label = "ARM",
    subset = function(data) {
      levels(data$ARMCD)[1:2]
    }
  )
  testthat::expect_equal(
    obj,
    structure(
      list(
        data = "ADSL",
        var_choices = "ARMCD",
        var_label = "ARM",
        subset = function(data) {
          levels(data$ARMCD)[1:2]
        },
        sep = " - "
      ),
      class = c("delayed_value_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = data_list, keys = primary_keys_list))
  testthat::expect_equal(
    res_obj,
    value_choices(ADSL,
      var_choices = "ARMCD", var_label = "ARM",
      subset = function(data) {
        levels(data$ARMCD)[1:2]
      }
    )
  )


  # functional subset with multiple columns
  combine_armcd_bmrkr2 <- function(data) {
    apply(
      expand.grid(levels(data$ARMCD)[1:2], levels(data$BMRKR2), stringsAsFactors = FALSE),
      1,
      paste,
      collapse = " - "
    )
  }

  obj <- value_choices(
    "ADSL",
    var_choices = c("ARMCD", "BMRKR2"),
    var_label = c("ARM", "BMRKR2"),
    subset = combine_armcd_bmrkr2
  )
  testthat::expect_equal(
    obj,
    structure(
      list(
        data = "ADSL",
        var_choices = c("ARMCD", "BMRKR2"),
        var_label = c("ARM", "BMRKR2"),
        subset = combine_armcd_bmrkr2, sep = " - "
      ),
      class = c("delayed_value_choices", "delayed_data", "choices_labeled")
    )
  )

  res_obj <- isolate(resolve_delayed(obj, datasets = data_list, keys = primary_keys_list))
  testthat::expect_equal(
    res_obj,
    value_choices(ADSL,
      var_choices = c("ARMCD", "BMRKR2"), var_label = c("ARM", "BMRKR2"),
      subset = combine_armcd_bmrkr2
    )
  )
})
