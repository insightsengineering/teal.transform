testthat::test_that("choices_selected requires choices to be specified", {
  testthat::expect_error(choices_selected(), "\"choices\" is missing")
})

testthat::test_that("choices_selected accepts choices as atomics", {
  testthat::expect_no_error(choices_selected(choices = c("a", "b")))
  testthat::expect_no_error(choices_selected(choices = c(1, 2)))
  testthat::expect_no_error(choices_selected(choices = NULL))
  testthat::expect_error(choices_selected(choices = list(1, 2)))
})

testthat::test_that("choices_selected returns list of class choices_selected", {
  testthat::expect_identical(
    choices_selected(choices = c("a", "b")),
    structure(list(choices = c("a", "b"), selected = "a", fixed = FALSE), class = "choices_selected")
  )
})

testthat::test_that("choices_selected accepts choices as delayed_data", {
  testthat::expect_no_error(choices_selected(choices = variable_choices(data = "iris")))
  testthat::expect_no_error(choices_selected(choices = value_choices(data = "iris", var_choices = "Species")))
})

testthat::test_that("choices_selected returns delayed_choices_selected when choices are delayed_data", {
  testthat::expect_s3_class(
    choices_selected(choices = variable_choices(data = "iris")),
    c("delayed_choices_selected", "delayed_data")
  )
})

testthat::test_that("choices_selected by default sets selected as first choice", {
  testthat::expect_identical(
    choices_selected(choices = c(1, 2)),
    choices_selected(choices = c(1, 2), selected = 1)
  )
})

testthat::test_that("choices_selected throws error when selected is not found in choices", {
  testthat::expect_error(
    choices_selected(choices = c("a", "b"), selected = c("c", "d")),
    "Must be a subset of \\{'a','b'\\}"
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
