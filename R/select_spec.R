#' Column selection input specification
#'
#' @description
#'
#' `select_spec` is used inside `teal` to create a [shiny::selectInput()]
#' that will select columns from a dataset.
#'
#' @rdname select_spec
#'
#' @param choices (`character` or `delayed_data`) object.
#' Named character vector to define the choices of a shiny [shiny::selectInput()].
#' These have to be columns in the dataset defined in the [data_extract_spec()]
#' where this is called.
#' `delayed_data` objects can be created via [variable_choices()] or [value_choices()].
#' @param selected (`character` or `NULL` or `delayed_choices` or `delayed_data`) optional
#' named character vector to define the selected values of a shiny [shiny::selectInput()].
#' Passing a `delayed_choices` object defers selection until data is available.
#' Defaults to the first value of `choices` or `NULL` for delayed data loading.
#' @param multiple (`logical`) Whether multiple values shall be allowed in the
#' shiny [shiny::selectInput()].
#' @param fixed (`logical`) optional [data_extract_spec()] specific feature to
#' hide the choices selected in case they are not needed. Setting fixed to `TRUE`
#' will not allow the user to select columns. It will then lead to a selection of
#' columns in the dataset that is defined by the developer of the app.
#' @param always_selected (`character`) Additional column names from the data set that should
#' always be selected
#' @param ordered (`logical(1)`) Flags whether selection order should be tracked.
#' @param label (`character`) optional, defines a label on top of this specific
#' shiny [shiny::selectInput()]. The default value is `"Select"`.
#'
#' @return A `select_spec`-S3 class object or `delayed_select_spec`-S3-class object.
#' It contains all input values.
#'
#' If `select_spec`, then the function double checks the `choices` and `selected` inputs.
#'
#' @examples
#' # Selection with just one column allowed
#' select_spec(
#'   choices = c("AVAL", "BMRKR1", "AGE"),
#'   selected = c("AVAL"),
#'   multiple = FALSE,
#'   fixed = FALSE,
#'   label = "Column"
#' )
#'
#' # Selection with just multiple columns allowed
#' select_spec(
#'   choices = c("AVAL", "BMRKR1", "AGE"),
#'   selected = c("AVAL", "BMRKR1"),
#'   multiple = TRUE,
#'   fixed = FALSE,
#'   label = "Columns"
#' )
#'
#' # Selection without user access
#' select_spec(
#'   choices = c("AVAL", "BMRKR1"),
#'   selected = c("AVAL", "BMRKR1"),
#'   multiple = TRUE,
#'   fixed = TRUE,
#'   label = "Columns"
#' )
#'
#' # Delayed version
#' select_spec(
#'   label = "Select variable:",
#'   choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
#'   selected = "BMRKR1",
#'   multiple = FALSE,
#'   fixed = FALSE
#' )
#'
#' # delayed_choices passed to selected
#' select_spec(
#'   label = "Select variable:",
#'   choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
#'   selected = all_choices()
#' )
#'
#' # Both below objects are semantically the same
#' select_spec(choices = variable_choices("ADSL"), selected = variable_choices("ADSL"))
#' select_spec(choices = variable_choices("ADSL"), selected = all_choices())
#' @export
#'
select_spec <- function(choices,
                        selected = `if`(inherits(choices, "delayed_data"), NULL, choices[1]),
                        multiple = length(selected) > 1 || inherits(selected, "multiple_choices"),
                        fixed = FALSE,
                        always_selected = NULL,
                        ordered = FALSE,
                        label = "Select") {
  checkmate::assert_flag(multiple)
  checkmate::assert_flag(fixed)
  checkmate::assert_character(always_selected, min.len = 1, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_flag(ordered)
  checkmate::assert_string(label, null.ok = TRUE)
  stopifnot(multiple || !inherits(selected, "multiple_choices"))
  if (fixed) stopifnot(is.null(always_selected))

  if (inherits(selected, "delayed_choices")) selected <- selected(choices)
  if (inherits(choices, "delayed_data") || inherits(selected, "delayed_data")) {
    select_spec.delayed_data(choices, selected, multiple, fixed, always_selected, ordered, label)
  } else {
    select_spec.default(choices, selected, multiple, fixed, always_selected, ordered, label)
  }
}

#' @rdname select_spec
#' @export
#'
select_spec.delayed_data <- function(choices, # nolint: object_name_linter.
                                     selected = NULL,
                                     multiple = length(selected) > 1,
                                     fixed = FALSE,
                                     always_selected = NULL,
                                     ordered = FALSE,
                                     label = NULL) {
  checkmate::assert(
    checkmate::check_null(selected),
    checkmate::check_atomic(selected),
    checkmate::check_class(selected, "delayed_data")
  )
  checkmate::assert(
    checkmate::check_null(choices),
    checkmate::check_atomic(choices),
    checkmate::check_class(choices, "delayed_data")
  )

  structure(
    list(
      choices = choices,
      selected = selected,
      multiple = multiple,
      fixed = fixed,
      always_selected = always_selected,
      ordered = ordered,
      label = label
    ),
    class = c("delayed_select_spec", "delayed_data", "select_spec")
  )
}

#' @rdname select_spec
#' @export
#'
select_spec.default <- function(choices, # nolint: object_name_linter.
                                selected = choices[1],
                                multiple = length(selected) > 1,
                                fixed = FALSE,
                                always_selected = NULL,
                                ordered = FALSE,
                                label = NULL) {
  checkmate::assert(
    checkmate::check_null(choices),
    checkmate::check_atomic(choices)
  )
  checkmate::assert(
    checkmate::check_null(selected),
    checkmate::check_atomic(selected)
  )

  # if names is NULL, shiny will put strange labels (with quotes etc.) in the selectInputs, so we set it to the values
  if (is.null(names(choices))) {
    names(choices) <- as.character(choices)
  }

  # Deal with selected
  if (length(selected) > 0) {
    checkmate::assert_atomic(selected)
    checkmate::assert_subset(selected, choices)
    stopifnot(multiple || length(selected) == 1)
    if (is.null(names(selected))) {
      names(selected) <- as.character(selected)
    }
  }

  if (length(intersect(choices, always_selected)) > 0) {
    warning("You cannot allow the user to select 'always_selected' columns.
      'choices' and 'always_selected' will be intersected")
    test_c <- choices[which(!choices %in% always_selected)]
    if (length(test_c) > 0) {
      class(test_c) <- c("choices_labeled", "character")
      choices <- test_c
    } else {
      choices <- NULL
    }
  }

  structure(
    list(
      choices = choices, selected = selected, multiple = multiple, fixed = fixed,
      always_selected = always_selected, ordered = ordered, label = label
    ),
    class = "select_spec"
  )
}
