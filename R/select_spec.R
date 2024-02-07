#' Column selection input specification
#'
#' @description `r lifecycle::badge("stable")`
#' \code{select_spec} is used inside `teal` to create a \code{\link[shiny]{selectInput}}
#' that will select columns from a dataset.
#'
#' @param choices (\code{character}) or (\code{delayed_data}) object.
#'   Named character vector to define the choices
#'   of a shiny \code{\link[shiny]{selectInput}}. These have to be columns in the
#'   dataset defined in the \code{\link{data_extract_spec}} where this is called.
#'   \code{delayed_data} objects can be created via \code{\link{variable_choices}} or \code{\link{value_choices}}.
#'
#' @param selected optional (\code{character} or \code{NULL} or \code{all_choices} or \code{delayed_data} object).
#' Named character vector to define the selected values of a shiny \code{\link[shiny]{selectInput}}.
#' Passing an `all_choices()` object indicates selecting all possible choices.
#' Defaults to the first value of \code{choices} or \code{NULL} for delayed data loading.
#'
#' @param multiple (\code{logical}) Whether multiple values shall be allowed in the
#'  shiny \code{\link[shiny]{selectInput}}.
#'
#' @param fixed optional (\code{logical}). \code{\link{data_extract_spec}} specific feature to
#'   hide the choices selected in case they are not needed. Setting fixed to \code{TRUE}
#'   will not allow the user to select columns. It will then lead to a selection of
#'   columns in the dataset that is defined by the developer of the app.
#'
#' @param always_selected (\code{character}) Additional column names from the data set that should
#'   always be selected
#'
#' @param ordered (`logical(1)`) Flags whether selection order should be tracked.
#'
#' @param label optional (\code{character}). Define a label on top of this specific
#' shiny \code{\link[shiny]{selectInput}}. The default value is \code{"Select"}.
#'
#' @return A \code{select_spec}-S3 class object or \code{delayed_select_spec}-S3-class object.
#' It contains all input values.
#' If \code{select_spec}, then the function double checks the \code{choices} and \code{selected} inputs.
#'
#'
#' @rdname select_spec
#'
#' @export
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
#' # all_choices passed to selected
#' select_spec(
#'   label = "Select variable:",
#'   choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
#'   selected = all_choices()
#' )
#'
#' # Both below objects are semantically the same
#' select_spec(choices = variable_choices("ADSL"), selected = variable_choices("ADSL"))
#' select_spec(choices = variable_choices("ADSL"), selected = all_choices())
select_spec <- function(choices,
                        selected = `if`(inherits(choices, "delayed_data"), NULL, choices[1]),
                        multiple = length(selected) > 1 || inherits(selected, "all_choices"),
                        fixed = FALSE,
                        always_selected = NULL,
                        ordered = FALSE,
                        label = "Select") {
  checkmate::assert_flag(multiple)
  checkmate::assert_flag(fixed)
  checkmate::assert_character(always_selected, min.len = 1, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_flag(ordered)
  checkmate::assert_string(label, null.ok = TRUE)
  stopifnot(multiple || !inherits(selected, "all_choices"))
  if (fixed) stopifnot(is.null(always_selected))

  if (inherits(selected, "all_choices")) selected <- choices
  if (inherits(choices, "delayed_data") || inherits(selected, "delayed_data")) {
    select_spec.delayed_data(choices, selected, multiple, fixed, always_selected, ordered, label)
  } else {
    select_spec.default(choices, selected, multiple, fixed, always_selected, ordered, label)
  }
}

#' @rdname select_spec
#' @export
select_spec.delayed_data <- function(choices, # nolint
                                     selected = NULL,
                                     multiple = length(selected) > 1,
                                     fixed = FALSE,
                                     always_selected = NULL,
                                     ordered = FALSE,
                                     label = NULL) {
  stopifnot(is.null(selected) || is.atomic(selected) || inherits(selected, "delayed_data"))
  stopifnot(is.null(choices) || is.atomic(choices) || inherits(choices, "delayed_data"))

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
select_spec.default <- function(choices, # nolint
                                selected = choices[1],
                                multiple = length(selected) > 1,
                                fixed = FALSE,
                                always_selected = NULL,
                                ordered = FALSE,
                                label = NULL) {
  stopifnot(is.null(choices) || is.atomic(choices))
  stopifnot(is.null(selected) || is.atomic(selected))

  # if names is NULL, shiny will put strange labels (with quotes etc.) in the selectInputs, so we set it to the values
  if (is.null(names(choices))) {
    names(choices) <- as.character(choices)
  }

  # Deal with selected
  if (length(selected) > 0) {
    stopifnot(is.atomic(selected))
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

  res <- list(
    choices = choices, selected = selected, multiple = multiple, fixed = fixed,
    always_selected = always_selected, ordered = ordered, label = label
  )
  class(res) <- "select_spec"

  return(res)
}
