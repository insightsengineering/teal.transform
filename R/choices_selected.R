no_select_keyword <- "-- no selection --"

#' Choices selected
#'
#' @description
#'
#' Construct a single list containing available choices, the default selected value, and
#' additional settings such as to order the choices with the selected elements appearing first
#' or whether to block the user from making selections.
#'
#' Can be used in UI input elements such as [teal.widgets::optionalSelectInput()].
#'
#' @details
#' Please note that the order of selected will always follow the order of choices. The `keep_order`
#' argument is set to false which will run the following code inside:
#'
#' ```
#' choices <- c(selected, setdiff(choices, selected))
#' ```
#'
#' In case you want to keep your specific order of choices, set `keep_order` to `TRUE`.
#'
#' @param choices (`character`) vector of possible choices or `delayed_data` object.
#'
#' See [variable_choices()] and [value_choices()].
#' @param selected (`character`) vector of preselected options, (`delayed_choices`) object
#' or (`delayed_data`) object.
#'
#' If `delayed_data` object then `choices` must also be `delayed_data` object.
#' If not supplied it will default to the first element of `choices` if
#' `choices` is a vector, or `NULL` if `choices` is a `delayed_data` object.
#' @param keep_order (`logical`) In case of `FALSE` the selected variables will
#' be on top of the drop-down field.
#' @param fixed (`logical`) optional, whether to block user to select choices.
#'
#' @return `choices_selected` returns list of `choices_selected`, encapsulating the specified
#' `choices`, `selected`, `keep_order` and `fixed`.
#'
#' @examples
#' library(shiny)
#' library(teal.widgets)
#'
#' ADSL <- teal.data::rADSL
#' choices_selected(variable_choices(ADSL), "SEX")
#'
#' # How to select nothing
#' # use an empty character
#' choices_selected(
#'   choices = c("", "A", "B", "C"),
#'   selected = ""
#' )
#'
#' # How to allow the user to select nothing
#' # use an empty character
#' choices_selected(
#'   choices = c("A", "", "B", "C"),
#'   selected = "A"
#' )
#'
#'
#' # How to make Nothing the Xth choice
#' # just use keep_order
#' choices_selected(
#'   choices = c("A", "", "B", "C"),
#'   selected = "A",
#'   keep_order = TRUE
#' )
#'
#'
#' # How to give labels to selections
#' # by adding names - choices will be replaced by "name" in UI, not in code
#' choices_selected(
#'   choices = c("name for A" = "A", "Name for nothing" = "", "name for b" = "B", "name for C" = "C"),
#'   selected = "A"
#' )
#'
#' # by using choices_labeled
#' # labels will be shown behind the choice
#' choices_selected(
#'   choices = choices_labeled(
#'     c("A", "", "B", "C"),
#'     c("name for A", "nothing", "name for B", "name for C")
#'   ),
#'   selected = "A"
#' )
#'
#' # Passing a `delayed_data` object to `selected`
#' choices_selected(
#'   choices = variable_choices("ADSL"),
#'   selected = variable_choices("ADSL", subset = c("STUDYID"))
#' )
#'
#' # Passing `delayed_choices` object - semantically identical objects:
#' choices_selected(choices = letters, selected = letters)
#' choices_selected(choices = letters, selected = all_choices())
#'
#' choices_selected(
#'   choices = setNames(LETTERS[1:5], paste("Letter", LETTERS[1:5])),
#'   selected = "E"
#' )
#' choices_selected(
#'   choices = setNames(LETTERS[1:5], paste("Letter", LETTERS[1:5])),
#'   selected = last_choice()
#' )
#'
#' # functional form (subsetting for factor variables only) of choices_selected
#' # with delayed data loading
#' choices_selected(variable_choices("ADSL", subset = function(data) {
#'   idx <- vapply(data, is.factor, logical(1))
#'   names(data)[idx]
#' }))
#'
#' cs <- choices_selected(
#'   choices = c("A", "B", "C"),
#'   selected = "A"
#' )
#'
#' ui <- bslib::page_fluid(
#'   optionalSelectInput(
#'     inputId = "id",
#'     choices = cs$choices,
#'     selected = cs$selected
#'   )
#' )
#'
#' server <- function(input, output, session) {}
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#' @export
#'
choices_selected <- function(choices,
                             selected = if (inherits(choices, "delayed_data")) NULL else choices[1],
                             keep_order = FALSE,
                             fixed = FALSE) {
  checkmate::assert(
    checkmate::check_atomic(choices),
    checkmate::check_class(choices, "delayed_data")
  )
  checkmate::assert(
    checkmate::check_atomic(selected),
    checkmate::check_multi_class(selected, c("delayed_data", "delayed_choices"))
  )
  checkmate::assert_flag(keep_order)
  checkmate::assert_flag(fixed)

  if (inherits(selected, "delayed_choices")) selected <- selected(choices)

  if (inherits(selected, "delayed_data") && !inherits(choices, "delayed_data")) {
    stop("If 'selected' is of class 'delayed_data', so must be 'choices'.")
  }

  if (inherits(choices, "delayed_data")) {
    return(
      structure(
        list(choices = choices, selected = selected, keep_order = keep_order, fixed = fixed),
        class = c("delayed_choices_selected", "delayed_data", "choices_selected")
      )
    )
  }

  if (!is.null(choices) && no_select_keyword %in% choices) {
    stop(paste(no_select_keyword, "is not a valid choice as it is used as a keyword"))
  }

  # remove duplicates
  choices <- vector_remove_dups(choices)
  selected <- vector_remove_dups(selected)
  checkmate::assert_subset(selected, choices)

  if (!keep_order && length(choices) > 0) {
    choices_in_selected <- which(choices %in% selected)
    choices <- vector_reorder(
      choices,
      c(choices_in_selected, setdiff(seq_along(choices), choices_in_selected))
    )
  }

  structure(
    list(
      choices = choices,
      selected = selected,
      fixed = fixed
    ),
    class = "choices_selected"
  )
}

#' @describeIn choices_selected Check if an object is a choices_selected class
#'
#' @param x (`choices_selected`) object to check.
#'
#' @return `is.choices_selected` returns `TRUE` if `x` inherits from a `choices_selected` object, `FALSE` otherwise.
#'
#' @export
#'
is.choices_selected <- function(x) { # nolint: object_name_linter.
  inherits(x, "choices_selected")
}

#' Add empty choice to choices selected
#'
#' @param x (`choices_selected`) object.
#' @param multiple (`logical(1)`) whether multiple selections are allowed or not.
#'
#' @return `choices_selected` object with an empty option added to the choices.
#'
#' @export
#'
add_no_selected_choices <- function(x, multiple = FALSE) {
  if (is.null(x)) {
    choices_selected(NULL)
  } else {
    stopifnot(is.choices_selected(x))

    if (!multiple) {
      x$choices <- c(no_select_keyword, x$choices)
      if (is.null(x$selected)) x$selected <- no_select_keyword
    }

    x
  }
}

#' Check select choices for no choice made
#'
#' @param x (`character`) Word that shall be checked for `NULL`, empty, "--no-selection".
#'
#' @return The word or `NULL`.
#'
#' @export
#'
no_selected_as_NULL <- function(x) { # nolint: object_name_linter.
  if (is.null(x) || identical(x, no_select_keyword) || x == "") {
    NULL
  } else {
    x
  }
}

## Non-exported utils functions ----
#' Modify vectors and keep attributes
#' @keywords internal
#' @noRd
#'
vector_reorder <- function(vec, idx) {
  checkmate::assert_atomic(vec)
  checkmate::assert_integer(idx, min.len = 1, lower = 1, any.missing = FALSE)
  stopifnot(length(vec) == length(idx))

  vec_attrs <- attributes(vec)

  vec <- vec[idx]

  for (vec_attrs_idx in seq_along(vec_attrs)) {
    if (length(vec_attrs[[vec_attrs_idx]]) == length(vec)) {
      vec_attrs[[vec_attrs_idx]] <- vec_attrs[[vec_attrs_idx]][idx]
    }
  }

  attributes(vec) <- vec_attrs
  vec
}

#' Remove item(s) and their attributes from vector
#' @keywords internal
#' @noRd
#'
vector_pop <- function(vec, idx) {
  checkmate::assert_atomic(vec)
  checkmate::assert_integer(idx, lower = 1, any.missing = FALSE)

  if (length(idx) == 0) {
    return(vec)
  }

  vec_attrs <- attributes(vec)
  names_vec_attrs <- names(vec_attrs)

  for (vec_attrs_idx in seq_along(vec_attrs)) {
    if (length(vec_attrs[[vec_attrs_idx]]) == length(vec) && names_vec_attrs[vec_attrs_idx] != "class") {
      vec_attrs[[vec_attrs_idx]] <- vec_attrs[[vec_attrs_idx]][-idx]
    }
  }

  vec <- vec[-idx]
  attributes(vec) <- vec_attrs
  vec
}

#' Remove duplicate elements or elements with the same name from a vector
#' @keywords internal
#' @noRd
#'
vector_remove_dups <- function(vec) {
  checkmate::assert_atomic(vec)

  idx <- which(duplicated(vec))

  if (length(idx) == 0) {
    vec
  } else if (is.null(attributes(vec))) {
    unique(vec)
  } else if (identical(names(attributes(vec)), "names")) {
    vec[-idx]
  } else {
    vector_pop(vec, idx)
  }
}
