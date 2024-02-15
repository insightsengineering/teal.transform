#' Data extract filter specification
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' It consists in choices and additionally the variable names for the choices.
#'
#' @details
#' The `filter_spec` is used inside `teal` apps to allow filtering datasets
#' for their key variables. Imagine having an adverse events table. It has
#' the columns `PARAMCD` and `CNSR`. `PARAMCD` contains the levels
#' `"OS"`, `"PFS"`, `"EFS"`. `CNSR` contains the levels `"0"` and `"1"`.
#' The first example should show how a `filter_spec` setup will influence
#' the drop-down menu the app user will see.
#'
#' @inheritParams select_spec
#' @param vars (`character` or `delayed_data`) object.
#' Character vector giving the columns to be filtered. These should be
#' key variables of the data set to be filtered.
#' `delayed_data` objects can be created via [variable_choices()], [value_choices()],
#' or [choices_selected()].
#' @param sep (`character`) A separator string to split the `choices` or
#' `selected` inputs into the values of the different columns.
#' @param choices (`character` or `numeric` or `logical` or (`delayed_data`) object.
#' Named character vector to define the choices of a shiny [shiny::selectInput()].
#' These choices will be used to filter the dataset.
#'
#' These shall be filter values of the `vars` input separated by the separator(`sep`). Please
#' watch out that the filter values have to follow the order of the `vars` input. In the following
#' example we will show how to filter two columns:
#'
#' `vars = c("PARAMCD","AVISIT")` and `choices = c("CRP - BASELINE", "ALT - BASELINE")`
#' will lead to a filtering of
#' `(PARAMCD == "CRP" & AVISIT == "BASELINE") | (PARAMCD == "ALT" & AVISIT == "BASELINE")`.
#'
#' The `sep` input has to be `" - "` in this case.
#'
#' `delayed_data` objects can be created via [variable_choices()] or [value_choices()].
#' @param selected (`character` or `numeric` or `logical` or (`delayed_data` or `all_choices`) object.
#' Named character vector to define the selected values of a shiny [shiny::selectInput()]
#' (default values).
#' This value will be displayed inside the shiny app upon start.
#' The `all_choices` object indicates selecting all possible choices.
#' @param drop_keys (optional `logical`) whether to drop filter column from the
#' dataset keys, `TRUE` on default.
#' @param label (optional `character`). Define a label on top of this specific
#' shiny [shiny::selectInput()]. The default value is `"Filter by"`.
#'
#' @return `filter_spec`-S3-class object or `delayed_filter_spec`-S3-class object.
#'
#' @examples
#' # for Adverse Events table
#' filter_spec(
#'   vars = c("PARAMCD", "CNSR"),
#'   sep = "-",
#'   choices = c("OS-1" = "OS-1", "OS-0" = "OS-0", "PFS-1" = "PFS-1"),
#'   selected = "OS-1",
#'   multiple = FALSE,
#'   label = "Choose endpoint and Censor"
#' )
#'
#' # filtering a single variable
#' filter_spec(
#'   vars = c("PARAMCD"),
#'   sep = "-",
#'   choices = c("OS", "PFS", "EFS"),
#'   selected = "OS",
#'   multiple = FALSE,
#'   label = "Choose endpoint"
#' )
#'
#' # filtering a single variable by multiple levels of the variable
#' filter_spec(
#'   vars = c("PARAMCD"),
#'   sep = "-",
#'   choices = c("OS", "PFS", "EFS"),
#'   selected = c("OS", "PFS"),
#'   multiple = TRUE,
#'   label = "Choose endpoint"
#' )
#'
#' # delayed version
#' filter_spec(
#'   vars = variable_choices("ADSL", "SEX"),
#'   sep = "-",
#'   choices = value_choices("ADSL", "SEX", "SEX"),
#'   selected = "F",
#'   multiple = FALSE,
#'   label = "Choose endpoint and Censor"
#' )
#' # using `choices_selected()`
#' filter_spec(
#'   vars = choices_selected(variable_choices("ADSL", subset = c("SEX", "AGE")), "SEX", fixed = FALSE),
#'   multiple = TRUE
#' )
#'
#' filter_spec(
#'   vars = choices_selected(variable_choices("ADSL"), "SEX", fixed = TRUE),
#'   multiple = TRUE
#' )
#'
#' # choose all choices
#' adsl_filter <- filter_spec(
#'   vars = choices_selected(variable_choices("ADSL"), "SEX", fixed = FALSE),
#'   choices = value_choices("ADSL", "SEX"),
#'   selected = all_choices()
#' )
#' @export
#'
filter_spec <- function(vars,
                        choices = NULL,
                        selected = `if`(inherits(choices, "delayed_data"), NULL, choices[1]),
                        multiple = length(selected) > 1 || inherits(selected, "all_choices"),
                        label = "Filter by",
                        sep = attr(choices, "sep"),
                        drop_keys = FALSE) {
  if (is.null(sep)) sep <- " - "
  checkmate::assert(
    checkmate::check_character(vars, min.len = 1, any.missing = FALSE),
    checkmate::check_class(vars, "delayed_data"),
    checkmate::check_class(vars, "choices_selected")
  )
  checkmate::assert(
    checkmate::check_null(choices),
    checkmate::check_character(choices, min.len = 1, any.missing = FALSE),
    checkmate::check_numeric(choices, min.len = 1, any.missing = FALSE),
    checkmate::check_logical(choices, min.len = 1, any.missing = FALSE),
    checkmate::check_class(choices, "delayed_data")
  )
  checkmate::assert(
    checkmate::check_null(selected),
    checkmate::check_character(selected, min.len = 1, any.missing = FALSE),
    checkmate::check_numeric(selected, min.len = 1, any.missing = FALSE),
    checkmate::check_logical(selected, min.len = 1, any.missing = FALSE),
    checkmate::check_class(selected, "delayed_data"),
    checkmate::check_class(selected, "all_choices")
  )

  checkmate::assert_flag(multiple)
  checkmate::assert_string(label, null.ok = TRUE)
  checkmate::assert_string(sep)
  checkmate::assert_flag(drop_keys)
  stopifnot(multiple || !inherits(selected, "all_choices"))

  if (inherits(selected, "all_choices") && !is.null(choices)) selected <- choices

  if (inherits(vars, "choices_selected")) {
    filter_spec_internal(
      vars_choices = vars$choices,
      vars_selected = vars$selected,
      vars_label = if (vars$fixed) NULL else label,
      vars_fixed = vars$fixed,
      vars_multiple = if (is.null(vars$selected)) FALSE else length(vars$selected) > 1,
      choices = choices,
      selected = selected,
      label = if (vars$fixed) label else NULL,
      fixed = FALSE,
      multiple = multiple,
      sep = sep,
      drop_keys = drop_keys
    )
  } else {
    filter_spec_internal(
      vars_choices = vars,
      vars_selected = vars,
      vars_label = NULL,
      vars_fixed = TRUE,
      vars_multiple = TRUE,
      choices = choices,
      selected = selected,
      label = label,
      fixed = FALSE,
      multiple = multiple,
      sep = sep,
      drop_keys = drop_keys
    )
  }
}


#' Data extract dynamic filter specification
#'
#' Builds a configuration for the `data_extract_ui` module. This function covers
#' the configuration of filtering datasets (so called `filter_spec`), which then
#' is used to build the UI element in the `teal` app.
#'
#' @inheritParams filter_spec
#' @param vars_choices (`character` or `delayed_data`)
#' the vector of dataset column names available to build dynamic filter
#' `delayed_data` objects can be created via [variable_choices()].
#' @param vars_selected (`NULL` or named `character`)
#' the selected column name out from `choices`.
#' @param vars_label (`character`)
#' the title printed on the UI element generated on the basis of this `filter_spec`.
#' @param vars_fixed (`logical`)
#' if true allow to change the selected variables in the UI element; otherwise, do not allow.
#' @param vars_multiple (`logical`)
#' if true allow to select multiple variables in the UI elements; otherwise, do not allow.
#' @param fixed (`logical`)
#' if true allow to change the initially selected values of the variables; otherwise, do not allow.
#' @param dataname (`character`)
#' the name of the dataset this filter covers. Set during the initialization of the `teal` application.
#' @param initialized (`logical`)
#' indicates whether this filter was already initialized in the application.
#' TRUE if this filter was already consumed by the server function; FALSE otherwise.
#'
#' @return `filter_spec` or `delayed_filter_spec` S3-class object.
#'
#' @seealso filter_spec
#'
#' @keywords internal
#'
filter_spec_internal <- function(vars_choices,
                                 vars_selected = NULL,
                                 vars_label = NULL,
                                 vars_fixed = FALSE,
                                 vars_multiple = TRUE,
                                 choices = NULL,
                                 selected = NULL,
                                 label = NULL,
                                 fixed = FALSE,
                                 multiple = TRUE,
                                 sep = attr(vars_choices, "sep"),
                                 drop_keys = FALSE,
                                 dataname = NULL,
                                 initialized = FALSE) {
  if (is.null(sep)) sep <- " - "
  checkmate::assert_string(vars_label, null.ok = TRUE)
  checkmate::assert_flag(vars_fixed)
  checkmate::assert_flag(vars_multiple)
  checkmate::assert_string(label, null.ok = TRUE)
  checkmate::assert_flag(fixed)
  checkmate::assert_flag(multiple)
  checkmate::assert_string(sep)
  checkmate::assert_flag(drop_keys)

  if (
    inherits(vars_choices, "delayed_data") ||
      inherits(vars_selected, "delayed_data") ||
      inherits(choices, "delayed_data") ||
      inherits(selected, "delayed_data")
  ) {
    filter_spec_internal.delayed_data(
      vars_choices = vars_choices,
      vars_selected = vars_selected,
      vars_label = vars_label,
      vars_fixed = vars_fixed,
      vars_multiple = vars_multiple,
      choices = choices,
      selected = selected,
      label = label,
      multiple = multiple,
      fixed = fixed,
      sep = sep,
      drop_keys = drop_keys,
      dataname = dataname,
      initialized = initialized
    )
  } else {
    UseMethod("filter_spec_internal")
  }
}

#' @rdname filter_spec_internal
#' @export
filter_spec_internal.delayed_data <- function(vars_choices,
                                              vars_selected = NULL,
                                              vars_label = NULL,
                                              vars_fixed = FALSE,
                                              vars_multiple = TRUE,
                                              choices = NULL,
                                              selected = NULL,
                                              label = NULL,
                                              fixed = FALSE,
                                              multiple = TRUE,
                                              sep = attr(vars_choices, "sep"),
                                              drop_keys = FALSE,
                                              dataname = NULL,
                                              initialized = FALSE) {
  if (is.null(sep)) sep <- " - "
  checkmate::assert(
    checkmate::check_character(vars_choices, min.len = 1, any.missing = FALSE),
    checkmate::check_numeric(vars_choices, min.len = 1, any.missing = FALSE),
    checkmate::check_logical(vars_choices, min.len = 1, any.missing = FALSE),
    checkmate::check_class(vars_choices, "delayed_data")
  )

  checkmate::assert(
    checkmate::check_null(vars_selected),
    checkmate::check_character(vars_selected, min.len = 1, any.missing = FALSE),
    checkmate::check_numeric(vars_selected, min.len = 1, any.missing = FALSE),
    checkmate::check_logical(vars_selected, min.len = 1, any.missing = FALSE),
    checkmate::check_class(vars_selected, "delayed_data")
  )

  checkmate::assert(
    checkmate::check_null(choices),
    checkmate::check_character(choices, min.len = 1, any.missing = FALSE),
    checkmate::check_numeric(choices, min.len = 1, any.missing = FALSE),
    checkmate::check_logical(choices, min.len = 1, any.missing = FALSE),
    checkmate::check_class(choices, "delayed_data")
  )

  checkmate::assert(
    checkmate::check_null(selected),
    checkmate::check_character(selected, min.len = 1, any.missing = FALSE),
    checkmate::check_numeric(selected, min.len = 1, any.missing = FALSE),
    checkmate::check_logical(selected, min.len = 1, any.missing = FALSE),
    checkmate::check_class(selected, "delayed_data"),
    checkmate::check_class(selected, "all_choices")
  )

  structure(
    list(
      vars_choices = vars_choices,
      vars_selected = vars_selected,
      vars_label = vars_label,
      vars_fixed = vars_fixed,
      vars_multiple = vars_multiple,
      choices = choices,
      selected = selected,
      label = label,
      multiple = multiple,
      fixed = fixed,
      sep = sep,
      drop_keys = drop_keys,
      dataname = dataname, # modified by data_extract_spec,
      initialized = initialized
    ),
    class = c(
      "delayed_filter_spec",
      "filter_spec",
      "delayed_data"
    )
  )
}

#' @rdname filter_spec_internal
#' @export
filter_spec_internal.default <- function(vars_choices,
                                         vars_selected = NULL,
                                         vars_label = NULL,
                                         vars_fixed = FALSE,
                                         vars_multiple = TRUE,
                                         choices = NULL,
                                         selected = NULL,
                                         label = NULL,
                                         fixed = FALSE,
                                         multiple = TRUE,
                                         sep = attr(vars_choices, "sep"),
                                         drop_keys = FALSE,
                                         dataname = NULL,
                                         initialized = FALSE) {
  if (is.null(sep)) sep <- " - "
  checkmate::assert(
    checkmate::check_character(vars_choices, min.len = 1, any.missing = FALSE),
    checkmate::check_numeric(vars_choices, min.len = 1, any.missing = FALSE),
    checkmate::check_logical(vars_choices, min.len = 1, any.missing = FALSE)
  )
  checkmate::assert_vector(vars_choices, unique = TRUE)

  if (!is.null(vars_selected)) {
    stopifnot(vars_multiple || length(vars_selected) == 1)
    checkmate::assert(
      checkmate::check_character(vars_selected, min.len = 1, any.missing = FALSE),
      checkmate::check_numeric(vars_selected, min.len = 1, any.missing = FALSE),
      checkmate::check_logical(vars_selected, min.len = 1, any.missing = FALSE)
    )
    checkmate::assert_vector(vars_selected, unique = TRUE)
    checkmate::assert_subset(vars_selected, vars_choices)
  }

  if (!is.null(choices)) {
    checkmate::assert_vector(choices, unique = TRUE)
    split_choices <- split_by_sep(choices, sep)
    stopifnot(all(vapply(split_choices, length, integer(1)) == length(vars_selected)))
  }

  if (!is.null(selected) && !inherits(selected, "all_choices")) {
    stopifnot(multiple || length(selected) == 1)
    checkmate::assert(
      checkmate::check_character(selected, min.len = 1, any.missing = FALSE),
      checkmate::check_numeric(selected, min.len = 1, any.missing = FALSE),
      checkmate::check_logical(selected, min.len = 1, any.missing = FALSE)
    )
    checkmate::assert_vector(selected, unique = TRUE)
    checkmate::assert_subset(selected, choices)
  }

  structure(
    list(
      vars_choices = vars_choices,
      vars_selected = vars_selected,
      vars_label = vars_label,
      vars_fixed = vars_fixed,
      vars_multiple = vars_multiple,
      choices = choices,
      selected = selected,
      label = label,
      multiple = multiple,
      fixed = fixed,
      sep = sep,
      drop_keys = drop_keys,
      dataname = dataname, # modified by data_extract_spec
      initialized = initialized
    ),
    class = "filter_spec"
  )
}
