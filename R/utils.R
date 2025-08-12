#' Split by separator (matched exactly)
#'
#' @param x (`character`) Character vector, each element of which is to be split.
#' Other inputs, including a factor return themselves.
#' @param sep (`character`) separator to use for splitting.
#'
#' @return List of character vectors split by `sep`. Self if `x` is not a `character`.
#'
#' @export
#'
split_by_sep <- function(x, sep) {
  checkmate::assert_atomic(x)
  if (is.character(x)) {
    strsplit(x, sep, fixed = TRUE)
  } else {
    x
  }
}

#' Extract labels from choices basing on attributes and names
#'
#' @param choices (`list` or `vector`) select choices.
#' @param values (`list` or `vector`) optional, with subset of `choices` for which
#' labels should be extracted, `NULL` for all choices.
#'
#' @return `character` vector with labels.
#'
#' @keywords internal
#'
extract_choices_labels <- function(choices, values = NULL) {
  res <- if (inherits(choices, "choices_labeled")) {
    attr(choices, "raw_labels")
  } else if (!is.null(names(choices)) && !setequal(names(choices), unlist(unname(choices)))) {
    names(choices)
  } else {
    NULL
  }

  if (!is.null(values) && !is.null(res)) {
    stopifnot(all(values %in% choices))
    res <- res[vapply(values, function(val) which(val == choices), numeric(1))]
  }

  res
}

#' Function to compose `validators` from `data_extract_multiple_srv`
#'
#' This function takes the output from `data_extract_multiple_srv` and
#' collates the `shinyvalidate::InputValidator`s returned into a single
#' `validator` and enables this.
#'
#' @param iv (`shinyvalidate::InputValidator`) A `validator`.
#' @param selector_list (`reactive` named list of `reactives`).
#' Typically this is the output from `data_extract_multiple_srv`.
#' The `validators` in this list (specifically `selector_list()[[validator_names]]()iv`)
#' will be added into `iv`.
#' @param validator_names (`character` or `NULL`). If `character` then only `validators`
#' in the elements of `selector_list()` whose name is in this list will be added. If `NULL`
#' all `validators` will be added
#'
#' @return (`shinyvalidate::InputValidator`) enabled `iv` with appropriate `validators` added into it.
#'
#' @examples
#' library(shiny)
#' library(shinyvalidate)
#' library(shinyjs)
#' library(teal.widgets)
#'
#' iris_extract <- data_extract_spec(
#'   dataname = "iris",
#'   select = select_spec(
#'     label = "Select variable:",
#'     choices = variable_choices(iris, colnames(iris)),
#'     selected = "Sepal.Length",
#'     multiple = TRUE,
#'     fixed = FALSE
#'   )
#' )
#'
#' data_list <- list(iris = reactive(iris))
#'
#' ui <- bslib::page_fluid(
#'   useShinyjs(),
#'   bslib::layout_sidebar(
#'     verbatimTextOutput("out1"),
#'     sidebar = tagList(
#'       data_extract_ui(
#'         id = "x_var",
#'         label = "Please select an X column",
#'         data_extract_spec = iris_extract
#'       ),
#'       data_extract_ui(
#'         id = "y_var",
#'         label = "Please select a Y column",
#'         data_extract_spec = iris_extract
#'       ),
#'       data_extract_ui(
#'         id = "col_var",
#'         label = "Please select a color column",
#'         data_extract_spec = iris_extract
#'       )
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   selector_list <- data_extract_multiple_srv(
#'     list(x_var = iris_extract, y_var = iris_extract, col_var = iris_extract),
#'     datasets = data_list,
#'     select_validation_rule = list(
#'       x_var = sv_required("Please select an X column"),
#'       y_var = compose_rules(
#'         sv_required("Exactly 2 'Y' column variables must be chosen"),
#'         function(x) if (length(x) != 2) "Exactly 2 'Y' column variables must be chosen"
#'       )
#'     )
#'   )
#'   iv_r <- reactive({
#'     iv <- InputValidator$new()
#'     compose_and_enable_validators(
#'       iv,
#'       selector_list,
#'       # if validator_names = NULL then all validators are used
#'       # to turn on only "x_var" then set this argument to "x_var"
#'       validator_names = NULL
#'     )
#'   })
#'
#'   output$out1 <- renderPrint({
#'     if (iv_r()$is_valid()) {
#'       ans <- lapply(selector_list(), function(x) {
#'         cat(format_data_extract(x()), "\n\n")
#'       })
#'     } else {
#'       "Check that you have made a valid selection"
#'     }
#'   })
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#' @export
#'
compose_and_enable_validators <- function(iv, selector_list, validator_names = NULL) {
  if (is.null(validator_names)) {
    validator_names <- names(selector_list())
  }
  valid_validator_names <- intersect(validator_names, names(selector_list()))

  for (validator_name in valid_validator_names) {
    single_des <- selector_list()[[validator_name]]()
    if (!is.null(single_des$iv)) {
      iv$add_validator(single_des$iv)
    }
  }
  iv$enable()
  iv
}

#' Ensures datasets is a list of reactive expression
#'
#' @param datasets (`reactive` or `teal_data` or `list`) of `data.frame`
#' wrapped or not in a reactive expression.
#'
#' @return List of `reactive` expressions that contains all the individual `datasets`.
#'
#' @keywords internal
#'
convert_teal_data <- function(datasets) {
  if (is.list(datasets)) {
    sapply(X = datasets, simplify = FALSE, FUN = function(x) {
      if (is.reactive(x)) x else reactive(x)
    })
  } else if (is.reactive(datasets) && inherits(isolate(datasets()), "teal_data")) {
    sapply(
      isolate(names(datasets())),
      function(dataname) {
        reactive(datasets()[[dataname]])
      },
      simplify = FALSE
    )
  } else {
    stop("datasets must be a list of reactive dataframes or a teal_data object")
  }
}
