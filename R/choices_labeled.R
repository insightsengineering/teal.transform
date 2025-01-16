#' Set "`<choice>:<label>`" type of names
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This is often useful for [choices_selected()] as it marks up the drop-down boxes
#' for [shiny::selectInput()].
#'
#' @details
#' If either `choices` or `labels` are factors, they are coerced to character.
#' Duplicated elements from `choices` get removed.
#'
#' @param choices (`character` or `factor` or `numeric` or `logical`) vector.
#' @param labels (`character`) vector containing labels to be applied to `choices`.
#' If `NA` then "Label Missing" will be used.
#' @param subset (`character` or `factor` or `numeric` or `logical`) vector that
#' is a subset of `choices`.
#' This is useful if only a few variables need to be named.
#' If this argument is used, the returned vector will match its order.
#' @param types (`character`) vector containing the types of the columns to be used for applying the appropriate
#' icons to the [choices_selected] drop down box (e.g. "numeric").
#'
#' @return Named `character` vector.
#'
#' @examples
#' library(teal.data)
#' library(shiny)
#'
#' ADSL <- rADSL
#' ADTTE <- rADTTE
#'
#' choices1 <- choices_labeled(names(ADSL), col_labels(ADSL, fill = FALSE))
#' choices2 <- choices_labeled(ADTTE$PARAMCD, ADTTE$PARAM)
#'
#' # if only a subset of variables are needed, use subset argument
#' choices3 <- choices_labeled(
#'   names(ADSL),
#'   col_labels(ADSL, fill = FALSE),
#'   subset = c("ARMCD", "ARM")
#' )
#'
#' ui <- fluidPage(
#'   selectInput("c1",
#'     label = "Choices from ADSL",
#'     choices = choices1,
#'     selected = choices1[1]
#'   ),
#'   selectInput("c2",
#'     label = "Choices from ADTTE",
#'     choices = choices2,
#'     selected = choices2[1]
#'   ),
#'   selectInput("c3",
#'     label = "Arm choices from ADSL",
#'     choices = choices3,
#'     selected = choices3[1]
#'   )
#' )
#' server <- function(input, output) {}
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#' @export
#'
choices_labeled <- function(choices, labels, subset = NULL, types = NULL) {
  if (is.factor(choices)) {
    choices <- as.character(choices)
  }

  checkmate::assert_atomic(choices, min.len = 1, any.missing = FALSE)

  if (is.factor(labels)) {
    labels <- as.character(labels)
  }

  checkmate::assert_character(labels[!is.na(labels)], any.missing = FALSE)
  if (length(choices) != length(labels)) {
    stop("length of choices must be the same as labels")
  }
  checkmate::assert_subset(subset, choices, empty.ok = TRUE)
  checkmate::assert_character(types, len = length(choices), null.ok = TRUE)

  if (!is.null(subset)) {
    if (!all(subset %in% choices)) {
      stop("all of subset variables must be in choices")
    }
    labels <- labels[choices %in% subset]
    types <- types[choices %in% subset]
    choices <- choices[choices %in% subset]
  }

  is_dupl <- duplicated(choices)
  choices <- choices[!is_dupl]
  labels <- labels[!is_dupl]
  types <- types[!is_dupl]
  labels[is.na(labels)] <- "Label Missing"
  raw_labels <- labels
  combined_labels <- if (length(choices) > 0) {
    paste0(choices, ": ", labels)
  } else {
    character(0)
  }

  if (!is.null(subset)) {
    ord <- match(subset, choices)
    choices <- choices[ord]
    raw_labels <- raw_labels[ord]
    combined_labels <- combined_labels[ord]
    types <- types[ord]
  }

  structure(
    choices,
    names = combined_labels,
    raw_labels = raw_labels,
    combined_labels = combined_labels,
    class = c("choices_labeled", "character"),
    types = types
  )
}

#' Variable label extraction and custom selection from data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Wrapper on [choices_labeled] to label variables basing on existing labels in data.
#'
#' @rdname variable_choices
#'
#' @param data (`data.frame` or `character`)
#' If `data.frame`, then data to extract labels from.
#' If `character`, then name of the dataset to extract data from once available.
#' @param subset (`character` or `function`)
#' If `character`, then a vector of column names.
#' If `function`, then this function is used to determine the possible columns (e.g. all factor columns).
#' In this case, the function must take only single argument "data" and return a character vector.
#'
#' See examples for more details.
#' @param key (`character`) vector with names of the variables, which are part of the primary key
#' of the `data` argument.
#'
#' This is an optional argument, which allows to identify variables associated
#' with the primary key and display the appropriate icon for them in the
#' [teal.widgets::optionalSelectInput()] widget.
#' @param fill (`logical(1)`) if `TRUE`, the function will return variable names
#' for columns with non-existent labels; otherwise will return `NA` for them.
#'
#' @return Named `character` vector with additional attributes or `delayed_data` object.
#'
#' @examples
#' library(teal.data)
#' ADRS <- rADRS
#' variable_choices(ADRS)
#' variable_choices(ADRS, subset = c("PARAM", "PARAMCD"))
#' variable_choices(ADRS, subset = c("", "PARAM", "PARAMCD"))
#' variable_choices(
#'   ADRS,
#'   subset = c("", "PARAM", "PARAMCD"),
#'   key = default_cdisc_join_keys["ADRS", "ADRS"]
#' )
#'
#' # delayed version
#' variable_choices("ADRS", subset = c("USUBJID", "STUDYID"))
#'
#' # functional subset (with delayed data) - return only factor variables
#' variable_choices("ADRS", subset = function(data) {
#'   idx <- vapply(data, is.factor, logical(1))
#'   names(data)[idx]
#' })
#' @export
#'
variable_choices <- function(data, subset = NULL, fill = FALSE, key = NULL) {
  checkmate::assert(
    checkmate::check_character(subset, null.ok = TRUE, any.missing = FALSE),
    checkmate::check_function(subset)
  )
  checkmate::assert_flag(fill)
  checkmate::assert_character(key, null.ok = TRUE, any.missing = FALSE)

  UseMethod("variable_choices")
}

#' @rdname variable_choices
#' @export
variable_choices.character <- function(data, subset = NULL, fill = FALSE, key = NULL) {
  structure(list(data = data, subset = subset, key = key),
    class = c("delayed_variable_choices", "delayed_data", "choices_labeled")
  )
}

#' @rdname variable_choices
#' @export
variable_choices.data.frame <- function(data, subset = NULL, fill = TRUE, key = NULL) {
  checkmate::assert(
    checkmate::check_character(subset, null.ok = TRUE),
    checkmate::check_function(subset, null.ok = TRUE)
  )

  if (is.function(subset)) {
    subset <- resolve_delayed_expr(subset, ds = data, is_value_choices = FALSE)
  }

  checkmate::assert_subset(subset, c("", names(data)), empty.ok = TRUE)

  if (length(subset) == 0) {
    subset <- names(data)
  }

  key <- intersect(subset, key)

  var_types <- vapply(data, function(x) class(x)[[1]], character(1))

  if (length(key) != 0) {
    var_types[key] <- "primary_key"
  }

  if (any(duplicated(subset))) {
    warning(
      "removed duplicated entries in subset:",
      paste(unique(subset[duplicated(subset)]), collapse = ", ")
    )
    subset <- unique(subset)
  }

  if ("" %in% subset) {
    choices_labeled(
      choices = c("", names(data)),
      labels = c("", unname(teal.data::col_labels(data, fill = fill))),
      subset = subset,
      types = c("", var_types)
    )
  } else {
    choices_labeled(
      choices = names(data),
      labels = unname(teal.data::col_labels(data, fill = fill)),
      subset = subset,
      types = var_types
    )
  }
}

#' Value labeling and filtering based on variable relationship
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Wrapper on [choices_labeled] to label variable values basing on other variable values.
#'
#' @rdname value_choices
#'
#' @param data (`data.frame`, `character`)
#' If `data.frame`, then data to extract labels from.
#' If `character`, then name of the dataset to extract data from once available.
#' @param var_choices (`character`, `delayed_variable_choices`) Choice of column names.
#' @param var_label (`character`) vector with labels column names.
#' @param subset (`character` or `function`)
#' If `character`, vector with values to subset.
#' If `function`, then this function is used to determine the possible columns (e.g. all factor columns).
#' In this case, the function must take only single argument "data" and return a character vector.
#'
#' See examples for more details.
#' @param sep (`character`) separator used in case of multiple column names.
#'
#' @return named character vector or `delayed_data` object.
#'
#' @examples
#' ADRS <- teal.data::rADRS
#' value_choices(ADRS, "PARAMCD", "PARAM", subset = c("BESRSPI", "INVET"))
#' value_choices(ADRS, c("PARAMCD", "ARMCD"), c("PARAM", "ARM"))
#' value_choices(ADRS, c("PARAMCD", "ARMCD"), c("PARAM", "ARM"),
#'   subset = c("BESRSPI - ARM A", "INVET - ARM A", "OVRINV - ARM A")
#' )
#' value_choices(ADRS, c("PARAMCD", "ARMCD"), c("PARAM", "ARM"), sep = " --- ")
#'
#' # delayed version
#' value_choices("ADRS", c("PARAMCD", "ARMCD"), c("PARAM", "ARM"))
#'
#' # functional subset
#' value_choices(ADRS, "PARAMCD", "PARAM", subset = function(data) {
#'   levels(data$PARAMCD)[1:2]
#' })
#' @export
#'
value_choices <- function(data,
                          var_choices,
                          var_label = NULL,
                          subset = NULL,
                          sep = " - ") {
  checkmate::assert(
    checkmate::check_character(var_choices, any.missing = FALSE),
    checkmate::check_class(var_choices, "delayed_variable_choices")
  )
  checkmate::assert_character(var_label, len = length(var_choices), null.ok = TRUE, any.missing = FALSE)
  checkmate::assert(
    checkmate::check_vector(subset, null.ok = TRUE),
    checkmate::check_function(subset)
  )
  checkmate::assert_string(sep)
  UseMethod("value_choices")
}

#' @rdname value_choices
#' @export
value_choices.character <- function(data,
                                    var_choices,
                                    var_label = NULL,
                                    subset = NULL,
                                    sep = " - ") {
  structure(
    list(
      data = data,
      var_choices = var_choices,
      var_label = var_label,
      subset = subset,
      sep = sep
    ),
    class = c("delayed_value_choices", "delayed_data", "choices_labeled")
  )
}

#' @rdname value_choices
#' @export
value_choices.data.frame <- function(data,
                                     var_choices,
                                     var_label = NULL,
                                     subset = NULL,
                                     sep = " - ") {
  checkmate::assert_subset(var_choices, names(data))
  checkmate::assert_subset(var_label, names(data), empty.ok = TRUE)

  var_choices <- as.vector(var_choices)
  df_choices <- data[var_choices]
  df_label <- data[var_label]

  for (i in seq_along(var_choices)) {
    if ("NA" %in% c(df_choices[[i]], levels(df_choices[[i]])) && any(is.na(df_choices[[i]]))) {
      warning(paste0(
        "Missing values and the string value of 'NA' both exist in the column of ", var_choices[i],
        " either as value(s) or level(s). ",
        "This will cause the missing values to be grouped with the actual string 'NA' values in the UI widget."
      ))
    }
  }

  choices <- if (
    length(var_choices) > 1 ||
      is.character(df_choices[[1]]) ||
      is.factor(df_choices[[1]]) ||
      inherits(df_choices[[1]], c("Date", "POSIXct", "POSIXlt", "POSIXt"))
  ) {
    df_choices <- dplyr::mutate_if(
      df_choices,
      .predicate = function(col) inherits(col, c("POSIXct", "POSIXlt", "POSIXt")),
      .funs = function(col) {
        if (is.null(attr(col, "tzone")) || all(attr(col, "tzone") == "")) {
          format(trunc(col), "%Y-%m-%d %H:%M:%S")
        } else {
          format(trunc(col), "%Y-%m-%d %H:%M:%S %Z")
        }
      }
    )
    apply(df_choices, 1, paste, collapse = sep)
  } else {
    df_choices[[var_choices]]
  }
  labels <- apply(df_label, 1, paste, collapse = sep)
  df <- unique(data.frame(choices, labels, stringsAsFactors = FALSE)) # unique combo of choices x labels

  if (is.function(subset)) {
    subset <- resolve_delayed_expr(subset, ds = data, is_value_choices = TRUE)
  }
  res <- choices_labeled(
    choices = df$choices,
    labels = df$labels,
    subset = subset
  )
  attr(res, "sep") <- sep
  attr(res, "var_choices") <- var_choices
  attr(res, "var_label") <- var_label
  res
}

#' @describeIn choices_labeled Print choices_labeled object
#'
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#'
print.choices_labeled <- function(x, ...) {
  cat(
    sprintf("number of choices: %s \n", length(x)),
    names(x),
    "",
    sep = "\n"
  )

  invisible(x)
}
