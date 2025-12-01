#' Checks `varname` argument and convert to call
#'
#' Checks `varname` type and parse if it's a `character`.
#'
#' @param varname (`name` or `call` or `character(1)`)
#' name of the variable
#'
#' @returns the parsed `varname`.
#'
#' @keywords internal
#'
call_check_parse_varname <- function(varname) {
  checkmate::assert(
    checkmate::check_string(varname),
    checkmate::check_class(varname, "call"),
    checkmate::check_class(varname, "name")
  )
  if (is.character(varname)) {
    parsed <- parse(text = varname, keep.source = FALSE)
    if (length(parsed) == 1) {
      varname <- parsed[[1]]
    } else {
      stop(
        sprintf(
          "Problem with parsing '%s'. Not able to process multiple calls",
          varname
        )
      )
    }
  }
  varname
}

#' Choices condition call
#'
#' Compose choices condition call from inputs.
#'
#' @details
#' `choices` can be vector of any type but for some output might be converted:
#' * `factor` call is composed on choices converted to `character`;
#' * `Date` call is composed on choices converted to `character` using
#' `format(choices)`;
#' * `POSIXct`, `POSIXlt` call is composed on choices converted to `character` using
#' `format(choices)`.
#'
#' One has to be careful here as formatted date-time variable might loose
#' some precision (see `format` argument in [format.POSIXlt()] and output call
#' could be insufficient for exact comparison. In this case one should specify
#' `varname = trunc(<varname>)` and possibly convert `choices` to `character`).
#'
#' @param varname (`name` or `call` or `character(1)`)
#' name of the variable.
#' @param choices (`vector`)
#' `varname` values to match using the `==` (single value) or `%in%` (vector)
#' condition.
#'
#' @return `call`.
#'
#' @keywords internal
#'
call_condition_choice <- function(varname, choices) {
  varname <- call_check_parse_varname(varname)

  if (is.factor(choices)) {
    choices <- as.character(choices)
  } else if (inherits(choices, "Date")) {
    choices <- format(choices)
  } else if (inherits(choices, c("POSIXct", "POSIXlt"))) {
    choices <- format(choices)
  }


  if (length(choices) == 1) {
    call("==", varname, choices)
  } else {
    c_call <- do.call(
      "call",
      append(list("c"), choices)
    )
    # c_call needed because it needs to be vector call
    # instead of vector. SummarizedExperiment.subset
    # handles only vector calls
    call("%in%", varname, c_call)
  }
}

#' `numeric` range condition call
#'
#' Compose `numeric` range condition call from inputs.
#'
#' @param varname (`name` or `character(1)`)
#' name of the variable.
#'
#' @param range (`numeric(2)`)
#' range of the variable.
#'
#' @return `call`.
#'
#' @keywords internal
#'
call_condition_range <- function(varname, range) {
  checkmate::assert_numeric(range, len = 2, sorted = TRUE)

  varname <- call_check_parse_varname(varname)
  call(
    "&",
    call(">=", varname, range[1]),
    call("<=", varname, range[2])
  )
}

#' `logical` variable condition call
#'
#' Compose `logical` variable condition call from inputs.
#'
#' @param varname (`name` or `character(1)`)
#' name of the variable
#'
#' @param choice (`logical(1)`)
#' chosen value
#'
#' @return `call`.
#'
#' @keywords internal
#'
call_condition_logical <- function(varname, choice) {
  checkmate::assert_flag(choice)
  varname <- call_check_parse_varname(varname)

  if (choice) {
    varname
  } else if (!choice) {
    call("!", varname)
  } else {
    stop(
      "Unknown filter state", toString(choice),
      " for logical var ", as.character(varname)
    )
  }
}

#' `POSIXct` range condition call
#'
#' Compose `POSIXct` range condition call from inputs.
#'
#' @param varname (`name` or `character(1)`) name of the variable.
#' @param range (`POSIXct`) range of the variable.
#' Be aware that output uses truncated range format `"%Y-%m-%d %H:%M:%S"`,
#' which means that some precision might be lost.
#' @param timezone (`character(1)`) specifies the time zone to be used for the conversion.
#' By default `Sys.timezone()` is used.
#'
#' @return `call`.
#'
#' @keywords internal
#'
call_condition_range_posixct <- function(varname, range, timezone = Sys.timezone()) {
  checkmate::assert_posixct(range, len = 2, sorted = TRUE)
  checkmate::assert_string(timezone)
  varname <- call_check_parse_varname(varname)

  range[1] <- trunc(range[1], units = c("secs"))
  range[2] <- trunc(range[2] + 1, units = c("secs"))

  range <- format(
    range,
    format = "%Y-%m-%d %H:%M:%S",
    tz = timezone
  )

  call(
    "&",
    call(">=", varname, call("as.POSIXct", range[1], tz = timezone)),
    call("<", varname, call("as.POSIXct", range[2], tz = timezone))
  )
}

#' `Date` range condition call
#'
#' Compose `Date` range condition call from inputs.
#'
#' @param varname (`name` or `character(1)`) name of the variable.
#' @param range (`Date`) range of the variable.
#'
#' @return `call`.
#'
#' @keywords internal
#'
call_condition_range_date <- function(varname, range) {
  checkmate::assert_date(range, len = 2)
  checkmate::assert_true(range[2] >= range[1])
  varname <- call_check_parse_varname(varname)

  call(
    "&",
    call(">=", varname, call("as.Date", as.character(range[1]))),
    call("<=", varname, call("as.Date", as.character(range[2])))
  )
}

#' Get call to subset and select array
#'
#' @param dataname (`character(1)` or `name`).
#' @param row (`name` or `call` or `logical` or `integer` or `character`) optional
#' name of the `row` or condition.
#' @param column (`name` or `call` or `logical` or `integer` or `character`) optional
#' name of the `column` or condition.
#' @param aisle (`name` or `call` or `logical` or `integer` or `character`) optional
#' name of the `row` or condition.
#'
#' @return [Extract()] `call` for 3-dimensional array in `x[i, j, k]` notation.
#'
#' @keywords internal
#'
call_extract_array <- function(dataname = ".", row = NULL, column = NULL, aisle = NULL) {
  checkmate::assert(
    checkmate::check_string(dataname),
    checkmate::check_class(dataname, "call"),
    checkmate::check_class(dataname, "name")
  )
  stopifnot(is.null(row) || is.call(row) || is.character(row) || is.logical(row) || is.integer(row) || is.name(row))
  stopifnot(is.null(column) || is.call(column) || is.vector(column) || is.name(column))
  stopifnot(is.null(aisle) || is.call(aisle) || is.vector(aisle) || is.name(aisle))

  if (is.language(dataname)) {
    dataname <- paste(trimws(deparse(dataname, width.cutoff = 500L)), collapse = "\n")
  }

  row <- if (is.null(row)) {
    ""
  } else {
    paste(trimws(deparse(row, width.cutoff = 500L)), collapse = "\n")
  }
  column <- if (is.null(column)) {
    ""
  } else {
    paste(trimws(deparse(column, width.cutoff = 500L)), collapse = "\n")
  }
  aisle <- if (is.null(aisle)) {
    ""
  } else {
    paste(trimws(deparse(aisle, width.cutoff = 500L)), collapse = "\n")
  }

  parse(
    text = sprintf("%s[%s, %s, %s]", dataname, row, column, aisle),
    keep.source = FALSE
  )[[1]]
}

#' Get call to subset and select matrix
#'
#' @param dataname (`character(1)` or `name`).
#' @param row (`name` or `call` or `logical` or `integer` or `character`) optional
#' name of the `row` or condition.
#' @param column (`name` or `call` or `logical` or `integer` or `character`) optional
#' name of the `column` or condition.
#'
#' @return [Extract()] `call` for matrix in `x[i, j]` notation.
#'
#' @keywords internal
#'
call_extract_matrix <- function(dataname = ".", row = NULL, column = NULL) {
  checkmate::assert(
    checkmate::check_string(dataname),
    checkmate::check_class(dataname, "call"),
    checkmate::check_class(dataname, "name")
  )
  stopifnot(is.null(row) || is.call(row) || is.character(row) || is.logical(row) || is.integer(row) || is.name(row))
  stopifnot(is.null(column) || is.call(column) || is.vector(column) || is.name(column))

  if (is.language(dataname)) {
    dataname <- paste(trimws(deparse(dataname, width.cutoff = 500L)), collapse = "\n")
  }

  row <- if (is.null(row)) {
    ""
  } else {
    paste(trimws(deparse(row, width.cutoff = 500L)), collapse = "\n")
  }
  column <- if (is.null(column)) {
    ""
  } else {
    paste(trimws(deparse(column, width.cutoff = 500L)), collapse = "\n")
  }

  parse(
    text = sprintf("%s[%s, %s]", dataname, row, column),
    keep.source = FALSE
  )[[1]]
}


#' Compose extract call with `$` operator
#'
#' @param dataname (`character(1)` or `name`) name of the object.
#' @param varname (`character(1)` or `name`) name of the slot in data.
#' @param dollar (`logical(1)`) whether returned call should use `$` or `[[` operator.
#'
#' @return [Extract()] `call` in `$` or `[[` notation (depending on parameters).
#'
#' @keywords internal
#'
call_extract_list <- function(dataname, varname, dollar = TRUE) {
  checkmate::assert_flag(dollar)
  checkmate::assert(
    checkmate::check_string(varname),
    checkmate::check_class(varname, "name"),
    checkmate::assert(
      combine = "and",
      checkmate::check_class(varname, "call"),
      checkmate::check_false(dollar)
    )
  )

  dataname <- call_check_parse_varname(dataname)

  if (dollar) {
    call("$", dataname, varname)
  } else {
    call("[[", dataname, varname)
  }
}

#' Create a call using a function in a given namespace
#'
#' The dot arguments in `...` need to be quoted because they will be evaluated otherwise.
#'
#' @param name `character` function name, possibly using namespace colon `::`, also
#' works with `:::` (sometimes needed, but strongly discouraged).
#' @param ... arguments to pass to function with name `name`.
#' @param unlist_args `list` extra arguments passed in a single list,
#' avoids the use of `do.call` with this function.
#'
#' @return `call`.
#'
#' @keywords internal
#'
call_with_colon <- function(name, ..., unlist_args = list()) {
  checkmate::assert_string(name)
  checkmate::assert_list(unlist_args)
  as.call(c(
    parse(text = name, keep.source = FALSE)[[1]],
    c(list(...), unlist_args)
  ))
}


#' Combine calls by operator
#'
#' Combine list of calls by specific operator.
#'
#' @param operator (`character(1)` or `name`) name / symbol of the operator.
#' @param calls (`list` of calls) list containing calls to be combined by `operator`.
#'
#' @return A combined `call`.
#'
#' @keywords internal
#'
calls_combine_by <- function(operator, calls) {
  checkmate::assert_string(operator)
  stopifnot(
    all(
      vapply(
        X = calls,
        FUN.VALUE = logical(1),
        FUN = function(x) is.language(x) || is.logical(x)
      )
    )
  )

  Reduce(
    x = calls,
    f = function(x, y) call(operator, x, y)
  )
}

#' Check if a call or list of calls uses the pipe operator (%>%)
#'
#' Recursively searches through a call or list of calls to determine
#' if the pipe operator `%>%` is used anywhere.
#'
#' @param x (`call`, `name`, or `list` of calls) The call(s) to check.
#'
#' @return `logical(1)` `TRUE` if `%>%` is found, `FALSE` otherwise.
#'
#' @keywords internal
#'
call_uses_magrittr_pipe <- function(x) {
  if (is.call(x)) {
    # Check if there is the pipe operator
    # Handle both quote(`%>%`) and as.name("%>%") cases
    return(any(grepl("%>%", x, fixed = TRUE)))
  }

  if (is.list(x) && length(x)) {
    # Check all elements in the list
    for (call_x in x) {
      if (Recall(call_x)) {
        return(TRUE)
      }
    }
  }

  FALSE
}
