#' Resolve the specification
#'
#' Given the specification of some data to extract find if they are available or not.
#'
#' @param spec A object extraction specification.
#' @param data A `qenv()`, or `teal.data::teal_data()` object.
#'
#' @returns A transform but resolved
#' @export
#'
#' @examples
#' dataset1 <- datasets("df", function(x){head(x, 1)})
#' dataset2 <- datasets(is.matrix, function(x){head(x, 1)})
#' spec <- dataset1 & variables("a", "a")
#' td <- within(teal.data::teal_data(), {
#'   df <- data.frame(a = as.factor(LETTERS[1:5]), b = letters[1:5])
#'   m <- matrix()
#' })
#' resolver(dataset2, td)
#' resolver(spec, td)
#' spec <- dataset1 & variables("a", is.factor)
#' resolver(spec, td)
resolver <- function(spec, data, ...) {
  if (!is(data, "qenv")) {
    stop("Please use qenv() or teal_data() objects.")
  }
  stopifnot(is.transform(spec), has_dataset(spec))
  specf <- spec
  if (has_dataset(specf)) {
    specf <- resolver.datasets(specf, data)
  } else {
    specf$datasets <- NULL
  }

  if (has_variable(specf) && !is.delayed(specf$datasets)) {
    specf <- resolver.variables(specf, data)
  } else {
    specf$variables <- NULL
  }

  if (has_value(specf) && !is.delayed(specf$datasets) && !is.delayed(specf$variables)) {
    specf <- resolver.values(specf, data)
  } else {
    specf$values <- NULL
  }

  class(specf) <- setdiff(class(specf), "delayed")
  specf
}

functions_names <- function(unresolved, reference) {
  is_fc <- vapply(unresolved, is.function, logical(1L))
  fc_unresolved <- unresolved[is_fc]
  x <- vector("character")
  for (f in fc_unresolved) {

    y <- tryCatch(f(reference), error = function(x) f )
    if (!is.logical(y)) {
      stop("Provided functions should return a logical object.")
    }
    x <- c(x, reference[y[!is.na(y)]])
  }
  unique(unlist(c(unresolved[!is_fc], x), FALSE, FALSE))
}

functions_data <- function(unresolved, data) {
  fc_unresolved <- unresolved[vapply(unresolved, is.function, logical(1L))]

  # This is for variables
  names <- names(data)
  datasets <- names(data)
  l <- lapply(fc_unresolved, function(f) {
    v <- vapply(datasets, function(d) {
      # Extract the data and apply the user supplied function
      out <- f(data(data, d))
      if (!is.logical(out)) {
        stop("Provided functions should return a logical object.")
      }
      if (length(out) > 1L) {
        # Function resolution is unconventional...
        return(FALSE)
      }
      out
    }, logical(1L))
    datasets[v]
  })
  unique(unlist(l, FALSE, FALSE))
}

resolver.datasets <- function(spec, data) {
  if (!is(data, "qenv")) {
    stop("Please use qenv() or teal_data() objects.")
  }

  sdatasets <- spec$datasets
  data_names <- names(data)

  if (is.delayed(sdatasets) && all(is.character(sdatasets$names))) {
    match <- intersect(data_names, sdatasets$names)
    missing <- setdiff(sdatasets$names, data_names)
    if (length(missing)) {
      stop("Missing datasets ", paste(sQuote(missing), collapse = ", "), " were specified.")
    }
    sdatasets$names <- match
    if (length(match) == 0) {
      stop("No selected datasets matching the conditions requested")
    } else if (length(match) == 1) {
      sdatasets$select <- match
    } else {
      new_select <- c(functions_names(sdatasets$select, sdatasets$names),
                      functions_data(sdatasets$select, data[sdatasets$names]))
      sdatasets$select <- unique(new_select[!is.na(new_select)])
    }
  } else if (is.delayed(sdatasets)) {
    new_names <- c(functions_names(sdatasets$names, data_names),
                   functions_data(sdatasets$names, data))
    sdatasets$names <- unique(new_names[!is.na(new_names)])

    if (length(sdatasets$names) == 0) {
      stop("No selected datasets matching the conditions requested")
    } else if (length(sdatasets$names) == 1) {
      sdatasets$select <- sdatasets$names
    }

    new_select <- c(functions_names(sdatasets$select, sdatasets$names),
                    functions_data(sdatasets$select, data[sdatasets$names]))

    sdatasets$select <- unique(new_select[!is.na(new_select)])
  }

  spec$datasets <- resolved(sdatasets, "dataset")
  spec
}

resolver.variables <- function(spec, data) {
  if (!is(data, "qenv")) {
    stop("Please use qenv() or teal_data() objects.")
  }

  if (is.delayed(spec$datasets)) {
    stop("Datasets not resolved yet")
  }
  datasets <- spec$datasets$select
  data_selected <- data(data, datasets)
  names_data <- names(data_selected)

  svariables <- spec$variables

  if (is.delayed(svariables) && all(is.character(svariables$names))) {
    match <- intersect(names_data, svariables$names)
    missing <- setdiff(svariables$names, names_data)
    if (length(missing)) {
      stop("Missing variables ", paste(sQuote(missing), collapse = ", "), " were specified.")
    }
    svariables$names <- match
    if (length(match) == 1) {
      svariables$select <- match
    } else {
      new_select <- c(functions_names(svariables$select, svariables$names),
                      functions_data(svariables$select, data_selected))
      svariables$select <- unique(new_select[!is.na(new_select)])
    }
  } else if (is.delayed(svariables)) {
    new_names <- c(functions_names(svariables$names, names_data),
                   functions_data(svariables$names, data_selected))
    svariables$names <- unique(new_names[!is.na(new_names)])
    # browser()
    if (length(svariables$names) == 1) {
      svariables$select <- svariables$names
    } else {
      new_select <- c(functions_names(svariables$select, svariables$names),
                      functions_data(svariables$select, data_selected))
      svariables$select <- unique(new_select[!is.na(new_select)])
    }
  }
  spec$variables <- resolved(svariables, "variables")
  spec
}

resolver.values <- function(spec, data) {
  if (!is(data, "qenv")) {
    stop("Please use qenv() or teal_data() objects.")
  }

  variables <- spec$variables$names
  svalues <- spec$values
  spec$variables <- if (is.delayed(svalues) && all(is.character(svalues$names))) {
    match <- intersect(datasets, svalues$names)
    missing <- setdiff(svalues$names, datasets)
    if (length(missing)) {
      stop("Missing values ", paste(sQuote(missing), collapse = ", "), " were specified.")
    }
    svalues$names <- match
    svalues$select <- functions_names(svalues$select, match)
    svalues
  } else if (is.delayed(svalues)) {
    svalues$names <- functions_names(svalues$names, datasets)
    svalues$select <- functions_names(svalues$select, svalues$names)
    svalues
  }

  spec$values <- resolved(svalues, "values")
  spec
}

#' @export
data.MultiAssayExperiment <- function(x, variable) {
  # length(variable) == 1L
  cd <- colData(x)
  cd[[variable]]
}

#' @export
data.matrix <- function(x, variable) {
  # length(variable) == 1L
  x[, variable, drop = TRUE]
}

#' @export
#' @method data data.frame
data.data.frame <- function(x, variable) {
  # length(variable) == 1L
  x[, variable, drop = TRUE]
}

#' @export
data.qenv <- function(x, variable) {
  x[[variable]]
}

#' @export
data.default <- function(x, variable) {
  x[, variable, drop = TRUE]
}

#' @export
data <- function(x, variable) {
  UseMethod("data")
}
