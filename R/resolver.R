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
resolver <- function(spec, data) {
  if (!inherits(data, "qenv")) {
    stop("Please use qenv() or teal_data() objects.")
  }
  stopifnot(is.transform(spec), has_dataset(spec))
  specf <- spec
  if (has_dataset(specf) && is.delayed(specf$datasets)) {
    specf <- resolver.datasets(specf, data)
  } else if (!has_dataset(specf)) {
    specf$datasets <- na_type("datasets")
  }

  if (has_variable(specf) && !is.delayed(specf$datasets)) {
    specf <- resolver.variables(specf, data)
  } else {
    specf$variables <- na_type("variables")
  }

  if (has_value(specf) && !is.delayed(specf$datasets) && !is.delayed(specf$variables)) {
    specf <- resolver.values(specf, data)
  } else {
    specf$values <- na_type("values")
  }

  attr(specf, "delayed") <- NULL
  delay(specf)
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
  datasets <- names(data)
  # Matrix doesn't have a names method
  if (is.null(datasets)) {
    datasets <- colnames(data)
  }
  l <- lapply(fc_unresolved, function(f) {
    v <- vapply(datasets, function(d) {
      # Extract the data and apply the user supplied function
      out <- tryCatch(f(data(data, d)), error = function(x){FALSE})
      if (!is.logical(out)) {
        stop("Provided functions should return a logical object.")
      }
      if (length(out) != 1L && length(out) != length(data(data, d))) {
        # Function resolution is unconventional, but this would produce too many warnings...
        # warning("The output of the function must be of length 1 or the same length as the data.")
        return(FALSE)
      }
      all(out)
    }, logical(1L))
    datasets[v]
  })
  unique(unlist(l, FALSE, FALSE))
}

resolver.datasets <- function(spec, data) {
  if (!inherits(data, "qenv")) {
    stop("Please use qenv() or teal_data() objects.")
  }
  if (is.null(spec[["datasets"]]) || all(is.na(spec[["datasets"]]))) {
    return(spec)
  }
  sdatasets <- spec$datasets
  data_names <- names(data)
  orig_names <- sdatasets$names
  orig_select <- sdatasets$select
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
      new_select <- unique(new_select[!is.na(new_select)])
      if (!length(new_select)) {
        stop("No datasets meet the requirements to be selected")
      }
      sdatasets$select <- new_select
    }
  } else if (is.delayed(sdatasets)) {
    old_names <- sdatasets$names
    new_names <- c(functions_names(sdatasets$names, data_names),
                   functions_data(sdatasets$names, data))
    new_names <- unique(new_names[!is.na(new_names)])
    if (!length(new_names)) {
      stop("No datasets meet the requirements")
    }
    sdatasets$names <- new_names

    if (length(sdatasets$names) == 0) {
      stop("No selected datasets matching the conditions requested")
    } else if (length(sdatasets$names) == 1) {
      sdatasets$select <- sdatasets$names
    }

    new_select <- c(functions_names(sdatasets$select, sdatasets$names),
                    functions_data(sdatasets$select, data[sdatasets$names]))

    new_select <- unique(new_select[!is.na(new_select)])
    if (!length(new_select)) {
      stop("No datasets meet the requirements to be selected")
    }
    sdatasets$select <- new_select
  }
  attr(sdatasets$names, "original") <- orig(orig_names)
  attr(sdatasets$select, "original") <- orig(orig_select)
  spec$datasets <- resolved(sdatasets, "dataset")
  spec
}

resolver.variables <- function(spec, data) {
  if (!inherits(data, "qenv")) {
    stop("Please use qenv() or teal_data() objects.")
  }

  if (is.delayed(spec$datasets)) {
    stop("Datasets not resolved yet")
  }
  if (is.null(spec[["variables"]]) || all(is.na(spec[["variables"]]))) {
    return(spec)
  }
  datasets <- spec$datasets$select
  data_selected <- data(data, datasets)
  if (is.null(names(data_selected))) {
    names_data <- colnames(data_selected)
  } else {
    names_data <- names(data_selected)
  }

  svariables <- spec$variables
  orig_names <- svariables$names
  orig_select <- svariables$select
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
      new_select <- unique(new_select[!is.na(new_select)])
      if (!length(new_select)) {
        stop("No variables meet the requirements to be selected")
      }
      svariables$select <- new_select
    }
  } else if (is.delayed(svariables)) {
    new_names <- c(functions_names(svariables$names, names_data),
                   functions_data(svariables$names, data_selected))
    new_names <- unique(new_names[!is.na(new_names)])
    if (!length(new_names)) {
      stop("No variables meet the requirements")
    }
    svariables$names <- new_names
    if (length(svariables$names) == 1) {
      svariables$select <- svariables$names
    } else {
      new_select <- c(functions_names(svariables$select, svariables$names),
                      functions_data(svariables$select, data_selected))
      new_select <- unique(new_select[!is.na(new_select)])
      if (!length(new_select)) {
        stop("No variables meet the requirements to be selected")
      }
      svariables$select <- new_select
    }
  }

  attr(svariables$names, "original") <- orig(orig_names)
  attr(svariables$select, "original") <- orig(orig_select)
  spec$variables <- resolved(svariables, "variables")
  spec

}

resolver.values <- function(spec, data) {
  if (!inherits(data, "qenv")) {
    stop("Please use qenv() or teal_data() objects.")
  }

  if (is.null(spec[["values"]]) || all(is.na(spec[["values"]]))) {
    return(spec)
  }
  svalues <- spec$values
  dataset <- data(data, spec$datasets$select)
  variable <- data(dataset, spec$variables$select)
  orig_names <- svalues$names
  orig_select <- svalues$select
  spec$values <- if (is.delayed(svalues) && all(is.character(svalues$names))) {
    match <- intersect(variable, svalues$names)
    missing <- setdiff(svalues$names, variable)
    if (length(missing)) {
      stop("Missing values ", paste(sQuote(missing), collapse = ", "), " were specified.")
    }
    svalues$names <- match
    if (length(match) == 1) {
      svalues$select <- match
    } else {
      match <- intersect(variable, svalues$names)
      new_select <- c(functions_names(svalues$select, svalues$names),
                      functions_data(svalues$select, variable))
      new_select <- unique(new_select[!is.na(new_select)])
      if (!length(new_select)) {
        stop("No variables meet the requirements to be selected")
      }
      svalues$select <- new_select
    }
  } else if (is.delayed(svalues)) {
    new_names <- c(functions_names(svalues$names, variable),
                   functions_data(svalues$names, variable))
    new_names <- unique(new_names[!is.na(new_names)])
    if (!length(new_names)) {
      stop("No variables meet the requirements")
    }
    svalues$names <- new_names

    if (length(svalues$names) == 1) {
      svalues$select <- svalues$names
    } else {
      new_select <- c(functions_names(svalues$select, variable),
                      functions_data(svalues$select, variable))
      new_select <- unique(new_select[!is.na(new_select)])
      if (!length(new_select)) {
        stop("No variables meet the requirements to be selected")
      }
      svalues$select <- new_select
    }
  }
  attr(svalues$names, "original") <- orig(orig_names)
  attr(svalues$select, "original") <- orig(orig_select)
  spec$values <- resolved(svalues, "values")
  spec
}

#' @export
data.MultiAssayExperiment <- function(x, variable) {
  if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
    stop("Required to have MultiAssayExperiment's package.")
  }
  cd <- MultiAssayExperiment::colData(x)
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

#' Update a specification
#'
#' Once a selection is made update the specification for different valid selection.
#' @param spec A resolved specification such as one created with datasets and variables.
#' @param type Which type was updated? One of datasets, variables, values.
#' @param value What is the new selection? One that is a valid value for the given type and specification.
#' @return The specification with restored choices and selection if caused by the update.
#' @export
#' @examples
#' td <- within(teal.data::teal_data(), {
#'     df <- data.frame(A = as.factor(letters[1:5]),
#'                      Ab = LETTERS[1:5])
#'     df_n <- data.frame(C = 1:5,
#'                        Ab = as.factor(letters[1:5]))
#' })
#' data_frames_factors <- datasets(is.data.frame) & variables(is.factor)
#' res <- resolver(data_frames_factors, td)
#' update_spec(res, "datasets", "df_n")
#' # update_spec(res, "datasets", "error")
update_spec <- function(spec, type, value) {
  w <- c("datasets", "variables", "values")
  type <- match.arg(type, w)
  restart_types <- w[seq_along(w) > which(type == w)]
  speci <- spec
  if (!is.character(value)) {
    stop("The updated value is not a character.",
         "\nDo you attempt to set a new specification? Please open an issue")
  }
  valid_names <- spec[[type]]$names
  if (is.delayed(spec[[type]])) {
    stop(type, " should be resolved before updating.")
  }
  if (!is.list(valid_names) && all(value %in% valid_names)) {
    original_select <- orig(spec[[type]]$select)
    spec[[type]][["select"]] <- value
    attr(spec[[type]][["select"]], "original") <- original_select
  } else if (!is.list(valid_names) && !all(value %in% valid_names)) {
    original_select <- orig(spec[[type]]$select)
    valid_values <- intersect(value, valid_names)
    if (!length(valid_values)) {
      stop("No valid value provided.")
    }
    spec[[type]][["select"]] <- valid_values
    attr(spec[[type]][["select"]], "original") <- original_select
  } else {
    stop("It seems the specification needs to be resolved first.")
  }

  # Restore to the original specs
  for (type in restart_types) {

    if (length(spec[[type]]) == 1L && is.na(spec[[type]])) {
      next
    }
    fun <- match.fun(type)
    restored_transform <- fun(x = orig(spec[[type]]$names),
                         select = orig(spec[[type]]$select))
    spec[[type]] <- restored_transform[[type]]
  }
  spec
}

orig <- function(x) {
  attr(x, "original")
}
