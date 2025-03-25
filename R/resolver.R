#' Resolve the specification
#'
#' Given the specification of some data to extract find if they are available or not.
#' The specification for selecting a variable shouldn't depend on the data of said variable.
#' @param spec A object extraction specification.
#' @param data A `qenv()`, or `teal.data::teal_data()` object.
#'
#' @returns A transform but resolved
#' @export
#'
#' @examples
#' dataset1 <- datasets(is.data.frame)
#' dataset2 <- datasets(is.matrix)
#' spec <- dataset1 & variables("a", "a")
#' td <- within(teal.data::teal_data(), {
#'   df <- data.frame(a = as.factor(LETTERS[1:5]), b = letters[1:5])
#'   df2 <- data.frame(a = LETTERS[1:5], b = 1:5)
#'   m <- matrix()
#' })
#' resolver(spec | dataset2, td)
#' resolver(dataset2, td)
#' resolver(spec, td)
#' spec <- dataset1 & variables("a", is.factor)
#' resolver(spec, td)
resolver <- function(spec, data) {
  if (!inherits(data, "qenv")) {
    stop("Please use qenv() or teal_data() objects.")
  }
  if (!is.delayed(spec)) {
    return(spec)
  }
  # Adding some default specifications if they are missing
  if ("values" %in% names(spec) && !"variables" %in% names(spec)) {
    spec <- variables(first) & spec
  }

  if ("variables" %in% names(spec) && !"datasets" %in% names(spec)) {
    spec <- datasets(first) & spec
  }

  stopifnot(is.transform(spec))
  det <- determine(spec, data, spec = spec)
  det$type
}

#' A method that should take a type and resolve it.
#'
#' Generic that makes the minimal check on spec.
#' Responsible of subsetting/extract the data received and check that the type matches
#' @param type The specification to resolve.
#' @param data The minimal data required.
#' @return A list with two elements, the type resolved and the data extracted.
#' @keywords internal
#' @export
determine <- function(type, data, ...) {
  stopifnot(is.type(type) || is.transform(type))
  if (!is.delayed(type)) {
    return(list(type = type, data = data))
  }
  UseMethod("determine")
}

#' @export
determine.default <- function(type, data, ..., spec) {
  if (!is.null(names(spec)) && is.delayed(spec)) {
    rt <- determine(spec, data)
  } else {
    rt <- lapply(spec, resolver, data = data, spec = spec)
    if (length(rt) == 1) {
      return(rt[[1]])
    }
  }
  rt
}

#' @export
determine.transform <- function(type, data, ..., spec) {
  stopifnot(inherits(data, "qenv"))
  # Recursion for other transforms in a list spec | spec
  if (is.null(names(spec))) {
    specs <- lapply(type, data, spec = spec)
    return(specs)
  }

  for (i in seq_along(type)) {
    di <- determine(type[[i]], data, spec = spec)
    # orverwrite so that next type in line receives the corresponding data and specification
    if (is.null(di$type)) {
      next
    }
    type[[i]] <- di$type
    data <- di$data
  }
  list(type = type, data = data) # It is the transform object resolved.
}

functions_names <- function(unresolved, reference) {
  stopifnot(is.character(reference) || is.factor(reference) || is.null(reference)) # Allows for NA characters
  if (is.null(reference)) {
    return(NULL)
  }
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

functions_data <- function(unresolved, data, names) {
  stopifnot(!is.null(data)) # Must be something but not NULL
  fc_unresolved <- unresolved[vapply(unresolved, is.function, logical(1L))]
  l <- lapply(fc_unresolved, function(f) {
    all_data <- tryCatch(f(data), error = function(x){FALSE})
    if (any(all_data)) {
      return(names[all_data])
    } else {
      return(NULL)
    }
  })
  unique(unlist(l, FALSE, FALSE))
}

# Checks that for the given type and data names and data it can be resolved
# The workhorse of the resolver
determine_helper <- function(type, data_names, data) {
  orig_names <- type$names
  orig_select <- type$select
  names_variables_obj <- if (is.null(names(data))) { colnames(data)} else {names(data)}
  if (is.delayed(type) && all(is.character(type$names))) {
    match <- intersect(data_names, type$names)
    missing <- setdiff(type$names, data_names)
    if (length(missing)) {
      return(NULL)
      # stop("Missing datasets ", paste(sQuote(missing), collapse = ", "), " were specified.")
    }
    type$names <- match
    if (length(match) == 0) {
      return(NULL)
      # stop("No selected ", is(type), " matching the conditions requested")
    } else if (length(match) == 1) {
      type$select <- match
    } else {
      new_select <- functions_names(type$select, type$names)
      new_select <- unique(new_select[!is.na(new_select)])
      if (!length(new_select)) {
        return(NULL)
        # stop("No ", is(type), " meet the requirements to be selected")
      }
      type$select <- new_select
    }
  } else if (is.delayed(type)) {
    old_names <- type$names
    new_names <- c(functions_names(type$names, names_variables_obj),
                   functions_data(type$names, data, data_names))
    new_names <- unique(new_names[!is.na(new_names)])
    if (!length(new_names)) {
      return(NULL)
      # stop("No ", is(type), " meet the requirements")
    }
    type$names <- new_names

    if (length(type$names) == 0) {
      return(NULL)
      # stop("No selected ", is(type), " matching the conditions requested")
    } else if (length(type$names) == 1) {
      type$select <- type$names
    }

    new_select <- functions_names(type$select, type$names)

    new_select <- unique(new_select[!is.na(new_select)])
    if (!length(new_select)) {
      return(NULL)
      stop("No ", is(type), " meet the requirements to be selected")
    }
    type$select <- new_select
  }
  attr(type$names, "original") <- orig(orig_names)
  attr(type$select, "original") <- orig(orig_select)
  resolved(type)
}

#' @export
determine.datasets <- function(type, data, ...) {
  if (!inherits(data, "qenv")) {
    stop("Please use qenv() or teal_data() objects.")
  }

  l <- vector("list", length(data))
  for (i in seq_along(data)){
    data_name_env <- names(data)[i]
    out <- determine_helper(type, data_name_env, data[[data_name_env]])
    if (!is.null(out)) {
      l[[i]] <- out
    }
  }

  # Merge together all the types
  type <- do.call(c, l[lengths(l) > 1])
  # Not possible to know what is happening

  if (!is.delayed(type) && length(type$select) > 1) {
    list(type = type, data = data[type$select])
  } else if (!is.delayed(type) && length(type$select) == 1) {
    list(type = type, data = data[[type$select]])
  } else {
    list(type = type, data = NULL)
  }
}

#' @export
determine.variables <- function(type, data, ...) {
  if (length(dim(data)) != 2L) {
    stop("Can't resolve variables from this object of class ", class(data))
  }

  if (ncol(data) <= 0L) {
    stop("Can't pull variable: No variable is available.")
  }

  # Assumes the object has colnames method (true for major object classes: DataFrame, tibble, Matrix, array)
  # FIXME: What happens if colnames is null: array(dim = c(4, 2)) |> colnames()
  l <- vector("list", ncol(data))
  for (i in seq_len(ncol(data))){
    out <- determine_helper(type, colnames(data)[i], data[, i])
    if (!is.null(out)) {
      l[[i]] <- out
    }
  }

  # Merge together all the types
  type <- do.call(c, l[lengths(l) > 1])

  # Not possible to know what is happening
  if (is.delayed(type)) {
    return(list(type = type, data = NULL))
  }
  # This works for matrices and data.frames of length 1 or multiple
  # be aware of drop behavior on tibble vs data.frame
  list(type = type, data = data[, type$select])
}

#' @export
determine.mae_colData <- function(type, data, ...) {
  if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
    stop("Requires 'MultiAssayExperiment' package.")
  }

  new_data <- colData(data)
  for (i in seq_along(new_data)){
    determine_helper(type, colnames(data)[i], new_data[, i])
  }
  if (length(dim(new_data)) != 2L) {
    stop("Can't resolve variables from this object of class ", class(new_data))
  }
  if (ncol(new_data) <= 0L) {
    stop("Can't pull variable: No variable is available.")
  }
  type <- determine_helper(type, colnames(data), data)

  # Not possible to know what is happening
  if (is.delayed(type)) {
    return(list(type = type, data = NULL))
  }

  if (length(type$select) > 1) {
    list(type = type, data = data[type$select])

  } else {
    list(type = type, data = data[[type$select]])
  }
}

#' @export
determine.mae_experiments <- function(type, data, ...) {
  if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
    stop("Requires 'MultiAssayExperiment' package.")
  }
  new_data <- experiments(data)
  type <- determine_helper(type, names(new_data), new_data)

  # Not possible to know what is happening
  if (is.delayed(type)) {
  }

  if (!is.delayed(type) && length(type$select) > 1) {
    list(type = type, data = new_data[type$select])

  } else if (!is.delayed(type) && length(type$select) == 1) {
    list(type = type, data = new_data[[type$select]])
  } else {
    return(list(type = type, data = NULL))
    }
}

#' @export
determine.mae_sampleMap <- function(type, data, ...) {
  if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
    stop("Requires 'MultiAssayExperiment' package.")
  }

  new_data <- sampleMap(data)
  type <- determine_helper(type, names(new_data), new_data)

  # Not possible to know what is happening
  if (is.delayed(type)) {
    return(list(type = type, data = NULL))
  }

  if (length(type$select) > 1) {
    list(type = type, data = data[type$select])

  } else {
    list(type = type, data = data[[type$select]])
  }
}

#' @export
determine.values <- function(type, data, ...) {
  type <- determine_helper(type, names(data), data)

  # Not possible to know what is happening
  if (is.delayed(type)) {
    return(list(type = type, data = NULL))
  }

  list(type = type, data = type$select)
}

orig <- function(x) {
  attr(x, "original")
}

unorig <- function(x) {
  attr(x, "original") <- NULL
  x
}
