#' Resolve the specification
#'
#' Given the specification of some data to extract find if they are available or not.
#' The specification for selecting a variable shouldn't depend on the data of said variable.
#' @param spec A object extraction specification.
#' @param data The qenv where the specification is evaluated.
#'
#' @returns A specification but resolved: the names and selection is the name of the objects (if possible).
#' @export
#'
#' @examples
#' dataset1 <- datasets(where(is.data.frame))
#' dataset2 <- datasets(where(is.matrix))
#' spec <- c(dataset1, variables("a", "a"))
#' td <- within(teal.data::teal_data(), {
#'   df <- data.frame(a = as.factor(LETTERS[1:5]), b = letters[1:5])
#'   df2 <- data.frame(a = LETTERS[1:5], b = 1:5)
#'   m <- matrix()
#' })
#' resolver(list(spec, dataset2), td)
#' resolver(dataset2, td)
#' resolver(spec, td)
#' spec <- c(dataset1, variables("a", where(is.character)))
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
    spec <- c(variables(first), spec)
  }

  if ("variables" %in% names(spec) && !"datasets" %in% names(spec)) {
    spec <- c(datasets(first), spec)
  }

  stopifnot(is.list(spec) || is.specification(spec))
  det <- determine(spec, data, spec = spec)
  if (is.null(names(det))) {
    return(lapply(det, `[[`, 1))
  } else {
    det$type
  }
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
  stopifnot(is.type(type) || is.list(type) || is.specification(type))
  if (!is.delayed(type) && length(type$select) > 1L) {
    return(list(type = type, data = data[unorig(type$select)]))
  } else if (!is.delayed(type) && length(type$select) == 1L) {
    return(list(type = type, data = data[[unorig(type$select)]]))
  }
  UseMethod("determine")
}

#' @export
determine.default <- function(type, data, ..., spec) {
  if (is.list(type) && is.null(names(type))) {
    l <- lapply(type, determine, data = data, spec = spec)
    return(l)
  }

  type <- eval_type_names(type, data)
  type <- eval_type_select(type, data[unorig(type$names)])

  if (!is.delayed(type) && length(type$select) == 1L) {
    list(type = type, data = data[[unorig(type$select)]])
  } else {
    list(type = type, data = data[unorig(type$select)])
  }
}

#' @export
determine.colData <- function(type, data, ..., spec) {
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Requires SummarizedExperiment package from Bioconductor.")
  }
  data <- as.data.frame(colData(data))
  type <- eval_type_names(type, data)

  if (is.null(type$names) || !length(type$names)) {
    stop("No ", toString(is(type)), " meet the specification.", call. = FALSE)
  }

  type <- eval_type_select(type, data[unorig(type$names)])

  if (!is.delayed(type) && length(type$select) == 1L) {
    list(type = type, data = data[[unorig(type$select)]])
  } else {
    list(type = type, data = data[unorig(type$select)])
  }
}

#' @export
determine.specification <- function(type, data, ..., spec) {
  stopifnot(inherits(data, "qenv"))
  d <- data
  for (i in seq_along(type)) {
    di <- determine(type[[i]], d, spec = spec)
    # overwrite so that next type in line receives the corresponding data and specification
    if (is.null(di$type)) {
      next
    }
    type[[i]] <- di$type
    d <- di$data
  }
  list(type = type, data = data) # It is the transform object resolved.
}

# Checks that for the given type and data names and data it can be resolved
# The workhorse of the resolver
# determine_helper <- function(type, data_names, data) {
#   stopifnot(!is.null(type))
#   orig_names <- type$names
#   orig_select <- type$select
#
#   if (is.delayed(type) && all(is.character(type$names))) {
#     new_names <- intersect(data_names, type$names)
#
#     type$names <- new_names
#     if (length(new_names) == 0) {
#       return(NULL)
#       # stop("No selected ", is(type), " matching the conditions requested")
#     } else if (length(new_names) == 1L) {
#       type$select <- new_names
#     } else {
#       new_select <- selector(data, type$names)
#       if (!length(new_select)) {
#         return(NULL)
#         # stop("No ", is(type), " meet the requirements to be selected")
#       }
#       type$select <- new_select
#     }
#   } else if (is.delayed(type)) {
#     new_names <- selector(data, type$select)
#   }
#
#
#   if (!length(new_names)) {
#     return(NULL)
#     # stop("No ", is(type), " meet the requirements")
#   }
#   type$names <- new_names
#
#   if (length(type$names) == 0) {
#     return(NULL)
#     # stop("No selected ", is(type), " matching the conditions requested")
#   } else if (length(type$names) == 1) {
#     type$select <- type$names
#   }
#
#   new_select <- selector(data, type$select)
#   type$select <- new_select
#   attr(type$names, "original") <- orig(orig_names)
#   attr(type$select, "original") <- orig(orig_select)
#   resolved(type)
# }

#' @export
determine.datasets <- function(type, data, ...) {
  if (is.null(data)) {
    return(list(type = type, data = NULL))
  } else if (!inherits(data, "qenv")) {
    stop("Please use qenv() or teal_data() objects.")
  }

  # Assumes the object has colnames method (true for major object classes: DataFrame, tibble, Matrix, array)
  # FIXME: What happens if colnames is null: colnames(array(dim = c(4, 2)))
  type <- eval_type_names(type, data)

  if (is.null(type$names) || !length(type$names)) {
    stop("No ", toString(is(type)), " meet the specification.", call. = FALSE)
  }

  type <- eval_type_select(type, data[unorig(type$names)])

  if (!is.delayed(type) && length(type$select) == 1L) {
    list(type = type, data = data[[unorig(type$select)]])
  } else {
    list(type = type, data = data[unorig(type$select)])
  }
}

#' @export
determine.variables <- function(type, data, ...) {
  if (is.null(data)) {
    return(list(type = type, data = NULL))
  } else if (length(dim(data)) != 2L) {
    stop(
      "Can't resolve variables from this object of class ",
      toString(sQuote(class(data)))
    )
  }

  if (ncol(data) <= 0L) {
    stop("Can't pull variable: No variable is available.")
  }

  type <- eval_type_names(type, data)

  if (is.null(type$names) || !length(type$names)) {
    stop("No ", toString(is(type)), " meet the specification.", call. = FALSE)
  }

  type <- eval_type_select(type, data)

  # Not possible to know what is happening
  if (is.delayed(type)) {
    return(list(type = type, data = NULL))
  }
  # This works for matrices and data.frames of length 1 or multiple
  # be aware of drop behavior on tibble vs data.frame
  list(type = type, data = data[, type$select, drop = FALSE])
}

# @export
# determine.mae_colData <- function(type, data, ...) {
#   if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
#     stop("Requires 'MultiAssayExperiment' package.")
#   }
#
#   new_data <- colData(data)
#   for (i in seq_along(new_data)) {
#     type <- determine_helper(type, colnames(new_data)[i], new_data[, i])
#   }
#   if (length(dim(new_data)) != 2L) {
#     stop("Can't resolve variables from this object of class ", class(new_data))
#   }
#   if (ncol(new_data) <= 0L) {
#     stop("Can't pull variable: No variable is available.")
#   }
#   type <- determine_helper(type, colnames(new_data), new_data)
#
#   # Not possible to know what is happening
#   if (is.delayed(type)) {
#     return(list(type = type, data = NULL))
#   }
#
#   if (length(type$select) > 1) {
#     list(type = type, data = data[type$select])
#   } else {
#     list(type = type, data = data[[type$select]])
#   }
# }

# @export
# determine.mae_experiments <- function(type, data, ...) {
#   if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
#     stop("Requires 'MultiAssayExperiment' package.")
#   }
#   new_data <- experiments(data)
#   type <- determine_helper(type, names(new_data), new_data)
#
#   # Not possible to know what is happening
#   if (is.delayed(type)) {
#   }
#
#   if (!is.delayed(type) && length(type$select) > 1) {
#     list(type = type, data = new_data[type$select])
#   } else if (!is.delayed(type) && length(type$select) == 1) {
#     list(type = type, data = new_data[[type$select]])
#   } else {
#     return(list(type = type, data = NULL))
#   }
# }

# @export
# determine.mae_sampleMap <- function(type, data, ...) {
#   if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
#     stop("Requires 'MultiAssayExperiment' package.")
#   }
#
#   new_data <- sampleMap(data)
#   type <- determine_helper(type, names(new_data), new_data)
#
#   # Not possible to know what is happening
#   if (is.delayed(type)) {
#     return(list(type = type, data = NULL))
#   }
#
#   if (length(type$select) > 1) {
#     list(type = type, data = data[type$select])
#   } else {
#     list(type = type, data = data[[type$select]])
#   }
# }

#' @export
determine.values <- function(type, data, ...) {
  if (!is.numeric(data)) {
    d <- data
    names(d) <- data
  } else {
    d <- data
  }
  sel <- selector(d, type$names)
  type$names <- data[sel]


  sel2 <- selector(d[sel], type$select)
  type$select <- data[sel][sel2]

  # Not possible to know what is happening
  if (is.delayed(type)) {
    return(list(type = type, data = NULL))
  }

  list(type = type, data = data[sel])
}

orig <- function(x) {
  attr(x, "original")
}

unorig <- function(x) {
  attr(x, "original") <- NULL
  x
}

eval_type_names <- function(type, data) {
  orig_names <- orig(type$names)
  if (length(orig_names) == 1L) {
    orig_names <- orig_names[[1L]]
  }

  new_names <- selector(data, type$names)

  new_names <- unique(names(new_names))
  attr(new_names, "original") <- orig_names

  type$names <- new_names

  type
}

eval_type_select <- function(type, data) {
  stopifnot(is.character(type$names))
  data <- extract(data, type$names)

  orig_select <- orig(type$select)
  if (length(orig_select) == 1L) {
    orig_select <- orig_select[[1L]]
  }

  names <- seq_along(type$names)
  names(names) <- type$names
  new_select <- names(selector(data, type$select))

  attr(new_select, "original") <- orig_select
  type$select <- new_select

  type
}
