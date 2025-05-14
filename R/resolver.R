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

  stopifnot(is.list(spec) || is.specification(spec))
  if (is.type(spec)) {
    spec <- list(spec)
    names(spec) <- is(spec[[1]])
    class(spec) <- c("specification", class(spec))
  }

  det <- determine(spec, data)
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
  if (!is.delayed(type)) {
    return(list(type = type, data = extract(data, unorig(type$selected))))
  }
  UseMethod("determine")
}

#' @export
determine.default <- function(type, data, ...) {
  stop("There is not a specific method to pick choices.")
}

#' @export
determine.list <- function(type, data, ...) {
  if (is.list(type) && is.null(names(type))) {
    l <- lapply(type, determine, data = data)
    return(l)
  }

  type <- eval_type_names(type, data)
  type <- eval_type_select(type, data)

  list(type = type, data = extract(data, unorig(type$selected)))
}

#' @export
determine.colData <- function(type, data, ...) {
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Requires SummarizedExperiment package from Bioconductor.")
  }
  data <- as.data.frame(colData(data))
  type <- eval_type_names(type, data)

  if (is.null(type$choices) || !length(type$choices)) {
    stop("No ", toString(is(type)), " meet the specification.", call. = FALSE)
  }

  type <- eval_type_select(type, data)

  list(type = type, data = extract(data, unorig(type$selected)))
}

#' @export
determine.specification <- function(type, data, ...) {
  stopifnot(inherits(data, "qenv"))

  # Adding some default specifications if they are missing
  if ("values" %in% names(type) && !"variables" %in% names(type)) {
    type <- append(type, list(variables = variables()), length(type) - 1)
  }

  if ("variables" %in% names(type) && !"datasets" %in% names(type)) {
    type <- append(type, list(variables = datasets()), length(type) - 1)
  }

  d <- data
  for (i in seq_along(type)) {
    di <- determine(type[[i]], d)
    # overwrite so that next type in line receives the corresponding data and specification
    if (is.null(di$type)) {
      next
    }
    type[[i]] <- di$type
    d <- di$data
  }
  list(type = type, data = data) # It is the transform object resolved.
}

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

  if (is.null(type$choices) || !length(type$choices)) {
    stop("No ", toString(is(type)), " meet the specification.", call. = FALSE)
  }

  type <- eval_type_select(type, data)

  list(type = type, data = extract(data, unorig(type$selected)))
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

  if (is.null(type$choices) || !length(type$choices)) {
    stop("No ", toString(is(type)), " meet the specification.", call. = FALSE)
  }

  type <- eval_type_select(type, data)

  # Not possible to know what is happening
  if (is.delayed(type)) {
    return(list(type = type, data = NULL))
  }
  # This works for matrices and data.frames of length 1 or multiple
  # be aware of drop behavior on tibble vs data.frame
  list(type = type, data = extract(data, unorig(type$selected)))
}

#' @export
determine.values <- function(type, data, ...) {
  if (!is.numeric(data)) {
    d <- data
    names(d) <- data
  } else {
    d <- data
  }
  sel <- selector(d, type$choices)
  type$choices <- data[sel]


  sel2 <- selector(d[sel], type$selected)
  type$selected <- data[sel][sel2]

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
  orig_choices <- orig(type$choices)
  if (length(orig_choices) == 1L) {
    orig_choices <- orig_choices[[1L]]
  }

  new_choices <- selector(data, type$choices)

  new_choices <- unique(names(new_choices))
  attr(new_choices, "original") <- orig_choices

  type$choices <- new_choices

  type
}

eval_type_select <- function(type, data) {
  stopifnot(is.character(type$choices))
  if (!is(data, "qenv")) {
    data <- extract(data, type$choices)
  } else {
    # Do not extract; selection would be from the data extracted not from the names.
    data <- data[type$choices]
  }
  orig_selected <- orig(type$selected)
  if (length(orig_selected) == 1L) {
    orig_selected <- orig_selected[[1L]]
  }

  choices <- seq_along(type$choices)
  names(choices) <- type$choices
  new_selected <- names(selector(data, type$selected))

  attr(new_selected, "original") <- orig_selected
  type$selected <- new_selected

  type
}
