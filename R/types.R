is.transform <- function(x) {
  inherits(x, "transform")
}


valid_transform <- function(x) {
  !((is.type(x) || is.transform(x)) || or.transform(x))
}

na_type <- function(type) {
  out <- NA_character_
  class(out) <- c(type, "type")
  out
}

is.type <- function(x) {
  inherits(x, "type")
}

#' @export
#' @method is.na type
is.na.type <- function(x) {
  anyNA(unclass(x[c("names", "select")]))
}

#' @export
anyNA.type <- function(x, recursive = FALSE) {
  anyNA(unclass(x[c("names", "select")]), recursive)
}

first <- function(x) {
  if (length(x) > 0) {
    false <- rep_len(FALSE, length.out = length(x))
    false[1] <- TRUE
    return(false)
  }
  return(FALSE)
}

check_input <- function(input) {
  is.character(input) || is.function(input) ||
    (is.list(input) && all(vapply(input, function(x) {
      is.function(x) || is.character(x)
    }, logical(1L))))
}

type_helper <- function(x, select, type) {
  out <- list(names = x, select = select)
  class(out) <- c(type, "type", "list")
  attr(out$names, "original") <- x
  attr(out$select, "original") <- select
  delay(out)
}

#' @rdname types
#' @name Types
#' @title Type specification
#' @description
#' Define how to select and extract data
#' @param x Character specifying the names or functions to select them. The functions will be applied on the data or the names.
#' @param select Character of `x` or functions to select on x (only on names or positional not on the data of the variable).
#' @returns An object of the same class as the function with two elements: names the content of x, and select.
#' @examples
#' datasets("A")
#' datasets("A") | datasets("B")
#' datasets(is.data.frame)
#' datasets("A") & variables(is.numeric)
NULL

#' @describeIn types Specify datasets.
#' @export
datasets <- function(x, select = everything()) {
  type_helper(x = rlang::enquo(x), select = rlang::enquo(select), type = "datasets")
}

#' @describeIn types Specify variables.
#' @export
variables <- function(x, select = everything()) {
  type_helper(x = rlang::enquo(x), select = rlang::enquo(select), type = "variables")
}

#' @describeIn types Specify colData of SummarizedExperiment and derived classes.
#' @export
colData <- function(x, select = everything()) {
  type_helper(x = rlang::enquo(x), select = rlang::enquo(select), type = "colData")
}

#' @describeIn types Specify values.
#' @export
values <- function(x, select = everything()) {
  type_helper(x = rlang::enquo(x), select = rlang::enquo(select), type = "values")
}

#' @export
c.transform <- function(...) {
  l <- list(...)
  types <- lapply(l, names)
  typesc <- vapply(l, is.transform, logical(1L))
  if (!all(typesc)) {
    stop("An object in position ", which(!typesc), " is not a specification.")
  }
  utypes <- unique(unlist(types, FALSE, FALSE))
  vector <- vector("list", length(utypes))
  names(vector) <- utypes
  for (t in utypes) {
    new_type <- vector("list", length = 2)
    names(new_type) <- c("names", "select")
    class(new_type) <- c("type", "list")
    for (i in seq_along(l)) {
      if (!t %in% names(l[[i]])) {
        next
      }
      # Slower but less code duplication:
      # new_type <- c(new_type, l[[i]][[t]])
      # then we need class(new_type) <- c(t, "type", "list") outside the loop
      old_names <- new_type$names
      old_select <- new_type$select
      new_type$names <- c(old_names, l[[i]][[t]][["names"]])
      attr(new_type$names, "original") <- c(orig(
        old_names
      ), orig(l[[i]][[t]][["names"]]))
      new_type$select <- c(old_select, l[[i]][[t]][["select"]])
      attr(new_type$select, "original") <- c(orig(old_select), orig(l[[i]][[t]][["select"]]))
      attr(new_type, "delayed") <- any(attr(new_type, "delayed"), attr(l[[i]], "delayed"))
    }
    orig_names <- unique(orig(new_type$names))
    new_type$names <- unique(new_type$names)
    attr(new_type$names, "original") <- orig_names

    orig_select <- unique(orig(new_type$select))
    new_type$select <- unique(new_type$select)
    attr(new_type$select, "original") <- orig_select
    class(new_type) <- c(t, "type", "list")
    vector[[t]] <- new_type
  }
  class(vector) <- c("transform", "list")
  vector
}

#' @export
c.type <- function(...) {
  l <- list(...)
  types <- lapply(l, is)
  typesc <- vapply(l, is.type, logical(1L))
  if (!all(typesc)) {
    stop("An object in position ", which(!typesc), " is not a type.")
  }
  utypes <- unique(unlist(types, FALSE, FALSE))
  vector <- vector("list", length(utypes))
  names(vector) <- utypes
  for (t in utypes) {
    new_type <- vector("list", length = 2)
    names(new_type) <- c("names", "select")
    for (i in seq_along(l)) {
      if (!is(l[[i]], t)) {
        next
      }
      old_names <- new_type$names
      old_select <- new_type$select
      new_type$names <- c(old_names, l[[i]][["names"]])
      attr(new_type$names, "original") <- c(orig(
        old_names
      ), orig(l[[i]][["names"]]))
      new_type$select <- unique(c(old_select, l[[i]][["select"]]))
      attr(new_type$select, "original") <- c(orig(old_select), orig(l[[i]][["select"]]))
    }
    orig_names <- unique(orig(new_type$names))
    orig_select <- unique(orig(new_type$select))
    new_type$names <- unique(new_type$names)
    attr(new_type$names, "original") <- orig_names

    # From the possible names apply the original function
    if (is.delayed(new_type)) {
      new_type$select <- functions_names(orig(new_type$select), new_type$names)
    }

    attr(new_type$select, "original") <- orig_select

    class(new_type) <- c(t, "type", "list")
    attr(new_type, "delayed") <- is.delayed(new_type)
    vector[[t]] <- new_type
  }
  if (length(vector) == 1) {
    return(vector[[1]])
  }
  class(vector) <- c("transform", "list")
  vector
}

simplify_c <- function(x) {
  unique(unlist(x, FALSE, FALSE))
}

#' @export
print.type <- function(x, ...) {
  if (is.na(x)) {
    cat("Nothing possible")
    return(x)
  }

  nam_functions <- count_functions(x$names)

  msg_values <- character()
  nam_values <- length(x$names) - sum(nam_functions)
  if (any(nam_functions)) {
    msg_values <- paste0(msg_values, sum(nam_functions), " functions for possible choices.",
      collapse = "\n"
    )
  }
  if (nam_values) {
    msg_values <- paste0(msg_values, paste0(sQuote(x$names[!nam_functions]), collapse = ", "),
      " as possible choices.",
      collapse = "\n"
    )
  }

  sel_functions <- count_functions(x$select)

  msg_sel <- character()
  sel_values <- length(x$select) - sum(sel_functions)
  if (any(sel_functions)) {
    msg_sel <- paste0(msg_sel, sum(sel_functions), " functions to select.",
      collapse = "\n"
    )
  }
  if (sel_values) {
    msg_sel <- paste0(msg_sel, paste0(sQuote(x$select[!sel_functions]), collapse = ", "),
      " selected.",
      collapse = "\n"
    )
  }
  if (!is.null(x[["except"]])) {
    exc_functions <- count_functions(x$except)
    msg_exc <- character()
    sel_values <- length(x$except) - sum(exc_functions)
    if (any(exc_functions)) {
      msg_exc <- paste0(msg_exc, sum(exc_functions), " functions to exclude.",
        collapse = "\n"
      )
    }
    if (sel_values) {
      msg_exc <- paste0(msg_exc, paste0(sQuote(x$except[!exc_functions]), collapse = ", "),
        " excluded.",
        collapse = "\n"
      )
    }
  } else {
    msg_exc <- character()
  }

  cat(msg_values, msg_sel, msg_exc)
  return(x)
}

count_functions <- function(x) {
  if (is.list(x)) {
    vapply(x, is.function, logical(1L))
  } else {
    FALSE
  }
}
