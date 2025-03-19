is.transform <- function(x) {
  inherits(x, "transform")
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

first <- function(x){
  if (length(x) > 0) {
    false <- rep(FALSE, length.out = length(x))
    false[1] <- TRUE
    return(false)
  }
  return(FALSE)
}

check_input <- function(input) {
  is.character(input) || is.function(input) ||
    (is.list(input) && all(vapply(input, function(x){is.function(x) || is.character(x)}, logical(1L))))
}

type_helper <- function(x, select, type) {
  stopifnot("Invalid options" = check_input(x),
            "Invalid selection" = check_input(type))
  if (is.function(x)) {
    x <- list(x)
  }
  if (is.function(select)) {
    select <- list(select)
  }
  out <- list(names = x, select = select)
  class(out) <- c(type, "type", "list")
  attr(out$names, "original") <- x
  attr(out$select, "original") <- select
  delay(out)
}

#' @export
datasets <- function(x, select = first) {
  type_helper(x, select, type = "datasets")
}


#' @export
variables <- function(x, select = first) {
  type_helper(x, select, type = "variables")
}

#' @export
values <- function(x, select = first) {
  type_helper(x, select, type = "values")
}

# #' @export
# c.type <- function(...) {
#
#   if (is.na(..1)) {
#     return(..2)
#   } else if (is.na(..2)) {
#     return(..1)
#   }
#
#   if (...length() > 2L) {
#     stop("We can't combine this (yet)")
#   } else if (all(class(..2) != class(..1))) {
#     type_out <- ..1
#     type_out$child <- ..2
#     return(type_out)
#   }
#   out <- mapply(c, ..., SIMPLIFY = FALSE)
#   out <- lapply(out, unique)
#   class(out) <- c("transform", class(out))
#   delay(out)
# }

#' @export
c.transform <- function(...) {
  l <- list(...)
  types <- lapply(l, names)
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
        old_names), orig(l[[i]][[t]][["names"]]))
      new_type$select <- c(old_select, l[[i]][[t]][["select"]])
      attr(new_type$select, "original") <- c(orig(old_select), orig(l[[i]][[t]][["select"]]))
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
  utypes <- unique(unlist(types, FALSE, FALSE))
  vector <- vector("list", length(utypes))
  names(vector) <- utypes
  for (t in utypes) {
    new_type <- vector("list", length = 2)
    names(new_type) <- c("names", "select")
    for (i in seq_along(l)) {
      if (!t %in% names(l[[i]])) {
        next
      }
      old_names <- new_type$names
      old_select <- new_type$select
      new_type$names <- c(old_names, l[[i]][[t]][["names"]])
      attr(new_type$names, "original") <- c(orig(
        old_names), orig(l[[i]][[t]][["names"]]))
      new_type$select <- c(old_select, l[[i]][[t]][["select"]])
      attr(new_type$select, "original") <- c(orig(old_select), orig(l[[i]][[t]][["select"]]))
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

  nam_list <- is.list(x$names)
  if (nam_list) {
    nam_functions <- vapply(x$names, is.function, logical(1L))
  } else {
    nam_functions <- FALSE
  }

  msg_values <- character()
  nam_values <- length(x$names) - sum(nam_functions)
  if (any(nam_functions)) {
    msg_values <- paste0(msg_values, sum(nam_functions), " functions for possible choices.",
                      collapse = "\n")
  }
  if (nam_values) {
    msg_values <- paste0(msg_values, paste0(sQuote(x$names[!nam_functions]), collapse = ", "),
                         " as possible choices.", collapse = "\n")
  }

  sel_list <- is.list(x$select)
  if (sel_list) {
    sel_functions <- vapply(x$select, is.function, logical(1L))
  } else {
    sel_functions <- FALSE
  }

  msg_sel <- character()
  sel_values <- length(x$select) - sum(sel_functions)
  if (any(sel_functions)) {
    msg_sel <- paste0(msg_sel, sum(sel_functions), " functions to select.",
                      collapse = "\n")
  }
  if (sel_values) {
    msg_sel <- paste0(msg_sel, paste0(sQuote(x$select[!sel_functions]), collapse = ", "),
                      " selected.", collapse = "\n")
  }
  cat(msg_values,  msg_sel)
  return(x)
}
