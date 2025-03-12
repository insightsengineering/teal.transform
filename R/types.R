transform <- function() {
  o <- list(datasets = na_type("datasets"),
            variables = na_type("variables"),
            values = na_type("values"))
  class(o) <- c("transform", "list")
  delay(o)
}

is.transform <- function(x) {
  inherits(x, "transform")
}

has_dataset <- function(x) {
  !anyNA(x[["datasets"]])
}

has_variable <- function(x) {
  !anyNA(x[["variables"]])
}

has_value <- function(x) {
  !anyNA(x[["values"]])
}

na_type <- function(type) {
  out <- NA_character_
  class(out) <- c(type, "type")
  out
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
    (is.list(input) && all(vapply(input, is.function, logical(1L))))
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
  o <- transform()
  o$datasets <- type_helper(x, select, type = "datasets")
  o
}


#' @export
variables <- function(x, select = first) {
  o <- transform()
  o$variables <- type_helper(x, select, type = "variables")
  o
}

#' @export
values <- function(x, select = first) {
  o <- transform()
  o$values <- type_helper(x, select, type = "values")
  o
}

#' @export
c.transform <- function(...) {
  transf <- mapply(c, ...)
  class(transf) <- c("transform", "list")
  delay(transf)
}

#' @export
c.type <- function(...) {
  c1 <- class(..1)
  c2 <- class(..2)

  if (is.null(..1)) {
    return(..2)
  } else if (is.null(..2)) {
    return(..1)
  }

  classes <- unique(c(c1, c2))
  other_classes <- setdiff(classes, c("delayed", "type", "list"))

  if ("delayed" %in% classes) {
    classes <- c("delayed", other_classes, "type", "list")
  } else {
    classes <- c(other_classes, "type", "list")
  }

  out <- NextMethod("c")

  if (all(is.na(out))) {
    return(na_type())
  } else if (anyNA(out)) {
    out <- out[!is.na(out)]
  }
  nam <- names(out)
  names <- nam == "names"
  selects <- nam == "select"

  new_l <- list(names = unlist(out[names], FALSE, FALSE),
              select = unlist(out[selects], FALSE, FALSE))

  l <- lapply(new_l, unique)
  class(l) <- classes

  attr(l$names, "original") <- unique(unlist(lapply(out[names], attr, "original"), TRUE, FALSE))
  attr(l$select, "original") <- unique(unlist(lapply(out[selects], attr, "original"), TRUE, FALSE))
  l
}

#' @export
`[.type` <- function(x, i, j, ..., exact = TRUE) {
  cx <- class(x)
  out <- NextMethod("[")
  class(out) <- cx
  out
}

#' @export
`[.type<-` <- function(x, i, j, ..., value) {
  cx <- class(x)
  if (!"type" %in% class(value)) {
    stop("Modifying the specification with invalid objects")
  }
  out <- NextMethod("[")
  class(out) <- cx
  out
}

#' @export
`[[.type` <- function(x, i, ..., drop = TRUE) {
  cx <- class(x)
  out <- NextMethod("[[")
  class(out) <- cx
  out
}


#' @export
`[[.type<-` <- function(x, i, value) {
  cx <- class(x)
  if (!"type" %in% class(value)) {
    stop("Modifying the specification with invalid objects.")
  }
  out <- NextMethod("[")
  class(out) <- cx
  out
}

#' @export
print.type <- function(x, ...) {
  is_na <- length(x) == 1L && is.na(x)
  if (is_na) {
    cat("Nothing possible")
    return(x)
  }

  nam_list <- is.list(x$names)
  if (nam_list) {
    nam_functions <- vapply(x$names, is.function, logical(1L))
  } else {
    nam_functions <- FALSE
  }

  nam_values <- length(x$names) - nam_functions
  if (nam_functions) {
    cat(sum(nam_functions), "functions for possible choices.\n")
  }
  if (nam_values) {
    cat(x$names[!nam_functions], "as possible choices.\n")
  }

  sel_list <- is.list(x$select)
  if (sel_list) {
    sel_functions <- vapply(x$select, is.function, logical(1L))
  } else {
    sel_functions <- FALSE
  }

  sel_values <- length(x$select) - sel_functions
  if (any(sel_functions)) {
    cat(sum(sel_functions), "functions to select.\n")
  }
  if (sel_values) {
    cat(x$select[!sel_functions], "selected.\n")
  }
  return(x)
}
