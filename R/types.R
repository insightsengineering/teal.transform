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
  if (...length() > 2) {
    stop("More than two specifications won't be considered. Use & to combine them", call. = FALSE)
  }
  transf <- mapply(c, ..., SIMPLIFY = FALSE)
  class(transf) <- c("transform", "list")
  delay(transf)
}

#' @export
c.type <- function(...) {

  if (length(..1) == 1L && is.na(..1)) {
    return(..2)
  } else if (length(..2) == 1L && is.na(..2)) {
    return(..1)
  }

  objects <- list(...)
  classes <- unlist(lapply(objects, class), FALSE,FALSE)
  type <- setdiff(classes, c("type", "list"))
  if (length(type) > 1L) {
    stop("Combining different types", call. = FALSE)
  }

  names <- lapply(objects, "[[", i = "names")
  select <- lapply(objects, "[[", i = "select")
  names_orig <- lapply(names, orig)
  select_orig <- lapply(select, orig)
  type_f <- match.fun(type)
  type_out <- type_f(x = simplify_c(names_orig),
                     select = simplify_c(select_orig))
  attr(type_out[[type]][["names"]], "original") <- NULL
  attr(type_out[[type]][["names"]], "original") <- simplify_c(names_orig)
  attr(type_out[[type]][["select"]], "original") <- NULL
  attr(type_out[[type]][["select"]], "original") <-  simplify_c(select_orig)
  delay(type_out[[type]])
}

simplify_c <- function(x) {
  unique(unlist(x, FALSE, FALSE))
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

  msg_values <- character()
  nam_values <- length(x$names) - nam_functions
  if (any(nam_functions)) {
    msg_values <- paste0(msg_values, sum(nam_functions), " functions for possible choices.",
                      collapse = "\n")
  }
  if (nam_values) {
    msg_values <- paste0(msg_values, x$names[!nam_functions], " as possible choices.",
                     collapse = "\n")
  }

  sel_list <- is.list(x$select)
  if (sel_list) {
    sel_functions <- vapply(x$select, is.function, logical(1L))
  } else {
    sel_functions <- FALSE
  }

  msg_sel <- character()
  sel_values <- length(x$select) - sel_functions
  if (any(sel_functions)) {
    msg_sel <- paste0(msg_sel, sum(sel_functions), " functions to select.",
                      collapse = "\n")
  }
  if (sel_values) {
    msg_sel <- paste0(msg_sel, x$select[!sel_functions], "selected.",
        collapse = "\n")
  }
  cat(msg_values,  msg_sel)
  return(x)
}
