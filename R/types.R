transform <- function() {
  o <- list(datasets = na_type(), variables = na_type(), values = na_type())
  class(o) <- c("delayed", "transform")
  o
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

na_type <- function() {
  out <- NA_character_
  class(out) <- c("type", class(out))
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

#' @export
datasets <- function(x, select = first) {
  stopifnot(is.character(x) || is.function(x) || (is.list(x) && all(vapply(x, is.function, logical(1L)))))
  stopifnot(is.character(select) || is.function(select) || (is.list(select) && all(vapply(select, is.function, logical(1L)))))
  if (is.function(x)) {
    x <- list(x)
  }
  if (is.function(select)) {
    select <- list(select)
  }
  type <- list(names = x, select = select)
  class(type) <- c("delayed", "datasets", "type", "list")
  attr(type$names, "original") <- x
  attr(type$select, "original") <- select
  o <- list(datasets = type, variables = na_type(), values = na_type())
  class(o) <- c("delayed", "transform", "list")
  o
}


#' @export
variables <- function(x, select = first) {
  stopifnot(is.character(x) || is.function(x) || (is.list(x) && all(vapply(x, is.function, logical(1L)))))
  stopifnot(is.character(select) || is.function(select) || (is.list(select) && all(vapply(select, is.function, logical(1L)))))
  if (is.function(x)) {
    x <- list(x)
  }
  if (is.function(select)) {
    select <- list(select)
  }
  type <- list(names = x, select = select)
  class(type) <- c("delayed", "variables", "type", "list")
  attr(type$names, "original") <- x
  attr(type$select, "original") <- select
  o <- list(datasets = na_type(), variables = type, values = na_type())
  class(o) <- c("delayed", "transform")
  o
}

#' @export
values <- function(x, select = first) {
  stopifnot(is.character(x) || is.function(x) || (is.list(x) && all(vapply(x, is.function, logical(1L)))))
  stopifnot(is.character(select) || is.function(select) || (is.list(select) && all(vapply(select, is.function, logical(1L)))))
  if (is.function(x)) {
    x <- list(x)
  }
  if (is.function(select)) {
    select <- list(select)
  }
  type <- list(names = x, select = select)
  class(type) <- c("delayed", "values", "type", "list")
  attr(type$names, "original") <- x
  attr(type$select, "original") <- select
  o <- list(datasets = na_type(), variables = na_type(), values = type)
  class(o) <- c("delayed", "transform")
  o
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
