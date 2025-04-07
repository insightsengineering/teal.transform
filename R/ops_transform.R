#' @export
Ops.transform <- function(e1, e2) {
  if (missing(e2)) {
    # out <- switch(.Generic,
    # "!" = Negate,
    stop("Method ", sQuote(.Generic), " not implemented for this class ", .Class, ".", call. = FALSE)
    # return(out)
  }
  switch(.Generic,
    "!=" = NextMethod(),
    "==" = NextMethod(),
    "|" = or_transform(e1, e2),
    "&" = nd_transform(e1, e2),
    stop("Method ", sQuote(.Generic), " not implemented for this class ", .Class, ".", call. = FALSE)
  )
}

#' @export
Ops.type <- function(e1, e2) {
  if (missing(e2)) {
    # out <- switch(.Generic,
    #        "!" = Negate,
    stop("Method ", sQuote(.Generic), " not implemented for this class ", .Class, ".", call. = FALSE)
    # return(out)
  }
  out <- switch(.Generic,
    "!=" = NextMethod(),
    # "==" = NextMethod(),
    "|" = or_type(e1, e2),
    "&" = nd_type(e1, e2),
    stop("Method ", sQuote(.Generic), " not implemented for this class ", .Class, ".", call. = FALSE)
  )
  out
}

or_transform <- function(e1, e2) {
  if (is.transform(e1) && is.type(e2) && !is.transform(e2)) {
    opt2 <- e1 & e2
    out <- list(e1, opt2)
  } else if (!is.transform(e1) && is.type(e1) && is.transform(e2)) {
    opt2 <- e2 & e1
    out <- list(e2, opt2)
  } else if (is.transform(e1) && is.transform(e2)) {
    out <- list(e1, e2)
  } else {
    stop("Missing implementation method.")
  }
  # FIXME: Should we signal it is a transform or just a list of transform is enough?
  # class(out) <- c("transform", "list")
  out
}

nd_transform <- function(e1, e2) {
  if (is.transform(e1) && is.transform(e2)) {
    types <- intersect(names(e1), names(e2))
    for (t in types) {
      e1[[t]] <- unique(c(e1[[t]], e2[[t]]))
    }
    return(e1)
  }

  if (is.type(e1) && is.transform(e2)) {
    if (!is(e1) %in% names(e2)) {
      e2[[is(e1)]] <- e1
    } else {
      e2[[is(e1)]] <- c(e2[[is(e1)]], e1)
    }
    return(e2)
  } else if (is.transform(e1) && is.type(e2)) {
    if (!is(e2) %in% names(e1)) {
      e1[[is(e2)]] <- e2
    } else {
      e1[[is(e2)]] <- c(e1[[is(e2)]], e2)
    }
    out <- e1
  } else if (is.type(e1) && is.transform(e2)) {
    out <- rev(c(e2, e1)) # To keep order in the list
  } else {
    stop("Method not implemented yet!")
  }
  out
}

nd_type <- function(e1, e2) {
  if (is.transform(e1) && !is.transform(e2)) {
    out <- c(e1, list(e2))
    names(out)[length(out)] <- is(e2)
  } else if (!is.transform(e1) && is.transform(e2)) {
    out <- c(e2, list(e1))
    names(out)[length(out)] <- is(e1)
  } else if (is.transform(e1) && is.transform(e2)) {
    out <- c(e1, e2)
  } else if (is.type(e1) && is.type(e2)) {
    out <- c(e1, e2)
  } else if (or.transform(e1) && is.type(e2) ){
    out <- lapply(e1, nd_type, e2 = e2)
  } else if (or.transform(e2) && is.type(e1) ){
    out <- lapply(e2, nd_type, e2 = e1)
  } else {
    stop("Maybe we should decide how to apply a type to a list of transformers...")
  }
  class(out) <- unique(c("transform", class(out)))
  out
}

or_type <- function(e1, e2) {
  substitute <- is(e2) %in% names(e1)
  if (substitute) {
    out <- e1
    e1[[is(e2)]] <- e2
    return(nd_type(out, e1))
  }
  list(e1, e2)
}


# chooseOpsMethod.list <- function(x, y, mx, my, cl, reverse) TRUE
#' @export
chooseOpsMethod.transform <- function(x, y, mx, my, cl, reverse) {
  # Apply one or other method
  # !is.transform(x)
  TRUE
}
# chooseOpsMethod.type <- function(x, y, mx, my, cl, reverse) TRUE
