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
         "|" = combine_transform(e1, e2),
         "&" = c(e1, e2),
         stop("Method ", sQuote(.Generic), " not implemented for this class ", .Class, ".", call. = FALSE))
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
         # "|" = ,
         "&" = c(e1, e2),
         stop("Method ", sQuote(.Generic), " not implemented for this class ", .Class, ".", call. = FALSE))
  class(out) <- class(e1)
  out
}

combine_transform <- function(e1, e2) {
  l <- list(e1, e2)
  class(l) <- c("transform", "list")
  l
}
