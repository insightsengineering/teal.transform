#' @export
`&.transform` <- function(e1, e2) {
  if (!is.transform(e1) || !is.transform(e2)) {
    stop("Method not available")
  }
  o <- transform()
  if (has_dataset(e1) || has_dataset(e2)) {
    o$datasets <- c(e1$datasets, e2$datasets)
    o$datasets <- o$datasets[!is.na(o$datasets)]
  }
  if (has_variable(e1) || has_variable(e2)) {
    o$variables <- c(e1$variables, e2$variables)
    o$variables <- o$variables[!is.na(o$variables)]
  }
  if (has_value(e1) || has_value(e2)) {
    o$values <- c(e1$values, e2$values)
    o$values <- o$values[!is.na(o$values)]
  }

  class(o) <- c("delayed", "transform")
  o
}

#' @export
`|.dataset` <- function(e1, e2) {
  if (!is.transform(e1) || !is.transform(e2)) {
    stop("Method not available")
  }
  s <- transform()
  class(x) <- c("delayed", "transform")
  x
}

# #' @export
# chooseOpsMethod.transform <- function(x, y, mx, my, cl, reverse) {
#   # cat("\nx\n")
#   # print(mx)
#   # cat("\ny\n")
#   # print(my)
#   # cat("\ncl\n")
#   # print(cl)
#   # cat("\nreverse\n")
#   # print(reverse)
#   is.transform(x)
# }

# ?Ops
