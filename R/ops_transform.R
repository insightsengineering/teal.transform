#' @export
`&.transform` <- function(e1, e2) {

  if (is.transform(e1) && is.dataset(e2)) {
    o <- e1
    o$dataset <- unique(c(e1$dataset, e2$dataset))
  } else if (is.transform(e1) && is.variable(e2)) {
    o <- e1
    o$variable <- unique(c(e1$variable, e2$variable))
  } else if (is.transform(e1) && is.value(e2)) {
    o <- e1
    o$value <- unique(c(e1$variable, e2$value))
  }
  class(o) <- c("delayed", "transform")
  o
}

#' @export
`&.dataset` <- function(e1, e2) {
  e1_var <- e1[["names"]]
  e2_var <- e2[["names"]]

  if (is.character(e1_var) && is.character(e2_var)) {
    x <- list(dataset = unique(c(e1_var, e2_var)))
  }
  class(x) <- c("delayed", "transform")
  x
}


#' @export
`&.variable` <- function(e1, e2) {
  dataset_n_var <- is.dataset(e1) && is.variable(e2)
  e1_var <- e1[["names"]]
  e2_var <- e2[["names"]]

  if (dataset_n_var && is.character(e1_var) && is.character(e2_var)) {
    x <- list(dataset = e1_var, variable = e2_var)
  }
  if (is.variable(e1) && is.variable(e2)) {
    x <- list(dataset = NA, variable = unique(c(e1_var, e2_var)))
  }

  if (is.variable(e1) && is.dataset(e2)) {
    x <- list(dataset = e2_var, variable = e1_var)
  }
  if (is.variable(e2) && is.dataset(e1)) {
    x <- list(dataset = e1_var, variable = e2_var)
  }
  class(x) <- c("delayed", "transform")
  x
}

#' @export
`|.dataset` <- function(e1, e2) {
  list()


  class(x) <- c("delayed", "transform")
  x
}

#' @export
chooseOpsMethod.transform <- function(x, y, mx, my, cl, reverse) {
  !is.transform(x)
}

#' @export
chooseOpsMethod.dataset <- function(x, y, mx, my, cl, reverse) {
  # cat("\nx\n")
  # print(mx)
  # cat("\ny\n")
  # print(my)
  # cat("\ncl\n")
  # print(cl)
  # cat("\nreverse\n")
  # print(reverse)
  is.transform(x)
}

#' @export
chooseOpsMethod.variable <- function(x, y, mx, my, cl, reverse) TRUE

#' @export
chooseOpsMethod.value <- function(x, y, mx, my, cl, reverse) TRUE

# ?Ops
