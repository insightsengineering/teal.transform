selector <- function(data, ...) {
  if (is.environment(data)) {
    data <- as.list(data)
  } else if (length(dim(data)) == 2L) {
    data <- as.data.frame(data)
  }

  if (is.null(names(data))) {
    stop("Can't extract the data.")
  }
  pos <- tidyselect::eval_select(expr = ..., data)
  pos
}
