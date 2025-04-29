selector <- function(data, ...) {
  if (is.environment(data)) {
    data <- as.list(data)
  }
  if (is.null(names(data))) {
    stop("Can't extract the data.")
  }
  pos <- tidyselect::eval_select(rlang::expr(c(...)), data)
  pos
}
