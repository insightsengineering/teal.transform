selector <- function(data, ...) {
  if (is.environment(data)) {
    # To keep the "order" of the names in the extraction: avoids suprises
    data <- as.list(data)[names(data)]
  } else if (length(dim(data)) == 2L) {
    data <- as.data.frame(data)
  }

  if (is.null(names(data))) {
    stop("Can't extract the data.")
  }
  pos <- tidyselect::eval_select(expr = ..., data)
  pos
}
