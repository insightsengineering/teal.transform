#' @rdname picks
#' @export
mae_data <- function(choices = tidyselect::everything(), selected = 1L, multiple = FALSE) {
  out <- .selected_choices(
    choices = if (.is_tidyselect(choices)) rlang::enquo(choices) else choices,
    selected = if (.is_tidyselect(selected)) rlang::enquo(selected) else selected,
    multiple = multiple
  )
  class(out) <- c("mae_data", class(out))
  out
}

#' @export
determine.mae_data <- function(x, data) {
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Requires SummarizedExperiment package from Bioconductor.")
  }
  data <- SummarizedExperiment::colData(data)
  NextMethod("determine", x)
}


#' @keywords internal
#' @export
.picker_icon.MultiAssayExperiment <- function(x) "layer-group"
