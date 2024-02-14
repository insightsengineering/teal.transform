#' Include `CSS` files from `/inst/css/` package directory to application header
#'
#' `system.file` should not be used to access files in other packages, it does
#' not work with `devtools`.
#' As a result, this method is individually redefined as required in each package.
#' Therefore, this function is not exported.
#'
#' @param pattern (`character`) pattern of files to be included.
#'
#' @return HTML code that includes `CSS` files.
#'
#' @keywords internal
#'
include_css_files <- function(pattern = "*") {
  css_files <- list.files(
    system.file("css", package = "teal.transform", mustWork = TRUE),
    pattern = pattern, full.names = TRUE
  )
  singleton(lapply(css_files, includeCSS))
}
