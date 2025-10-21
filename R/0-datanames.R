#' Extract dataset names from picks objects
#'
#' `datanames()` extracts the names of all datasets referenced in one or more `picks` objects.
#' This is useful for determining which datasets need to be available in the data environment
#' before a module can function properly.
#'
#' @param x (`picks` object, a list of `picks`)
#'
#' @return A character vector of unique dataset names. Only returns names when dataset choices
#'   are specified as character vectors (static choices). Returns `NULL` or empty vector when
#'   datasets are specified using `tidyselect` expressions (dynamic choices), since the actual
#'   dataset names cannot be determined until runtime.
#'
#' @details
#' The function examines the `datasets()` component of each `picks` object and extracts
#' dataset names only when they are explicitly specified as character vectors. This allows
#' modules to declare their data dependencies upfront.
#'
#' ## Behavior with different choice types
#'
#' - **Static choices**: When `datasets(choices = c("iris", "mtcars"))` uses character vectors,
#'   `datanames()` returns `c("iris", "mtcars")`.
#'
#' - **Dynamic choices**: When `datasets(choices = tidyselect::everything())` or other
#'   tidyselect expressions are used, `datanames()` cannot determine the dataset names in
#'   advance and returns an empty result.
#'
#' - **Mixed lists**: When processing multiple `picks` objects, only the statically defined
#'   dataset names are extracted and combined.
#'
#' @examples
#' # Single picks object with one dataset
#' p1 <- picks(
#'   datasets(choices = "iris", selected = "iris"),
#'   variables(choices = tidyselect::everything(), selected = 1)
#' )
#' datanames(p1) # Returns "iris"
#'
#' # Single picks object with multiple datasets
#' p2 <- picks(
#'   datasets(choices = c("iris", "mtcars"), selected = "iris"),
#'   variables(choices = tidyselect::where(is.numeric), selected = 1)
#' )
#' datanames(p2) # Returns c("iris", "mtcars")
#'
#' # List of picks objects
#' p3 <- picks(
#'   datasets(choices = c("chickwts", "PlantGrowth"), selected = 1),
#'   variables(choices = tidyselect::everything(), selected = 1)
#' )
#' datanames(list(p1, p2, p3)) # Returns c("iris", "mtcars", "chickwts", "PlantGrowth")
#'
#' # Dynamic choices - cannot determine dataset names
#' p4 <- picks(
#'   datasets(choices = tidyselect::where(is.data.frame), selected = 1),
#'   variables(choices = tidyselect::everything(), selected = 1)
#' )
#' datanames(p4) # Returns NULL or empty vector
#'
#' # List with NULL values (filtered out automatically)
#' datanames(list(p1, NULL, p2)) # Returns c("iris", "mtcars")
#'
#' # Duplicate dataset names are removed
#' datanames(list(p1, p1, p2)) # Returns c("iris", "mtcars") - no duplicates
#'
#' @seealso [picks()], [datasets()]
#' @export
datanames <- function(x) {
  if (inherits(x, "picks")) {
    x <- list(x)
  }
  checkmate::assert_list(x, c("picks", "NULL"))
  unique(unlist(lapply(x, function(x) {
    if (is.character(x$datasets$choices)) x$datasets$choices
  })))
}
