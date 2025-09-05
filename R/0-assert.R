assert_picks <- function(x) {

}

assert_variables <- function(x, multiple, .var.name = checkmate::vname(x)) {
  if (!inherits(x, "variables")) {
    stop(.var.name, " should be of class variables")
  }

  checkmate::assert_flag(multiple)
  if (!missing(multiple) && !identical(isTRUE(attr(x, "multiple")), multiple)) {
    stop(.var.name, " should have a property multiple = `", multiple, "`.")
  }
}
