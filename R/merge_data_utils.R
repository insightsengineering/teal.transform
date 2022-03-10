check_merge_function <- function(merge_function) {
  checkmate::assert_string(merge_function)
  stopifnot(length(intersect(methods::formalArgs(eval(rlang::parse_expr(merge_function))), c("x", "y", "by"))) == 3)
}
