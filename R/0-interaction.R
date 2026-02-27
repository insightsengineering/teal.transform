#' Declare interaction variable pairs for tidyselect
#'
#' Used inside tidyselect expressions to declare a pair of variables that
#' interact with each other. The pair is recorded in the selection environment
#' and the positions of both variables within the available variables are
#' returned.
#'
#' @param var1 An unquoted variable name.
#' @param var2 An unquoted variable name that interacts with `var1`.
#' @param vars Character vector of available variable names, retrieved
#'   automatically via [tidyselect::peek_vars()].
#'
#' @return An integer vector of length 2 giving the positions of `var1` and
#'   `var2` in `vars`, or `NA` where a variable is not found.
#'
#' @export
interaction_vars <- function(var1, var2, vars = tidyselect::peek_vars(fn = "my_helper")) {
  interaction_vars <- c(as.character(substitute(var1)), as.character(substitute(var2)))
  result <- vctrs::vec_match(interaction_vars, vars)
  select_env$interaction_vars <- c(select_env$interaction_vars %||% list(), list(interaction_vars)) # Option 2
  result
}

# Environment to store interaction variable pairs during tidyselect evaluation
# This is used to communicate between the `interaction_vars()` function and the resolver that
# processes the picks with variables that interact.
# The resolver will look for this information in the environment to know which variables are
# meant to interact and need to be combined in the data.
select_env <- new.env(parent = emptyenv())

#' Find all `interactive_vars` calls in an expression
#'
#' Traverses an expression tree using a breadth-first search and collects the
#' arguments of every `interactive_vars()` call found.
#'
#' @param expr An R expression or quosure to search.
#'
#' @return A list of argument lists, one element per `interactive_vars()` call
#'   found. Each element is a list of the unevaluated arguments passed to
#'   `interactive_vars()`.
#'
#' @noRd
.find_interactive_vars <- function(expr) {
  expr <- if (rlang::is_quosure(expr)) rlang::quo_get_expr(expr) else expr

  queue <- list(expr)
  results <- list()

  while (length(queue) > 0) {
    node <- queue[[1]]
    queue <- queue[-1]

    if (rlang::is_call(node, "interaction_vars")) {
      results <- c(results, list(as.list(node)[-1]))
    } else if (is.call(node)) {
      # Add all child nodes to the queue
      queue <- c(queue, as.list(node)[-1])
    }
  }

  results

}
