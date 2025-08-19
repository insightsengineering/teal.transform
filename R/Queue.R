# Queue ====

#' R6 Class - A First-In-First-Out Abstract Data Type
#' @docType class
#'
#' @description
#'
#' Abstract data type that stores and returns any number of elements.
#'
#' @details
#' A `Queue` object stores all elements in a single vector,
#' thus all data types can be stored, but silent coercion may occur.
#'
#' Elements are returned in the same order that they were added.
#'
#' @name Queue
#' @keywords internal
#'
Queue <- R6::R6Class( # nolint: object_name_linter.
  classname = "Queue",
  # public methods ----
  public = list(
    #' @description
    #' Adds element(s) to `Queue`.
    #'
    #' @param new_elements vector of elements to add.
    #'
    #' @return `self`, invisibly.
    #'
    push = function(new_elements) {
      for (i in seq_along(new_elements)) {
        # new_elements[i] does not discard names if it's a named list
        private$array <- append(private$array, new_elements[i])
      }

      invisible(self)
    },
    #' @description
    #' Returns all contents of the `Queue` object.
    #'
    #' @return Single vector containing all `Queue` contents.
    #'
    get = function() {
      private$array
    },
    #' @description
    #' Returns the first (oldest) element of the `Queue` and removes it.
    #'
    #' @return vector of length 1 containing the first element of `Queue`
    #' or `NULL` if `Queue` is empty.
    #'
    pop = function() {
      returned_element <- self$get()[1L]
      private$array <- private$array[-1L]
      returned_element
    },
    #' @description
    #' Removes the oldest occurrence of specified element(s) from `Queue`.
    #' Relies on implicit type conversions of R identify elements to remove.
    #'
    #' @param elements vector of elements to remove from `Queue`.
    #'
    #' @return `self`, invisibly.
    #'
    remove = function(elements) {
      for (el in elements) {
        ind <- Position(function(x) identical(x, el), private$array)
        if (!is.na(ind)) private$array <- private$array[-ind]
      }
      invisible(self)
    },
    #' @description
    #' Removes all elements from `Queue`.
    #'
    #' @return `self`, invisibly.
    #'
    empty = function() {
      private$array <- c()
      invisible(self)
    },
    #' @description
    #' Returns the number of elements in `Queue`.
    #'
    #' @return `integer(1)`.
    #'
    size = function() {
      length(self$get())
    },
    #' @description
    #' Prints this `Queue`.
    #'
    #' @param ... Additional arguments to this method, ignored.
    #'
    #' @return `self`, invisibly.
    print = function(...) {
      cat(
        sprintf(
          "%s\nSize: %i\nElements:\n%s\n",
          strsplit(format(self), "\n")[[1]][1],
          self$size(),
          paste(self$get(), collapse = " ")
        )
      )
      invisible(self)
    }
  ),

  # private members ----
  private = list(
    array = c()
  ),
  lock_class = TRUE
)
