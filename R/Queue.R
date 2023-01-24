# Queue ====

#' @title R6 Class - A First-In-First-Out Abstract Data Type
#'
#' @description `r lifecycle::badge("experimental")`\cr
#' Abstract data type that stores and returns any number of elements.
#'
#' A `Queue` object stores all elements in a single vector,
#' thus all data types can be stored, but silent coercion may occur.
#'
#' Elements are returned in the same order that they were added.
#'
Queue <- R6::R6Class( # nolint
  classname = "Queue",
  # public methods ----
  public = list(
    #' @description
    #' Adds element(s) to `Queue`.
    #'
    #' @param new_elements vector of elements to add
    #'
    #' @return self invisibly
    #'
    push = function(new_elements) {
      if (R6::is.R6(new_elements) || isS4(new_elements)) {
        private$set(new_elements, append = TRUE)
      } else {
        for (i in seq_along(new_elements)) {
          if (length(new_elements[i]) == 1 || R6::is.R6(new_elements[i])) {
            # new_elements[i] does not discard names if it's a names list
            private$set(new_elements[i], append = TRUE)
          }
        }
      }

      invisible(self)
    },
    #' @description
    #' Returns all contents of the `Queue` object.
    #'
    #' @param reversed (`logical`)\cr
    #' if TRUE then returns the First-In-First-Out order;
    #' otherwise returns the Last-In-First-Out order.
    #'
    #' @return single vector containing all `Queue` contents
    #'
    get = function(reversed = FALSE) {
      if (reversed) {
        rev(private$array)
      } else {
        private$array
      }
    },
    #' @description
    #' Returns the first (oldest) element of the `Queue` and removes it.
    #'
    #' @return
    #' vector of length 1 containing the first element of `Queue` or NULL if `Queue` is empty
    #'
    pop = function() {
      returned_element <- self$get()[1]
      private$set(self$get()[-1])
      returned_element
    },
    #' @description
    #' Removes the oldest occurrence of specified element(s) from `Queue`.
    #' Relies on implicit type conversions of R identify elements to remove.
    #'
    #' @param elements vector of elements to remove from `Queue`
    #'
    #' @return self invisibly
    #'
    remove = function(elements) {
      lapply(elements, function(element) {
        index_to_remove <- which(vapply(self$get(), identical, logical(1), element))[1]
        if (!is.na(index_to_remove)) private$set(self$get()[-index_to_remove])
      })
      invisible(self)
    },
    #' @description
    #' Removes all elements from `Queue`.
    #'
    #' @return self invisibly
    #'
    empty = function() {
      private$set(c())
      invisible(self)
    },
    #' @description
    #' Returns the number of elements in `Queue`.
    #'
    #' @return integer of length 1
    #'
    size = function() {
      length(self$get())
    },
    #' @description
    #' Prints this `Queue`.
    #'
    #' @param ... additional arguments to this method, ignored
    #'
    #' @return invisibly self
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
    array = c(),
    set = function(x, append = FALSE) {
      if (isTRUE(append)) {
        private$array <- c(private$array, x)
      } else {
        private$array <- x
      }
    }
  ),
  lock_class = TRUE
)
