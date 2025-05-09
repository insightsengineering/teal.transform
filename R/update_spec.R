#' Update a specification
#'
#' Update the specification for different selection.
#' @param spec A resolved specification such as one created with datasets and variables.
#' @param type Which type was updated? One of datasets, variables, values.
#' @param value What is the new selection? One that is a valid value for the given type and specification.
#' @return The specification with restored choices and selection if caused by the update.
#' @export
#' @examples
#' td <- within(teal.data::teal_data(), {
#'   df <- data.frame(
#'     A = as.factor(letters[1:5]),
#'     Ab = LETTERS[1:5]
#'   )
#'   df_n <- data.frame(
#'     C = 1:5,
#'     Ab = as.factor(letters[1:5])
#'   )
#' })
#' data_frames_factors <- c(datasets(where(is.data.frame)), variables(where(is.factor)))
#' res <- resolver(data_frames_factors, td)
#' update_spec(res, "datasets", "df_n")
#' # update_spec(res, "datasets", "error")
update_spec <- function(spec, type, value) {
  if (!is.character(value)) {
    stop(
      "The updated value is not a character.",
      "\nDo you attempt to set a new specification? Please open an issue."
    )
  }

  if (valid_specification(spec)) {
    stop("Unexpected object used as specification")
  }

  if (is.null(names(spec))) {
    updated_spec <- lapply(spec, update_s_spec, type = type, value = value)
    class(updated_spec) <- class(spec)
    return(updated_spec)
  }
  if (!is.null(names(spec))) {
    updated_spec <- update_s_spec(spec, type, value)
  } else if (is.type(spec)) {
    updated_spec <- update_s_spec(spec, is(spec), value)
  }
  updated_spec
}

#' @importFrom methods is
update_s_spec <- function(spec, type, value) {
  if (is.type(spec)) {
    l <- list(spec)
    names(l) <- is(spec)
    out <- update_s_spec(l, type, value)
    return(out[[is(spec)]])
  }

  if (is.delayed(spec)) {
    stop("Specification is not resolved (`!is.delayed(spec)`) can't update selections.")
  }

  spec_types <- names(spec)
  type <- match.arg(type, spec_types)
  restart_types <- spec_types[seq_along(spec_types) > which(type == spec_types)]

  valid_names <- spec[[type]]$names

  if (!is.list(valid_names) && all(value %in% valid_names)) {
    original_select <- orig(spec[[type]]$select)
    spec[[type]][["select"]] <- value
    attr(spec[[type]][["select"]], "original") <- original_select
  } else if (!is.list(valid_names) && !all(value %in% valid_names)) {
    original_select <- orig(spec[[type]]$select)

    valid_values <- intersect(value, valid_names)
    if (!length(valid_values)) {
      stop("No valid value provided.")
    }
    if (!length(valid_values)) {
      spec[[type]][["select"]] <- original_select
    } else {
      spec[[type]][["select"]] <- valid_values
    }
    attr(spec[[type]][["select"]], "original") <- original_select
  } else {
    stop("It seems the specification needs to be resolved first.")
  }

  # Restore to the original specs
  for (type_restart in restart_types) {
    if (is.na(spec[[type_restart]])) {
      next
    }
    restored_specification <- type_helper(
      type = type_restart,
      names = orig(spec[[type_restart]]$names),
      select = orig(spec[[type_restart]]$select)
    )
    spec[[type_restart]] <- restored_specification
  }
  spec
}
