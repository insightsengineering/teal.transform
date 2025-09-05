des_to_picks <- function(x) {
  if (inherits(x, "picks")) {
    x
  } else if (length(x)) {
    args <- Filter(
      length,
      list(
        datasets(choices = x$dataname, fixed = TRUE),
        select_spec_to_variables(x$select)
        # don't use filter_spec as they doesn't have to be linked with selected variables
        #  as filter_spec can be speciefied on the variable(s) different than select_spec for example:
        #  (pseudocode) select_spec(AVAL); filter_spec(PARAMCD, AVISIT)
      )
    )
    do.call(picks, args)
  }
}

select_spec_to_variables <- function(x) {
  if (length(x)) {
    variables(
      choices = x$choices,
      selected = x$selected,
      # keep_order = x$ordered,
      multiple = x$multiple,
      fixed = x$fixed
    )
  }
}


extract_filters <- function(selectors) {
  unlist(
    lapply(selectors, function(des) {
      if (checkmate::test_list(des, "data_extract_spec")) {
        unlist(extract_filters(des), recursive = FALSE)
      } else if (inherits(des, "data_extract_spec")) {
        filter <- if (inherits(des$filter, "filter_spec")) {
          list(des$filter)
        } else {
          des$filter
        }
        lapply(filter, function(x) {
          picks(
            datasets(choices = des$dataname, selected = des$dataname),
            variables(choices = x$vars_choices, selected = x$vars_selected, multiple = FALSE),
            values(choices = x$choices, selected = x$selected, multiple = x$multiple)
          )
        })
      }
    }),
    recursive = FALSE
  )
}
