to_picks <- function(x, dataname) {
  if (checkmate::test_list(x, "data_extract_spec")) {

  }
}

des_to_picks <- function(x) {
  if (inherits(x, "picks")) {
    x
  } else if (length(x)) {
    args <- Filter(
      length,
      list(
        datasets(choices = x$dataname, fixed = TRUE),
        select_spec_to_variables(x$select)
        # don't use filter_spec as they are not necessary linked with `select` (selected variables)
        #  as filter_spec can be speciefied on the variable(s) different than select_spec for example:
        #  for example: #pseudocode select = select_spec(AVAL); filter = filter_spec(PARAMCD))
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

extract_filters <- function(elem, dataname) {
  if (inherits(elem, "filter_spec")) {
    picks(
      datasets(choices = dataname, selected = dataname),
      variables(choices = elem$vars_choices, selected = elem$vars_selected, multiple = FALSE), # can't be multiple
      values(choices = elem$choices, selected = elem$selected, multiple = elem$multiple)
    )
  } else if (checkmate::test_list(elem, "filter_spec")) {
    lapply(elem, extract_filters, dataname = dataname)
  } else if (inherits(elem, "data_extract_spec")) {
    extract_filters(elem$filter, dataname = elem$dataname)
  } else if (checkmate::test_list(elem, c("data_extract_spec", "list", "NULL"))) {
    unlist(
      lapply(Filter(length, elem), extract_filters),
      recursive = FALSE
    )
  }
}
