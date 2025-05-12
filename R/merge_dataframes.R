# Simplify multiple datasets & variables into the bare minimum necessary.
# This simplifies the number of extractions and merging required
consolidate_extraction <- function(...) {
  if (...length() > 1) {
    input_resolved <- list(...)
  } else {
    input_resolved <- ..1
  }

  # Assume the data is a data.frame so no other specifications types are present.
  datasets <- lapply(input_resolved, function(x) {
    x$datasets
  })
  variables <- lapply(input_resolved, function(x) {
    x$variables
  })
  lapply(unique(datasets),
    function(dataset, x, y) {
      list(
        "datasets" = dataset,
        "variables" = unique(unlist(y[x == dataset]))
      )
    },
    x = datasets, y = variables
  )
}

# Function to add ids of data.frames to the output of modules to enable merging them.
add_ids <- function(input, data) {
  jk <- teal.data::join_keys(data)
  # If no join keys they should be on the input
  if (!length(jk)) {
    return(input)
  }

  datasets <- lapply(input, function(x) {
    x$datasets
  })
  for (i in seq_along(input)) {
    x <- input[[i]]
    # Avoid adding as id something already present: No duplicating input.
    ids <- setdiff(unique(unlist(jk[[x$datasets]])), x$variables)
    input[[i]][["variables"]] <- c(x$variables, ids)
  }
  input
}

# Find common ids to enable merging.
extract_ids <- function(input, data) {
  jk <- teal.data::join_keys(data)
  # No join_keys => input
  if (!length(jk)) {
    input <- unlist(input)
    tab <- table(input)
    out <- names(tab)[tab > 1]

    if (!length(out)) {
      return(NULL)
    }
    return(out)
  }

  l <- lapply(datasets, function(x, join_keys) {
    unique(unlist(jk[[x]]))
  }, join_keys = jk)
  out <- unique(unlist(l))
}

merge_call_pair <- function(selections, by, data,
                            merge_function = "dplyr::full_join",
                            anl_name = "ANL") {
  selections <- consolidate_extraction(selections)
  stopifnot(length(selections) == 2L)
  datasets <- unique(unlist(lapply(selections, `[[`, "datasets"), FALSE, FALSE))
  stopifnot(length(datasets) >= 2)
  by <- extract_ids(input = selections, data)

  if (grepl("::", merge_function, fixed = TRUE)) {
    m <- strsplit(merge_function, split = "::", fixed = TRUE)[[1]]
    data <- teal.code::eval_code(data, call("library", m[1]))
    merge_function <- m[2]
  }

  if (!missing(by) && length(by)) {
    call_m <- call(merge_function,
      x = as.name(datasets[1]),
      y = as.name(datasets[2]),
      by = by
    )
  } else {
    call_m <- call(merge_function,
      x = as.name(datasets[1]),
      y = as.name(datasets[2])
    )
  }
  call_m
}

merge_call_multiple <- function(input, ids, merge_function, data,
                                anl_name = "ANL") {
  input <- consolidate_extraction(input)
  datasets <- unique(unlist(lapply(input, `[[`, "datasets"), FALSE, FALSE))
  stopifnot(is.character(datasets) && length(datasets) >= 1L)
  number_merges <- length(datasets) - 1L

  out <- vector("list", length = 2)
  names(out) <- c("code", "specification")

  if (number_merges == 0L) {
    dataset <- names(input)
    variables <- input[[1]]$variables
    final_call <- call(
      "<-", as.name(anl_name),
      call("dplyr::select", as.name(dataset), as.names(variables))
    )
    out$code <- teal.code::eval_code(data, final_call)
    out$input <- input
    return(out)
  }
  stopifnot(
    "Number of arguments for type matches data" = length(merge_function) == number_merges || length(merge_function) == 1L
  )
  if (!missing(ids)) {
    stopifnot("Number of arguments for ids matches data" = !(is.list(ids) && length(ids) == number_merges))
  }
  if (length(merge_function) != number_merges) {
    merge_function <- rep(merge_function, number_merges)
  }
  if (!missing(ids) && length(ids) != number_merges) {
    ids <- rep(ids, number_merges)
  }

  if (number_merges == 1L && missing(ids)) {
    previous <- merge_call_pair(input, merge_function = merge_function, data = data)
    final_call <- call("<-", x = as.name(anl_name), value = previous)
    out$code <- teal.code::eval_code(data, final_call)
    out$input <- input
    return(out)
  } else if (number_merges == 1L && !missing(ids)) {
    previous <- merge_call_pair(input, by = ids, merge_function = merge_function, data = data)
    final_call <- call("<-", x = as.name(anl_name), value = previous)
    out$code <- teal.code::eval_code(data, final_call)
    out$input <- input
    return(out)
  }



  for (merge_i in seq_len(number_merges)) {
    if (merge_i == 1L) {
      datasets_i <- seq_len(2)
      if (!missing(ids)) {
        ids <- ids[[merge_i]]
        previous <- merge_call_pair(input[datasets_i],
          ids,
          merge_function[merge_i],
          data = data
        )
      } else {
        previous <- merge_call_pair(input[datasets_i],
          merge_function[merge_i],
          data = data
        )
      }
    } else {
      datasets_ids <- merge_i:(merge_i + 1L)
      if (!missing(ids)) {
        current <- merge_call_pair(input[datasets_ids],
          merge_function = merge_function[merge_i], data = data
        )
      } else {
        ids <- ids[[merge_i]]
        current <- merge_call_pair(input[datasets_ids],
          ids,
          merge_function = merge_function[merge_i], data = data
        )
      }
    }
    previous <- call("%>%", as.name(previous), as.name(current))
  }
  final_call <- call("<-", x = as.name(anl_name), value = previous)
  out$code <- teal.code::eval_code(data, final_call)
  out$input <- input
  out
}

merge_selector_srv <- function(id, available, data) {
  moduleServer(
    id,
    function(input, output, session) {
      req(input)
      resolved_spec <- reactive({
        resolved_spec <- lapply(names(available), function(x) {
          module_input_server(x, available[[x]], data)()
        })
        names(resolved_spec) <- names(available)
        resolved_spec
      })
      resolved_spec()
    }
  )
}
