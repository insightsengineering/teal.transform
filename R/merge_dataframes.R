# Simplify multiple datasets & variables into the bare minimum necessary.
# This simplifies the number of extractions and merging required
consolidate_extraction <- function(...) {
  if (...length() > 1) {
    input_resolved <- list(...)
  } else {
    input_resolved <- ..1
  }

  # Assume the data is a data.frame so no other specifications types are present.
  datasets <- lapply(input_resolved, `$`, name = datasets)
  variables <- lapply(input_resolved, `$`, name = variables)
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
  jk <- join_keys(data)
  # If no join keys they should be on the input
  if (!length(jk)) {
    return(input)
  }

  datasets <- lapply(input, `$`, name = datasets)
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
  stopifnot(length(selections) == 2L)
  datasets <- sapply(selections, function(x) {
    x$datasets
  })
  by <- extract_ids(input = selections, data)

  if (grepl("::", merge_function, fixed = TRUE)) {
    m <- strsplit(merge_function, split = "::", fixed = TRUE)[[1]]
    data <- eval_code(data, call("library", m[1]))
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
  datasets <- sapply(input, function(x) {
    x$datasets
  })
  stopifnot(is.character(datasets) && length(datasets) >= 1L)
  number_merges <- length(datasets) - 1L
  stopifnot(
    "Number of datasets is enough" = number_merges >= 1L,
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
    return(eval_code(data, final_call))
  } else if (number_merges == 1L && !missing(ids)) {
    previous <- merge_call_pair(input, by = ids, merge_function = merge_function, data = data)
    final_call <- call("<-", x = as.name(anl_name), value = previous)
    return(eval_code(data, final_call))
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
          type = merge_function[merge_i], data = data
        )
      } else {
        ids <- ids[[merge_i]]
        current <- merge_call_pair(input[datasets_ids],
          ids,
          type = merge_function[merge_i], data = data
        )
      }
    }
    previous <- call("%>%", as.name(previous), as.name(current))
  }
  final_call <- call("<-", x = as.name(anl_name), value = previous)
  eval_code(data, final_call)
}
