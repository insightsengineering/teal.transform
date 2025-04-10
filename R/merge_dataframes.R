
merge_module_ui <- function(id) {
  ns <- NS(id)
  renderText(ns("a"))
}

consolidate_extraction <- function(...) {
  if (...length() > 1) {
    input_resolved <- list(...)
  } else {
    input_resolved <- ..1
  }

  datasets <- lapply(input_resolved, function(x){x$datasets})
  # Assume the data is a data.frame so no other specifications types are present.
  variables <- lapply(input_resolved, function(x){x$variables})
  lapply(unique(datasets),
         function(dataset, x, y) {
           list("datasets" = dataset, "variables" = unique(unlist(y[x == dataset])))
         }, x = datasets, y = variables)
}

add_ids <- function(input, data) {

  jk <- join_keys(data)
  if (!length(jk)) {
    return(input)
  }

  datasets <- lapply(input, function(x){x$datasets})
  for (i in seq_along(input)) {
    x <- input[[i]]
    # Avoid adding as id something already present.
    ids <- setdiff(unique(unlist(jk[[x$datasets]])), x$variables)
    input[[i]][["variables"]] <- c(x$variables, ids)
  }
  input
}


extract_ids <- function(input, data) {
  jk <- join_keys(data)
  # No join_keys => input
  if (!length(jk)) {
    input <- unlist(input)
    tab <- table(input)
    out <- names(tab)[tab > 1]

    if (length(out)) {
      return(NULL)
    }
    return(out)
  }

  l <- lapply(datasets, function(x, join_keys) {
    unique(unlist(jk[[x]]))
  }, join_keys = jk)
  out <- unique(unlist(l))
}

merge_module_srv <- function(id, ..., data, ids, type) {
  # stopifnot(is.reactive(data))
  stopifnot(is.character(id))
  moduleServer(id, function(input, output, session) {
    out <- reactive({
      if (...length() == 1L && is.list(..1)) {
        input_list <- ..1
      } else {
        input_list <- list(...)
      }

      input_list <- consolidate_extraction(input_list)

      # No merge is needed
      if (length(input_list) == 1L) {
        out <- extract_input(input_list, data)
        output$out <- out
        return(out)
      }
      # Add ids to merge by them if known
      input_list <- add_ids(input_list, data)
      input_data <- lapply(input_list, extract_input, data = data)
      # TODO: return an expression
      # Evaluation should be addressed by eval_code(qenv, code = output)
      merging(input_data, ids = extract_ids(input_list, data), type = type)
    })
    output$out <- out
    out
  })
}

extract_input <- function(input, data) {
  for (i in input) {
    # Extract data recursively: only works on lists and square objects (no MAE or similar)
    # To work on new classes implement an extract.class method
    # Assumes order of extraction on the input: qenv > datasets > variables
    # IF datasetes > variables > qenv order
    data <- extract(data, i, drop = FALSE)
  }
  data
}

# Allows merging arbitrary number of data.frames by ids and type

merging <- function(..., ids, type) {
  input_as_list <- is.list(..1) & ...length() == 1L
  if (input_as_list) {
    list_df <- ..1
  } else {
    list_df <- list(...)
  }
  number_merges <- length(list_df) - 1L
  stopifnot(
    "Number of datasets is enough" = number_merges >= 1L,
    "Number of arguments for type matches data" = length(type) == number_merges || length(type) == 1L
  )

  if (!missing(ids)) {
    stopifnot("Number of arguments for ids matches data" = !(is.list(ids) && length(ids) == number_merges))
  }
  if (length(type) != number_merges) {
    type <- rep(type, number_merges)
  }
  if (!missing(ids) && length(ids) != number_merges) {
    ids <- rep(ids, number_merges)
  }

  if (number_merges == 1L && !input_as_list && !missing(ids)) {
    return(self_merging(..1, ..2, ids = ids, type = type))
  } else if (number_merges == 1L && !input_as_list && missing(ids)) {
    return(self_merging(..1, ..2, type = type))
  } else if (number_merges == 1L && input_as_list && missing(ids)) {
    return(self_merging(list_df[[1]], list_df[[2]], type = type))
  } else if (number_merges == 1L && input_as_list && !missing(ids)) {
    return(self_merging(list_df[[1]], list_df[[2]], ids = ids, type = type))
  }

  for (merge_i in seq_len(number_merges)) {
    message(merge_i)
    if (merge_i == 1L) {
      if (missing(ids)) {
        ids <- intersect(colnames(list_df[[merge_i]]), colnames(list_df[[merge_i + 1L]]))
      } else {
        ids <- ids[[merge_i]]
      }
      out <- self_merging(list_df[[merge_i]], list_df[[merge_i + 1L]],
        ids,
        type = type[[merge_i]]
      )
    } else {
      if (missing(ids)) {
        ids <- intersect(colnames(out, colnames(list_df[[merge_i + 1L]])))
      } else {
        ids <- ids[[merge_i]]
      }
      out <- self_merging(out, list_df[[merge_i + 1L]],
        ids,
        type = type[[merge_i]]
      )
    }
  }
  out
}


# self_merge(df1, df2) almost equal to self_merge(df2, df1): Only changes on the column order.
self_merging <- function(e1, e2, ids = intersect(colnames(e1), colnames(e2)), type) {
  # Get the name of the variables to use as suffix.
  # If we need the name at higher environments (ie: f(self_merging()) ) it could use rlang (probably)
  name1 <- deparse(substitute(e1))
  name2 <- deparse(substitute(e2))
  suffix1 <- paste0(".", name1)
  suffix2 <- paste0(".", name2)
  ce1 <- colnames(e1)
  ce2 <- colnames(e2)
  type <- match.arg(type, c("inner", "left", "right", "full"))

  # Called by its side effects of adding the two variables the the current environment
  switch(type,
    inner = {
      all.x <- FALSE
      all.y <- FALSE
    },
    full = {
      all.x <- TRUE
      all.y <- TRUE
    },
    left = {
      all.x <- TRUE
      all.y <- FALSE
    },
    right = {
      all.x <- FALSE
      all.y <- TRUE
    },
    {
      all.x <- FALSE
      all.y <- FALSE
    }
  )

  if (!is.null(names(ids))) {
    name_ids <- names(ids)
  } else {
    name_ids <- ids
  }

  if (!all(ids %in% name_ids) && !all(ids %in% ce2)) {
    stop("Not all ids are in both objects")
  }
  # The default generic should find the right method, if not we :
  # a) ask for the method to be implemented or
  # b) implement it ourselves here to be used internally.
  mm <- merge(e1, e2,
    all.x = all.x, all.y = all.y,
    by.x = name_ids, by.y = ids,
    suffixes = c(".e1", ".e2")
  )
  g <- grep("\\.[(e1)(e2)]", colnames(mm))
  if (length(g)) {
    mix_columns <- setdiff(intersect(ce1, ce2), ids)
    for (column in mix_columns) {
      mc1 <- paste0(column, ".e1")
      mc2 <- paste0(column, ".e2")
      # Rename column and delete one if they are the same
      if (identical(mm[, mc1], mm[, mc2])) {
        mm[, mc2] <- NULL
        colnames(mm)[colnames(mm) %in% mc1] <- column
      } else {
        # Rename to keep the suffic of the data names
        colnames(mm)[colnames(mm) %in% mc1] <- paste0(column, suffix1)
        colnames(mm)[colnames(mm) %in% mc2] <- paste0(column, suffix2)
      }
    }
  }
  mm
}
