adsl_df <- as.data.frame(as.list(stats::setNames(nm = teal.data::get_cdisc_keys("ADSL"))))
adlb_df <- as.data.frame(as.list(stats::setNames(nm = teal.data::get_cdisc_keys("ADLB"))))

adsl <- teal.data::cdisc_dataset("ADSL", adsl_df)
adlb <- teal.data::cdisc_dataset("ADLB", adlb_df)

datasets <- teal.slice::init_filtered_data(
  list(ADSL = list(dataset = adsl_df)),
  join_keys = teal.data::join_keys(
    teal.data::join_key("ADSL", "ADSL", teal.data::get_cdisc_keys("ADSL"))
  )
)

data_list <- sapply(X = datasets$datanames(), simplify = FALSE, FUN = function(x) {
  reactive(datasets$get_data(dataname = x, filtered = FALSE))
})

nr_data_list <- sapply(X = datasets$datanames(), simplify = FALSE, FUN = function(x) {
  datasets$get_data(dataname = x, filtered = FALSE)
})

join_keys_list <- datasets$get_join_keys()

adsl_extract <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    label = "Select variable:",
    choices = variable_choices(adsl, teal.data::get_cdisc_keys("ADSL")),
    selected = "STUDYID",
    multiple = TRUE,
    fixed = FALSE
  )
)

adlb_extract <- data_extract_spec(
  dataname = "ADLB",
  select = select_spec(
    label = "Select variable:",
    choices = c("STUDYID"),
    selected = "STUDYID",
    multiple = TRUE,
    fixed = FALSE
  )
)

testthat::test_that(
  desc = "data_extract_srv accepts a FilteredData object or a list of (reactive) data frames to datasets",
  code = {
    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        data_extract_srv(id = "x", data_extract_spec = adsl_extract, datasets = datasets),
        NA
      )
    )

    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        data_extract_srv(id = "x", data_extract_spec = adsl_extract, datasets = data_list, join_keys = join_keys_list),
        NA
      )
    )

    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        data_extract_srv(
          id = "x",
          data_extract_spec = adsl_extract,
          datasets = nr_data_list,
          join_keys = join_keys_list
        ),
        NA
      )
    )

    mixed_data_list <- list(ADSL = reactive(datasets$get_data(dataname = "ADSL", filtered = FALSE)), ADLB = adsl_df)
    mixed_join_keys_list <- teal.data::join_keys(
      teal.data::join_key("ADSL", "ADSL", teal.data::get_cdisc_keys("ADSL")),
      teal.data::join_key("ADLB", "ADLB", teal.data::get_cdisc_keys("ADLB")),
      teal.data::join_key("ADSL", "ADLB", teal.data::get_cdisc_keys("ADSL"))
    )

    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        data_extract_srv(
          id = "x",
          data_extract_spec = adsl_extract,
          datasets = mixed_data_list,
          join_keys = mixed_join_keys_list
        ),
        NA
      )
    )
  }
)

testthat::test_that(
  desc = "data_extract_srv works with join_keys = NULL (default)",
  code = {
    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_silent(
        data_extract_srv(id = "x", data_extract_spec = adsl_extract, datasets = data_list)
      )
    )
  }
)

testthat::test_that(
  desc = "data_extract_srv accepts throws error when join_keys argument is not a JoinKeys object",
  code = {
    shiny::withReactiveDomain(
      domain = shiny::MockShinySession$new(),
      expr = testthat::expect_error(
        data_extract_srv(id = "x", data_extract_spec = adsl_extract, datasets = data_list, join_keys = "key_list"),
        regexp = "class 'character'.",
        fixed = TRUE
      )
    )
  }
)

testthat::test_that("data_extract_srv returns a list of elements", {
  shiny::testServer(
    data_extract_srv,
    args = list(id = "x", data_extract_spec = adsl_extract, datasets = nr_data_list, join_keys = join_keys_list),
    expr = {
      testthat::expect_is(session$returned(), "list")
      testthat::expect_setequal(
        names(session$returned()),
        c("filters", "select", "always_selected", "reshape", "dataname", "internal_id", "keys", "iv")
      )
    }
  )
})

testthat::test_that("data_extract_srv throws error with missing arguments", {
  testthat::expect_error(
    shiny::testServer(
      data_extract_srv,
      args = list(id = "x", datasets = nr_data_list, join_keys = join_keys_list),
      expr = NULL
    ),
    "argument \"data_extract_spec\" is missing, with no default"
  )

  testthat::expect_error(
    shiny::testServer(
      data_extract_srv,
      args = list(id = "x", data_extract_spec = adsl_extract),
      expr = NULL
    ),
    "argument \"datasets\" is missing, with no default"
  )
})

testthat::test_that("data_extract_srv throws error with wrong argument input type", {
  testthat::expect_error(
    shiny::testServer(
      data_extract_srv,
      args = list(id = "x", data_extract_spec = c("data_extract"), datasets = nr_data_list, join_keys = join_keys_list),
      expr = NULL
    ),
    regexp = "has class 'character'"
  )

  testthat::expect_error(
    shiny::testServer(
      data_extract_srv,
      args = list(id = "x", data_extract_spec = TRUE, datasets = nr_data_list, join_keys = join_keys_list),
      expr = NULL
    ),
    regexp = "has class 'logical'"
  )

  testthat::expect_error(
    shiny::testServer(
      data_extract_srv,
      args = list(id = "x", data_extract_spec = adsl_extract, datasets = adsl, join_keys = join_keys_list),
      expr = NULL
    ),
    regexp = "Assertion on 'datasets' failed:"
  )

  testthat::expect_error(
    shiny::testServer(
      data_extract_srv,
      args = list(
        id = "adsl_extract",
        data_extract_spec = adsl_extract,
        datasets = nr_data_list,
        join_keys = join_keys_list,
        select_validation_rule = "string"
      ),
      expr = NULL
    ),
    regexp = "Assertion on 'select_validation_rule' failed:"
  )

  testthat::expect_error(
    shiny::testServer(
      data_extract_srv,
      args = list(
        id = "adsl_extract",
        data_extract_spec = adsl_extract,
        datasets = nr_data_list,
        join_keys = join_keys_list,
        filter_validation_rule = "string"
      ),
      expr = NULL
    ),
    regexp = "Assertion on 'filter_validation_rule' failed:"
  )

  testthat::expect_error(
    shiny::testServer(
      data_extract_srv,
      args = list(
        id = "adsl_extract",
        data_extract_spec = adsl_extract,
        datasets = nr_data_list,
        join_keys = join_keys_list,
        dataset_validation_rule = "string"
      ),
      expr = NULL
    ),
    regexp = "Assertion on 'dataset_validation_rule' failed:"
  )

  testthat::expect_error(
    shiny::testServer(
      data_extract_srv,
      args = list(
        id = "adsl_extract",
        data_extract_spec = adsl_extract,
        datasets = nr_data_list,
        join_keys = join_keys_list,
        select_validation_rule = TRUE
      ),
      expr = NULL
    ),
    regexp = "Assertion on 'select_validation_rule' failed:"
  )

  testthat::expect_error(
    shiny::testServer(
      data_extract_srv,
      args = list(
        id = "adsl_extract",
        data_extract_spec = adsl_extract,
        datasets = nr_data_list,
        join_keys = join_keys_list,
        select_validation_rule = 1
      ),
      expr = NULL
    ),
    regexp = "Assertion on 'select_validation_rule' failed:"
  )
})

testthat::test_that("data_extract_srv uses the current session id when id is missing", {
  shiny::testServer(
    data_extract_srv,
    args = list(
      id = "adsl_extract",
      data_extract_spec = adsl_extract,
      datasets = nr_data_list,
      join_keys = join_keys_list
    ),
    expr = {
      testthat::expect_is(session$returned(), "list")
      testthat::expect_setequal(
        names(session$returned()),
        c("filters", "select", "always_selected", "reshape", "dataname", "internal_id", "keys", "iv")
      )
    }
  )
})


testthat::test_that("data_extract_srv returns select ordered according to selection", {
  extract_ordered <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(adsl, teal.data::get_cdisc_keys("ADSL")),
      selected = "STUDYID",
      ordered = TRUE
    )
  )

  shiny::testServer(
    data_extract_srv,
    args = list(id = "x", data_extract_spec = extract_ordered, datasets = datasets),
    expr = {
      session$setInputs(`dataset_ADSL_singleextract-select` = c("b", "c"))
      testthat::expect_identical(filter_and_select_reactive()$select, c("b", "c"))

      session$setInputs(`dataset_ADSL_singleextract-select` = "c")
      testthat::expect_identical(filter_and_select_reactive()$select, "c")

      session$setInputs(`dataset_ADSL_singleextract-select` = c("b", "c"))
      testthat::expect_identical(filter_and_select_reactive()$select, c("c", "b"))
    }
  )
})

testthat::test_that("data_extract_srv returns select ordered according to choices", {
  extract_unordered <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(adsl, teal.data::get_cdisc_keys("ADSL")),
      selected = "STUDYID",
      ordered = FALSE
    )
  )

  shiny::testServer(
    data_extract_srv,
    args = list(id = "x", data_extract_spec = extract_unordered, datasets = datasets),
    expr = {
      session$setInputs(`dataset_ADSL_singleextract-select` = c("b", "c"))
      testthat::expect_identical(filter_and_select_reactive()$select, c("b", "c"))

      session$setInputs(`dataset_ADSL_singleextract-select` = "c")
      testthat::expect_identical(filter_and_select_reactive()$select, "c")

      session$setInputs(`dataset_ADSL_singleextract-select` = c("b", "c"))
      testthat::expect_identical(filter_and_select_reactive()$select, c("b", "c"))
    }
  )
})

datasets <- teal.slice::init_filtered_data(
  list(
    ADSL = list(dataset = adsl_df),
    ADLB = list(dataset = adsl_df)
  ),
  join_keys = teal.data::join_keys(
    teal.data::join_key("ADSL", "ADSL", teal.data::get_cdisc_keys("ADSL")),
    teal.data::join_key("ADLB", "ADLB", teal.data::get_cdisc_keys("ADLB"))
  )
)
testthat::test_that("data_extract_srv with a list of multiple data_extract_spec", {
  extract_list <- list(adsl_extract = adsl_extract, adlb_extract = adlb_extract)

  shiny::testServer(
    data_extract_srv,
    args = list(id = "x", data_extract_spec = extract_list, datasets = datasets),
    expr = {
      session$setInputs(`dataset` = "ADLB")
      testthat::expect_identical(input$dataset, "ADLB")
      testthat::expect_identical(filter_and_select_reactive()$dataname, "ADLB")

      session$setInputs(`dataset` = "ADSL")
      testthat::expect_identical(input$dataset, "ADSL")
      testthat::expect_identical(filter_and_select_reactive()$dataname, "ADSL")
    }
  )
})

ADSL_val <- data.frame( # nolint
  STUDYID = "A",
  USUBJID = LETTERS[1:10],
  SEX = rep(c("F", "M"), 5),
  AGE = rpois(10, 30),
  BMRKR1 = rlnorm(10)
)

adsl_extract_val <- data_extract_spec(
  dataname = "ADSL",
  filter = filter_spec(vars = "SEX", choices = c("F", "M"), selected = "F"),
  select = select_spec(
    label = "Select variable:",
    choices = variable_choices(ADSL_val, c("AGE", "BMRKR1")),
    selected = "AGE",
    multiple = TRUE,
    fixed = FALSE
  )
)

data_list_val <- list(ADSL = reactive(ADSL_val))
join_keys_val <- teal.data::join_keys(teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")))

testthat::test_that("select validation", {
  server <- function(input, output, session) {
    adsl_reactive_input <- data_extract_srv(
      id = "adsl_var",
      datasets = data_list_val,
      data_extract_spec = adsl_extract_val,
      join_keys = join_keys_val,
      select_validation_rule = shinyvalidate::sv_required("Please select a variable.")
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_validator(adsl_reactive_input()$iv)
      iv$enable()
      iv
    })

    output$out1 <- renderPrint({
      if (iv_r()$is_valid()) {
        cat(format_data_extract(adsl_reactive_input()))
      } else {
        "Please fix errors in your selection"
      }
    })
  }

  shiny::testServer(server, {
    session$setInputs("adsl_var-dataset_ADSL_singleextract-select" = "STUDYID")
    testthat::expect_true(iv_r()$is_valid())
    testthat::expect_equal(output$out1, format_data_extract(adsl_reactive_input()))

    session$setInputs("adsl_var-dataset_ADSL_singleextract-select" = "")
    testthat::expect_match(output$out1, "Please fix errors in your selection")
  })
})

testthat::test_that("validation only runs on currently selected dataset's data extract spec", {
  iris_extract_val <- data_extract_spec(
    dataname = "IRIS",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(iris, colnames(iris)),
      selected = "Sepal.Length",
      multiple = TRUE,
      fixed = FALSE
    )
  )


  server <- function(input, output, session) {
    adsl_reactive_input <- data_extract_srv(
      id = "adsl_var",
      datasets = data_list_val,
      data_extract_spec = list(adsl_extract_val, iris_extract_val),
      join_keys = join_keys_val,
      select_validation_rule = shinyvalidate::sv_required("Please select a variable.")
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_validator(adsl_reactive_input()$iv)
      iv$enable()
      iv
    })

    output$out1 <- renderPrint({
      if (iv_r()$is_valid()) {
        cat(format_data_extract(adsl_reactive_input()))
      } else {
        "Please fix errors in your selection"
      }
    })
  }

  shiny::testServer(server, {
    session$setInputs("adsl_var-dataset_ADSL_singleextract-select" = "")
    testthat::expect_match(output$out1, "Please fix errors in your selection")
    session$setInputs("adsl_var-dataset" = "IRIS")
    session$setInputs("adsl_var-dataset_IRIS_singleextract-select" = "Species")
    testthat::expect_true(iv_r()$is_valid())
  })
})

testthat::test_that("filter validation", {
  server <- function(input, output, session) {
    adsl_reactive_input <- data_extract_srv(
      id = "adsl_var",
      datasets = data_list_val,
      data_extract_spec = adsl_extract_val,
      join_keys = join_keys_val,
      filter_validation_rule = shinyvalidate::sv_required("Please select a variable.")
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_validator(adsl_reactive_input()$iv)
      iv$enable()
      iv
    })

    output$out1 <- renderPrint({
      if (iv_r()$is_valid()) {
        cat(format_data_extract(adsl_reactive_input()))
      } else {
        "Please fix errors in your selection"
      }
    })
  }

  shiny::testServer(server, {
    session$setInputs("adsl_var-dataset_ADSL_singleextract-filter1-vals" = "F")
    testthat::expect_true(iv_r()$is_valid())
    testthat::expect_equal(output$out1, format_data_extract(adsl_reactive_input()))

    session$setInputs("adsl_var-dataset_ADSL_singleextract-filter1-vals" = "")
    testthat::expect_match(output$out1, "Please fix errors in your selection")
  })
})


testthat::test_that("select validation accepts function as validator", {
  server <- function(input, output, session) {
    adsl_reactive_input <- data_extract_srv(
      id = "adsl_var",
      datasets = data_list_val,
      data_extract_spec = adsl_extract_val,
      join_keys = join_keys_val,
      select_validation_rule = ~ if (nchar(.) == 0) "error"
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_validator(adsl_reactive_input()$iv)
      iv$enable()
      iv
    })

    output$out1 <- renderPrint({
      if (iv_r()$is_valid()) {
        cat(format_data_extract(adsl_reactive_input()))
      } else {
        "Please fix errors in your selection"
      }
    })
  }

  shiny::testServer(server, {
    session$setInputs("adsl_var-dataset_ADSL_singleextract-select" = "STUDYID")
    testthat::expect_true(iv_r()$is_valid())
    testthat::expect_equal(output$out1, format_data_extract(adsl_reactive_input()))

    session$setInputs("adsl_var-dataset_ADSL_singleextract-select" = "")
    testthat::expect_match(output$out1, "Please fix errors in your selection")
  })
})

testthat::test_that("data_extract_multiple_srv input validation", {
  iris_select <- data_extract_spec(
    dataname = "iris",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(iris, colnames(iris)),
      selected = "Sepal.Length",
      multiple = TRUE,
      fixed = FALSE
    )
  )

  iris_filter <- data_extract_spec(
    dataname = "iris",
    filter = filter_spec(
      vars = "Species",
      choices = c("setosa", "versicolor", "virginica"),
      selected = "setosa",
      multiple = TRUE
    )
  )

  data_list <- list(iris = reactive(iris))

  server <- function(input, output, session) {
    exactly_2_validation <- function(msg) {
      ~ if (length(.) != 2) msg
    }


    selector_list <- data_extract_multiple_srv(
      list(x_var = iris_select, species_var = iris_filter),
      datasets = data_list,
      select_validation_rule = list(
        x_var = shinyvalidate::sv_required("Please select an X column")
      ),
      filter_validation_rule = list(
        species_var = shinyvalidate::compose_rules(
          shinyvalidate::sv_required("Exactly 2 Species must be chosen"),
          exactly_2_validation("Exactly 2 Species must be chosen")
        )
      )
    )
    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      compose_and_enable_validators(
        iv,
        selector_list,
        validator_names = NULL
      )
    })

    output$out1 <- renderPrint({
      if (iv_r()$is_valid()) {
        ans <- lapply(selector_list(), function(x) {
          cat(format_data_extract(x()), "\n\n")
        })
      } else {
        "Please fix errors in your selection"
      }
    })
  }

  shiny::testServer(server, {
    testthat::expect_false(iv_r()$is_valid())
    session$setInputs(
      "x_var-dataset_iris_singleextract-select" = "Sepal.Length"
    )
    session$setInputs(
      "species_var-dataset_iris_singleextract-filter1-vals" = c("setosa", "versicolor")
    )
    testthat::expect_true(iv_r()$is_valid())

    out1 <- paste(output$out1, collapse = "")
    msg <- paste(
      lapply(
        selector_list(),
        function(x) format_data_extract(x())
      ),
      collapse = ""
    )
    # slight difference in spacing and new line b/c of cat()
    testthat::expect_equal(
      gsub("(\\s)|(\n)", "", out1),
      gsub("(\\s)|(\n)", "", msg)
    )

    session$setInputs(
      "x_var-dataset_iris_singleextract-select" = ""
    )
    testthat::expect_false(iv_r()$is_valid())
    testthat::expect_match(output$out1, "Please fix errors in your selection")

    session$setInputs(
      "x_var-dataset_iris_singleextract-select" = "Sepal.Length"
    )
    session$setInputs(
      "species_var-dataset_iris_singleextract-filter1-vals" = ""
    )
    testthat::expect_false(iv_r()$is_valid())

    session$setInputs(
      "species_var-dataset_iris_singleextract-filter1-vals" = c("setosa", "versicolor")
    )
    testthat::expect_true(iv_r()$is_valid())
  })
})
