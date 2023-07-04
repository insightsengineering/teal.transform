ADLB <- rADLB # nolint
ADTTE <- rADTTE # nolint

testthat::test_that("Single filter", {
  data_extract <- data_extract_spec(
    dataname = "ADTTE",
    select = select_spec(
      choices = c("AVAL", "AVALU", "BMRKR1", "SITEID"),
      selected = c("AVAL"),
      multiple = FALSE,
      fixed = FALSE, # Whether the user can select the item (optional)
      label = "Column" # Label the column select dropdown (optional)
    ),
    filter = filter_spec(
      vars = "PARAMCD",
      choices = levels(ADTTE$PARAMCD),
      selected = "OS",
      multiple = TRUE,
      label = "Choose endpoint"
    )
  )


  testthat::expect_silent(input <- data_extract_single_ui(id = NULL, data_extract))
  testthat::expect_silent(filter <- input$children[[1]])
  testthat::expect_equal(filter$children[[1]]$children[[1]][[2]]$attribs, list(class = "shinyjs-hide"))


  testthat::expect_equal(filter$children[[1]]$children[[2]][[2]]$children[[2]]$attribs$multiple, "multiple")

  # more tests - check levels of filtered variables
  # check also colummns selected
})


testthat::test_that("Multiple filters", {
  data_extract <- data_extract_spec(
    dataname = "ADLB",
    select = select_spec(
      choices = c("AVAL", "AVALU", "BMRKR1", "SITEID"),
      selected = c("AVAL"),
      multiple = FALSE,
      fixed = FALSE, # Whether the user can select the item (optional)
      label = "Column" # Label the column select dropdown (optional)
    ),
    filter = list(
      filter_spec(
        vars = "AVISIT",
        choices = levels(ADLB$AVISIT),
        selected = "BASELINE",
        multiple = FALSE,
        label = "Choose endpoint"
      ),
      filter_spec(
        vars = "PARAMCD",
        choices = levels(ADLB$PARAMCD),
        selected = "ALT",
        multiple = FALSE,
        label = "Choose endpoint"
      )
    )
  )


  testthat::expect_silent(input <- data_extract_single_ui(id = NULL, data_extract))
  testthat::expect_silent(filters <- input$children[[1]])
  testthat::expect_silent(columns <- input$children[[2]])
  testthat::expect_silent(reshape <- input$children[[3]])

  # more tests
  # number of filters
  testthat::expect_identical(length(lapply(filters$children, function(x) x$children)), 2L)
  # filter levels
  # if filter multiple
  # number of column inputs
})


testthat::test_that("Multiple datasets", {
  data_extract_ADTTE <- data_extract_spec(
    dataname = "ADTTE",
    select = select_spec(
      choices = c("AVAL", "AVALU", "BMRKR1", "SITEID"),
      selected = c("AVAL"),
      multiple = FALSE,
      fixed = FALSE, # Whether the user can select the item (optional)
      label = "Column" # Label the column select dropdown (optional)
    ),
    filter = filter_spec(
      vars = "PARAMCD",
      choices = levels(ADTTE$PARAMCD),
      selected = "OS",
      multiple = TRUE,
      label = "Choose endpoint"
    )
  )

  data_extract_ADLB <- data_extract_spec(
    dataname = "ADLB",
    select = select_spec(
      choices = c("AVAL", "AVALU", "BMRKR1", "SITEID"),
      selected = c("AVAL"),
      multiple = FALSE,
      fixed = FALSE, # Whether the user can select the item (optional)
      label = "Column" # Label the column select dropdown (optional)
    ),
    filter = list(
      filter_spec(
        vars = "AVISIT",
        choices = levels(ADLB$AVISIT),
        selected = "BASELINE",
        multiple = FALSE,
        label = "Choose endpoint"
      ),
      filter_spec(
        vars = "PARAMCD",
        choices = levels(ADLB$PARAMCD),
        selected = "ALT",
        multiple = FALSE,
        label = "Choose endpoint"
      )
    )
  )

  testthat::expect_error(
    input1 <- data_extract_ui(
      id = NULL,
      label = "Variable X",
      data_extract_spec = list(data_extract_ADTTE, data_extract_ADTTE),
      "list contains data_extract_spec objects with the same dataset"
    )
  )


  testthat::expect_silent(
    input1 <- data_extract_ui(
      id = NULL,
      label = "Variable X",
      data_extract_spec = list(data_extract_ADTTE, data_extract_ADLB)
    )
  )
})

testthat::test_that("get_initial_filters_values returns empty strings if vars_selected is NULL", {
  filtered_data <- teal.slice::init_filtered_data(list(iris = list(dataset = utils::head(iris))))
  filter <- filter_spec(vars = "test")
  filter$vars_selected <- NULL
  testthat::expect_equal(
    get_initial_filter_values(filter = filter, datasets = filtered_data),
    list(choices = character(0), selected = character(0))
  )
})

testthat::test_that("get_initial_filters_values returns all column values and the selected option
  if choices is NULL", {
  data_list <- list(iris = reactive(utils::head(iris)))

  filter <- filter_spec(vars = colnames(iris)[1])
  filter$choices <- NULL
  filter$dataname <- "iris"
  filter$selected <- "test"
  testthat::expect_equal(
    isolate(get_initial_filter_values(filter = filter, datasets = data_list)),
    list(choices = value_choices(utils::head(iris), colnames(iris)[1]), selected = "test")
  )
})

testthat::test_that("get_initial_filters_values returns the selected and choices if they are not null", {
  filtered_data <- teal.slice::init_filtered_data(list(iris = list(dataset = utils::head(iris))))
  filter <- filter_spec(vars = colnames(iris)[length(colnames(iris))])
  filter$choices <- "setosa"
  filter$selected <- "setosa"
  testthat::expect_equal(
    isolate(get_initial_filter_values(filter = filter, datasets = filtered_data)),
    list(choices = "setosa", selected = "setosa")
  )
})
