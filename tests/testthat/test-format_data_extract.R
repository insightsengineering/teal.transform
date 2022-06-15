required_names <- c("select", "filters", "dataname")

testthat::test_that("format_data_extract is a function that accepts a list", {
  data_extract_fake <- as.list(stats::setNames(nm = required_names))
  data_extract_fake$filters <- list()
  testthat::expect_error(format_data_extract(data_extract_fake), regexp = NA)
})

testthat::test_that("format_data_extract asserts its argument has required names", {
  testthat::expect_error(
    format_data_extract(list()),
    regexp = "data_extract must be a named list with names: select filters dataname"
  )
})

testthat::test_that("format_data_extract returns a string representation of the extracted data", {
  data_extract_fake <- as.list(stats::setNames(nm = required_names))
  data_extract_fake$dataname <- "test dataname"
  data_extract_fake$filters <- list(list(columns = c("ColA", "ColB"), selected = "ColB"))
  data_extract_fake$select <- c("ColC", "ColD")
  data_extract_fake

  testthat::expect_equal(
    format_data_extract(data_extract_fake),
    paste(
      "<Data Extract for dataset: test dataname>",
      "Filters:",
      "  Columns: ColA ColB Selected: ColB",
      "Selected columns:",
      "  ColC ColD",
      sep = "\n"
    )
  )
})

testthat::test_that("format_data_extract integrates with data_extract_srv", {
  sample_filtered_data <- teal.slice::init_filtered_data(list(iris = list(dataset = iris)))


  simple_des <- data_extract_spec(
    dataname = "iris",
    filter = filter_spec(vars = "Petal.Length", choices = c("1.4", "1.5")),
    select = select_spec(choices = c("Petal.Length", "Species"))
  )

  shiny::testServer(
    data_extract_srv,
    args = list(data_extract_spec = simple_des, datasets = sample_filtered_data),
    expr = {
      testthat::expect_error(format_data_extract(session$returned()), regexp = NA)
    }
  )
})

testthat::test_that("format_data_extract integrates with data_extract_srv and the filtered data object", {
  skip_if_too_deep(4)
  shinytest::expect_pass(
    shinytest::testApp(testthat::test_path("shinytest/format_data_extract"), compareImages = FALSE)
  )
})