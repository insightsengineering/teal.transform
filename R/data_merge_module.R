#' Data merge module
#'
#' @description `r lifecycle::badge("experimental")`
#' @details This function is a convenient wrapper to combine `data_extract_multiple_srv()` and
#' `data_merge_srv()` when no additional processing is required.
#' Compare the example below with that found in [data_merge_srv()].
#'
#' @inheritParams shiny::moduleServer
#' @param datasets (`FilteredData`)\cr
#'  object containing data, see [teal.slice::FilteredData] for more.
#' @param data_extract (named `list` of `data_extract_spec`)\cr
#' @param merge_function (`character(1)`)\cr
#'  A character string of a function that
#'  accepts the arguments `x`, `y` and `by` to perform the merging of datasets.
#' @param anl_name (`character(1)`)\cr
#'  Name of the analysis dataset.
#'
#' @return reactive expression with output from [data_merge_srv()].
#'
#' @seealso [data_merge_srv()]
#'
#' @export
#'
#' @examples
#' library(shiny)
#'
#' ADSL <- data.frame(
#'   STUDYID = "A",
#'   USUBJID = LETTERS[1:10],
#'   SEX = rep(c("F", "M"), 5),
#'   AGE = rpois(10, 30),
#'   BMRKR1 = rlnorm(10)
#' )
#' ADLB <- expand.grid(
#'   STUDYID = "A",
#'   USUBJID = LETTERS[1:10],
#'   PARAMCD = c("ALT", "CRP", "IGA"),
#'   AVISIT = c("SCREENING", "BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15")
#' )
#' ADLB$AVAL <- rlnorm(120)
#' ADLB$CHG <- rnorm(120)
#'
#' data <- teal.data::cdisc_data(
#'   teal.data::cdisc_dataset("ADSL", ADSL),
#'   teal.data::cdisc_dataset("ADLB", ADLB)
#' )
#' datasets <- teal.slice::init_filtered_data(data)
#'
#' adsl_extract <- data_extract_spec(
#'   dataname = "ADSL",
#'   select = select_spec(
#'     label = "Select variable:",
#'     choices = c("AGE", "BMRKR1"),
#'     selected = "AGE",
#'     multiple = TRUE,
#'     fixed = FALSE
#'   )
#' )
#' adlb_extract <- data_extract_spec(
#'   dataname = "ADLB",
#'   filter = filter_spec(vars = "PARAMCD", choices = c("ALT", "CRP", "IGA"), selected = "ALT"),
#'   select = select_spec(
#'     label = "Select variable:",
#'     choices = c("AVAL", "CHG"),
#'     selected = "AVAL",
#'     multiple = TRUE,
#'     fixed = FALSE
#'   )
#' )
#' app <- shinyApp(
#'   ui = fluidPage(
#'     teal.widgets::standard_layout(
#'       output = div(
#'         verbatimTextOutput("expr"),
#'         dataTableOutput("data")
#'       ),
#'       encoding = tagList(
#'         data_extract_ui("adsl_var", label = "ADSL selection", adsl_extract),
#'         data_extract_ui("adlb_var", label = "ADLB selection", adlb_extract)
#'       )
#'     )
#'   ),
#'   server = function(input, output, session) {
#'     merged_data <- data_merge_module(
#'       data_extract = list(adsl_var = adsl_extract, adlb_var = adlb_extract),
#'       datasets = datasets,
#'       merge_function = "dplyr::left_join"
#'     )
#'     output$expr <- renderText(merged_data()$expr)
#'     output$data <- renderDataTable(merged_data()$data())
#'   }
#' )
#' \dontrun{
#' runApp(app)
#' }
data_merge_module <- function(datasets,
                              data_extract,
                              merge_function = "dplyr::full_join",
                              anl_name = "ANL",
                              id = "merge_id") {
  logger::log_trace("data_merge_module called with: { paste(datasets$datanames(), collapse = ', ') } datasets.")

  checkmate::assert_list(data_extract, c("list", "data_extract_spec", "NULL"))
  lapply(data_extract, function(x) {
    if (is.list(x) && !inherits(x, "data_extract_spec")) {
      checkmate::assert_list(x, "data_extract_spec")
    }
  })

  selector_list <- data_extract_multiple_srv(data_extract, datasets)

  data_merge_srv(
    id = id,
    selector_list = selector_list,
    datasets = datasets,
    merge_function = merge_function,
    anl_name = anl_name
  )
}


#' Data merge module server
#'
#' @description `r lifecycle::badge("experimental")`
#' @details When additional processing of the `data_extract` list input is required, `data_merge_srv()` can be combined
#'   with `data_extract_multiple_srv()` or `data_extract_srv()` to influence the `selector_list` input.
#'   Compare the example below with that found in [data_merge_module()].
#'
#' @inheritParams shiny::moduleServer
#' @param selector_list (`reactive`)\cr
#'   output from [data_extract_multiple_srv()] or a reactive named list of outputs from [data_extract_srv()].
#'   When using a reactive named list, the names must be identical to the shiny ids of the respective [data_extract_ui()].
#' @param datasets (`FilteredData`)\cr
#'  object containing data (see `teal.slice::FilteredData`).
#' @param merge_function (`character(1)` or `reactive`)\cr
#'  A character string of a function that accepts the arguments
#'  `x`, `y` and `by` to perform the merging of datasets.
#' @param anl_name (`character(1)`)\cr
#'  Name of the analysis dataset.
#'
#' @return reactive expression with output from [merge_datasets].
#'
#' @seealso [data_extract_srv()]
#'
#' @export
#'
#' @examples
#' library(shiny)
#'
#' ADSL <- data.frame(
#'   STUDYID = "A",
#'   USUBJID = LETTERS[1:10],
#'   SEX = rep(c("F", "M"), 5),
#'   AGE = rpois(10, 30),
#'   BMRKR1 = rlnorm(10)
#' )
#' ADLB <- expand.grid(
#'   STUDYID = "A",
#'   USUBJID = LETTERS[1:10],
#'   PARAMCD = c("ALT", "CRP", "IGA"),
#'   AVISIT = c("SCREENING", "BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15")
#' )
#' ADLB$AVAL <- rlnorm(120)
#' ADLB$CHG <- rnorm(120)
#'
#' data <- teal.data::cdisc_data(
#'   teal.data::cdisc_dataset("ADSL", ADSL),
#'   teal.data::cdisc_dataset("ADLB", ADLB)
#' )
#' datasets <- teal.slice::init_filtered_data(data)
#'
#' adsl_extract <- data_extract_spec(
#'   dataname = "ADSL",
#'   select = select_spec(
#'     label = "Select variable:",
#'     choices = c("AGE", "BMRKR1"),
#'     selected = "AGE",
#'     multiple = TRUE,
#'     fixed = FALSE
#'   )
#' )
#' adlb_extract <- data_extract_spec(
#'   dataname = "ADLB",
#'   filter = filter_spec(vars = "PARAMCD", choices = c("ALT", "CRP", "IGA"), selected = "ALT"),
#'   select = select_spec(
#'     label = "Select variable:",
#'     choices = c("AVAL", "CHG"),
#'     selected = "AVAL",
#'     multiple = TRUE,
#'     fixed = FALSE
#'   )
#' )
#'
#' app <- shinyApp(
#'   ui = fluidPage(
#'     teal.widgets::standard_layout(
#'       output = div(
#'         verbatimTextOutput("expr"),
#'         dataTableOutput("data")
#'       ),
#'       encoding = tagList(
#'         data_extract_ui("adsl_var", label = "ADSL selection", adsl_extract),
#'         data_extract_ui("adlb_var", label = "ADLB selection", adlb_extract)
#'       )
#'     )
#'   ),
#'   server = function(input, output, session) {
#'     selector_list <- data_extract_multiple_srv(
#'       list(adsl_var = adsl_extract, adlb_var = adlb_extract),
#'       datasets
#'     )
#'     merged_data <- data_merge_srv(
#'       selector_list = selector_list,
#'       datasets = datasets,
#'       merge_function = "dplyr::left_join"
#'     )
#'     output$expr <- renderText(merged_data()$expr)
#'     output$data <- renderDataTable(merged_data()$data())
#'   }
#' )
#' \dontrun{
#' runApp(app)
#' }
data_merge_srv <- function(id = "merge_id",
                           selector_list,
                           datasets,
                           merge_function = "dplyr::full_join",
                           anl_name = "ANL") {
  checkmate::assert_string(anl_name)
  stopifnot(make.names(anl_name) == anl_name)
  checkmate::assert_class(selector_list, "reactive")
  checkmate::assert_class(datasets, "FilteredData")
  moduleServer(
    id,
    function(input, output, session) {
      logger::log_trace(
        "data_merge_srv initialized with: { paste(datasets$datanames(), collapse = ', ') } datasets."
      )
      reactive({
        checkmate::assert_list(selector_list(), names = "named", types = "reactive")
        merge_fun_name <- if (inherits(merge_function, "reactive")) merge_function() else merge_function

        datasets_list <- sapply(
          datasets$datanames(),
          simplify = FALSE,
          function(x) reactive(datasets$get_data(x, filtered = TRUE))
        )
        join_keys <- datasets$get_join_keys()
        check_merge_function(merge_fun_name)

        # function to filter out selectors which are NULL or only have validator
        f <- function(x) {
          is.null(x) || (length(names(x)) == 1 && names(x) == "iv")
        }

        ds <- Filter(Negate(f), lapply(selector_list(), function(x) x()))

        validate(need(length(ds) > 0, "At least one dataset needs to be selected"))
        merged_data <- merge_datasets(
          selector_list = ds,
          datasets = datasets_list,
          join_keys = join_keys,
          merge_function = merge_fun_name,
          anl_name = anl_name
        )
        ch <- teal.code::chunks_new()
        datasets_list_nr <- sapply(datasets$datanames(), simplify = FALSE, datasets$get_data, filtered = TRUE)

        teal.code::chunks_reset(envir = list2env(datasets_list_nr), chunks = ch)
        for (chunk in merged_data$expr) teal.code::chunks_push(expression = chunk, chunks = ch)
        teal.code::chunks_safe_eval(chunks = ch)
        merged_data$data <- reactive({
          ch$get(anl_name)
        })
        merged_data$chunks <- ch
        merged_data$expr <- paste(merged_data$expr, collapse = "\n")
        merged_data
      })
    }
  )
}
