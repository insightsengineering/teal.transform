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
#' @param data_extract (`list` of `data_extract_spec`)\cr
#'  The usage of named list as input can replace `input_id` argument.
#' @param input_id (`character`)\cr
#'  vector of input IDs to read from.
#' `input_id` is optional as a named list in `data_extract` can replace it.
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
#' @note `input_id` argument is deprecated and will be removed in future releases.
#' Please consider using a named list for the `data_extract` argument as an alternative.
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
#' datasets <- teal.slice:::filtered_data_new(data)
#' teal.slice:::filtered_data_set(data, datasets)
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
                              input_id,
                              merge_function = "dplyr::full_join",
                              anl_name = "ANL",
                              id = "merge_id") {
  logger::log_trace("data_merge_module called with: { paste(datasets$datanames(), collapse = ', ') } datasets.")

  if (!missing(input_id)) {
    names(data_extract) <- input_id
    lifecycle::deprecate_soft(
      when = "0.2.13",
      what = "data_merge_module(input_id = )",
      details =
        "Please consider passing a named data_extract list to `data_merge_module` to replace input_id
      argument in the future."
    )
  }

  checkmate::assert_list(data_extract)
  stopifnot(
    all(vapply(
      data_extract,
      function(x) {
        inherits(x, "data_extract_spec") || all(vapply(x, inherits, logical(1), "data_extract_spec"))
      },
      logical(1)
    ))
  )

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
#' datasets <- teal.slice:::filtered_data_new(data)
#' teal.slice:::filtered_data_set(data, datasets)
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
        check_merge_function(merge_fun_name)

        ds <- Filter(Negate(is.null), lapply(selector_list(), function(x) x()))
        validate(need(length(ds) > 0, "At least one dataset needs to be selected"))
        merge_datasets(
          ds,
          datasets = datasets,
          merge_function = merge_fun_name,
          anl_name = anl_name
        )
      })
    }
  )
}
