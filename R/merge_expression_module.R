#' Merge expression module
#'
#' @description `r lifecycle::badge("experimental")`
#' @details This function is a convenient wrapper to combine `data_extract_multiple_srv()` and
#' `merge_expression_srv()` when no additional processing is required.
#' Compare the example below with that found in [merge_expression_srv()].
#'
#' @inheritParams shiny::moduleServer
#' @param data (named `list`)\cr of datasets.
#' @param datasets (`FilteredData`)\cr
#'  object containing data, see [teal.slice::FilteredData] for more.
#' @param join_keys (named `list`)\cr of variables used as join keys for each of the datasets in `data`.
#' @param data_extract (named `list` of `data_extract_spec`)\cr
#' @param merge_function (`character(1)`)\cr
#'  A character string of a function that
#'  accepts the arguments `x`, `y` and `by` to perform the merging of datasets.
#' @param anl_name (`character(1)`)\cr
#'  Name of the analysis dataset.
#'
#' @return reactive expression with output from [merge_expression_srv()].
#'
#' @seealso [merge_expression_srv()]
#'
#' @export
#'
#' @examples
#' library(shiny)
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
#' data_list <- list(
#'   ADSL = ADSL,
#'   ADLB = ADLB
#' )
#'
#' join_keys <- list(
#' ADSL = list(
#'   ADSL = c(STUDYID = "STUDYID", USUBJID = "USUBJID"),
#'   ADLB = c(STUDYID = "STUDYID", USUBJID = "USUBJID")
#' ),
#' ADLB = list(
#'   ADLB = c(STUDYID = "STUDYID", USUBJID = "USUBJID", PARAMCD = "PARAMCD", AVISIT = "AVISIT"),
#'   ADSL = c(STUDYID = "STUDYID", USUBJID = "USUBJID")
#' )
#' )
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
#'     chunks_h <- teal.code::chunks_new()
#'
#'     teal.code::chunks_push(
#'       str2lang("ADSL <- data.frame(
#'       STUDYID = 'A',
#'       USUBJID = LETTERS[1:10],
#'       SEX = rep(c('F', 'M'), 5),
#'       AGE = rpois(10, 30),
#'       BMRKR1 = rlnorm(10))"),
#'       chunks = chunks_h
#'     )
#'
#'     teal.code::chunks_push(
#'       str2lang("ADLB <- expand.grid(
#'       STUDYID = 'A',
#'       USUBJID = LETTERS[1:10],
#'       PARAMCD = c('ALT', 'CRP', 'IGA'),
#'       AVISIT = c('SCREENING', 'BASELINE', 'WEEK 1 DAY 8', 'WEEK 2 DAY 15'),
#'       AVAL = rlnorm(120),
#'       CHG = rlnorm(120))"),
#'       chunks = chunks_h
#'     )
#'
#'     merged_data <- merge_expression_module(
#'       data_extract = list(adsl_var = adsl_extract, adlb_var = adlb_extract),
#'       data = data_list,
#'       datasets = datasets,
#'       join_keys = join_keys,
#'       merge_function = "dplyr::left_join"
#'     )
#'
#'     ch_merge <- reactive({
#'       ch <- teal.code::chunks_deep_clone(chunks_h)
#'       for (chunk in merged_data()$expr) teal.code::chunks_push(chunks = ch, expression = chunk)
#'       ch$eval()
#'       ch
#'     })
#'
#'     output$expr <- renderText(paste(merged_data()$expr, collapse = "\n"))
#'     output$data <- renderDataTable(ch_merge()$get("ANL"))
#'   }
#' )
#' \dontrun{
#' runApp(app)
#' }
merge_expression_module <- function(data,
                                    datasets,
                                    join_keys = NULL,
                                    data_extract,
                                    merge_function = "dplyr::full_join",
                                    anl_name = "ANL",
                                    id = "merge_id") {
  logger::log_trace("merge_expression_module called with: { paste(names(data), collapse = ', ') } datasets.")

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

  merge_expression_srv(
    id = id,
    selector_list = selector_list,
    data = data,
    join_keys = join_keys,
    merge_function = merge_function,
    anl_name = anl_name
  )
}


#' Data merge module server
#'
#' @description `r lifecycle::badge("experimental")`
#' @details When additional processing of the `data_extract` list input is required, `merge_expression_srv()` can be combined
#'   with `data_extract_multiple_srv()` or `data_extract_srv()` to influence the `selector_list` input.
#'   Compare the example below with that found in [merge_expression_module()].
#'
#' @inheritParams shiny::moduleServer
#' @param data (named `list`)\cr of datasets.
#' @param join_keys (named `list`)\cr of variables used as join keys for each of the datasets in `data`.
#' @param selector_list (`reactive`)\cr
#'   output from [data_extract_multiple_srv()] or a reactive named list of outputs from [data_extract_srv()].
#'   When using a reactive named list, the names must be identical to the shiny ids of the respective [data_extract_ui()].
#' @param merge_function (`character(1)` or `reactive`)\cr
#'  A character string of a function that accepts the arguments
#'  `x`, `y` and `by` to perform the merging of datasets.
#' @param anl_name (`character(1)`)\cr
#'  Name of the analysis dataset.
#'
#' @return reactive expression with output from [merge_datasets()].
#'
#' @seealso [merge_expression_srv()]
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
#'
#' ADLB <- expand.grid(
#'   STUDYID = "A",
#'   USUBJID = LETTERS[1:10],
#'   PARAMCD = c("ALT", "CRP", "IGA"),
#'   AVISIT = c("SCREENING", "BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15")
#' )
#' ADLB$AVAL <- rlnorm(120)
#' ADLB$CHG <- rlnorm(120)
#'
#' data <- teal.data::cdisc_data(
#'   teal.data::cdisc_dataset("ADSL", ADSL),
#'   teal.data::cdisc_dataset("ADLB", ADLB)
#' )
#' datasets <- teal.slice:::filtered_data_new(data)
#' teal.slice:::filtered_data_set(data, datasets)
#'
#' data_list <- list(
#'   ADSL = ADSL,
#'   ADLB = ADLB
#' )
#'
#' join_keys <- list(
#' ADSL = list(
#'   ADSL = c(STUDYID = "STUDYID", USUBJID = "USUBJID"),
#'   ADLB = c(STUDYID = "STUDYID", USUBJID = "USUBJID")
#' ),
#' ADLB = list(
#'   ADLB = c(STUDYID = "STUDYID", USUBJID = "USUBJID", PARAMCD = "PARAMCD", AVISIT = "AVISIT"),
#'   ADSL = c(STUDYID = "STUDYID", USUBJID = "USUBJID")
#' )
#' )
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
#'     chunks_h <- teal.code::chunks_new()
#'
#'     teal.code::chunks_push(
#'       str2lang("ADSL <- data.frame(
#'       STUDYID = 'A',
#'       USUBJID = LETTERS[1:10],
#'       SEX = rep(c('F', 'M'), 5),
#'       AGE = rpois(10, 30),
#'       BMRKR1 = rlnorm(10))"),
#'       chunks = chunks_h
#'     )
#'
#'     teal.code::chunks_push(
#'       str2lang("ADLB <- expand.grid(
#'       STUDYID = 'A',
#'       USUBJID = LETTERS[1:10],
#'       PARAMCD = c('ALT', 'CRP', 'IGA'),
#'       AVISIT = c('SCREENING', 'BASELINE', 'WEEK 1 DAY 8', 'WEEK 2 DAY 15'),
#'       AVAL = rlnorm(120),
#'       CHG = rlnorm(120))"),
#'       chunks = chunks_h
#'     )
#'
#'     selector_list <- data_extract_multiple_srv(
#'       list(adsl_var = adsl_extract, adlb_var = adlb_extract),
#'       datasets = datasets
#'     )
#'     merged_data <- merge_expression_srv(
#'       selector_list = selector_list,
#'       data = data_list,
#'       join_keys = join_keys,
#'       merge_function = "dplyr::left_join"
#'     )
#'
#'     ch_merge <- reactive({
#'       ch <- teal.code::chunks_deep_clone(chunks_h)
#'       teal.code::chunks_push(chunks = ch, expression = merged_data()$expr)
#'       ch$eval()
#'       ch
#'     })
#'
#'     output$expr <- renderText(paste(merged_data()$expr, collapse = "\n"))
#'     output$data <- renderDataTable(ch_merge()$get("ANL"))
#'   }
#' )
#' \dontrun{
#' runApp(app)
#' }

merge_expression_srv <- function(id = "merge_id",
                                 selector_list,
                                 data,
                                 join_keys = NULL,
                                 merge_function = "dplyr::full_join",
                                 anl_name = "ANL") {
  checkmate::assert_string(anl_name)
  stopifnot(make.names(anl_name) == anl_name)
  checkmate::assert_class(selector_list, "reactive")
  checkmate::assert_list(data, names = "named")
  checkmate::assert_list(join_keys, names = "named")
  moduleServer(
    id,
    function(input, output, session) {
      logger::log_trace(
        "merge_expression_srv initialized with: { paste(names(data), collapse = ', ') } datasets."
      )
      reactive({
        checkmate::assert_list(selector_list(), names = "named", types = "reactive")
        merge_fun_name <- if (inherits(merge_function, "reactive")) merge_function() else merge_function
        check_merge_function(merge_fun_name)

        ds <- Filter(Negate(is.null), lapply(selector_list(), function(x) x()))
        validate(need(length(ds) > 0, "At least one dataset needs to be selected"))
        merge_datasets(
          selector_list = ds,
          data = data,
          join_keys = join_keys,
          merge_function = merge_fun_name,
          anl_name = anl_name
        )
      })
    }
  )
}
