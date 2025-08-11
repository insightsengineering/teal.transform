#' Merge expression module
#'
#' @description
#'
#' Convenient wrapper to combine `data_extract_multiple_srv()` and
#' `merge_expression_srv()` when no additional processing is required.
#' Compare the example below with that found in [merge_expression_srv()].
#'
#' @inheritParams shiny::moduleServer
#' @param datasets (named `list` of `reactive` or non-`reactive` `data.frame`)
#' object containing data as a list of `data.frame`.
#' When passing a list of non-reactive `data.frame` objects, they are
#' converted to reactive `data.frame` objects internally.
#' @param join_keys (`join_keys`)
#' of variables used as join keys for each of the datasets in `datasets`.
#' This will be used to extract the `keys` of every dataset.
#' @param data_extract (named `list` of `data_extract_spec`).
#' @param merge_function (`character(1)`)
#' A character string of a function that accepts the arguments `x`, `y` and
#' `by` to perform the merging of datasets.
#' @param anl_name (`character(1)`)
#' Name of the analysis dataset.
#'
#' @return Reactive expression with output from [merge_expression_srv()].
#'
#' @seealso [merge_expression_srv()]
#'
#' @examples
#' library(shiny)
#' library(teal.data)
#' library(teal.widgets)
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
#' data_list <- list(
#'   ADSL = reactive(ADSL),
#'   ADLB = reactive(ADLB)
#' )
#'
#' join_keys <- join_keys(
#'   join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")),
#'   join_key("ADSL", "ADLB", c("STUDYID", "USUBJID")),
#'   join_key("ADLB", "ADLB", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
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
#'
#' ui <- bslib::page_fluid(
#'   bslib::layout_sidebar(
#'     tags$div(
#'       verbatimTextOutput("expr"),
#'       dataTableOutput("data")
#'     ),
#'     sidebar = tagList(
#'       data_extract_ui("adsl_var", label = "ADSL selection", adsl_extract),
#'       data_extract_ui("adlb_var", label = "ADLB selection", adlb_extract)
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data_q <- qenv()
#'
#'   data_q <- eval_code(
#'     data_q,
#'     "ADSL <- data.frame(
#'         STUDYID = 'A',
#'         USUBJID = LETTERS[1:10],
#'         SEX = rep(c('F', 'M'), 5),
#'         AGE = rpois(10, 30),
#'         BMRKR1 = rlnorm(10)
#'       )"
#'   )
#'
#'   data_q <- eval_code(
#'     data_q,
#'     "ADLB <- expand.grid(
#'         STUDYID = 'A',
#'         USUBJID = LETTERS[1:10],
#'         PARAMCD = c('ALT', 'CRP', 'IGA'),
#'         AVISIT = c('SCREENING', 'BASELINE', 'WEEK 1 DAY 8', 'WEEK 2 DAY 15'),
#'         AVAL = rlnorm(120),
#'         CHG = rlnorm(120)
#'        )"
#'   )
#'
#'   merged_data <- merge_expression_module(
#'     data_extract = list(adsl_var = adsl_extract, adlb_var = adlb_extract),
#'     datasets = data_list,
#'     join_keys = join_keys,
#'     merge_function = "dplyr::left_join"
#'   )
#'
#'   code_merge <- reactive({
#'     for (exp in merged_data()$expr) data_q <- eval_code(data_q, exp)
#'     data_q
#'   })
#'
#'   output$expr <- renderText(paste(merged_data()$expr, collapse = "\n"))
#'   output$data <- renderDataTable(code_merge()[["ANL"]])
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#' @export
#'
merge_expression_module <- function(datasets,
                                    join_keys = NULL,
                                    data_extract,
                                    merge_function = "dplyr::full_join",
                                    anl_name = "ANL",
                                    id = "merge_id") {
  UseMethod("merge_expression_module", datasets)
}

#' @rdname merge_expression_module
#' @export
#'
merge_expression_module.reactive <- function(datasets,
                                             join_keys = NULL,
                                             data_extract,
                                             merge_function = "dplyr::full_join",
                                             anl_name = "ANL",
                                             id = "merge_id") {
  checkmate::assert_class(isolate(datasets()), "teal_data")
  datasets_new <- convert_teal_data(datasets)
  if (is.reactive(datasets) && inherits(isolate(datasets()), "teal_data")) {
    join_keys <- isolate(teal.data::join_keys(datasets()))
  }
  merge_expression_module(datasets_new, join_keys, data_extract, merge_function, anl_name, id)
}

#' @rdname merge_expression_module
#' @export
#'
merge_expression_module.list <- function(datasets,
                                         join_keys = NULL,
                                         data_extract,
                                         merge_function = "dplyr::full_join",
                                         anl_name = "ANL",
                                         id = "merge_id") {
  logger::log_debug("merge_expression_module called with: { paste(names(datasets), collapse = ', ') } datasets.")
  checkmate::assert_list(datasets, names = "named")
  checkmate::assert_list(data_extract, names = "named", types = c("list", "data_extract_spec", "NULL"))
  checkmate::assert_class(join_keys, "join_keys")
  lapply(data_extract, function(x) {
    if (is.list(x) && !inherits(x, "data_extract_spec")) {
      checkmate::assert_list(x, "data_extract_spec")
    }
  })

  selector_list <- data_extract_multiple_srv(data_extract, datasets, join_keys)

  merge_expression_srv(
    id = id,
    selector_list = selector_list,
    datasets = datasets,
    join_keys = join_keys,
    merge_function = merge_function,
    anl_name = anl_name
  )
}

#' Data merge module server
#'
#' When additional processing of the `data_extract` list input is required,
#' `merge_expression_srv()` can be combined with `data_extract_multiple_srv()`
#' or `data_extract_srv()` to influence the `selector_list` input.
#' Compare the example below with that found in [merge_expression_module()].
#'
#' @inheritParams merge_expression_module
#' @param selector_list (`reactive`)
#' output from [data_extract_multiple_srv()] or a reactive named list of
#' outputs from [data_extract_srv()].
#' When using a reactive named list, the names must be identical to the shiny
#' ids of the respective
#' [data_extract_ui()].
#' @param merge_function (`character(1)` or `reactive`)
#'  A character string of a function that accepts the arguments
#'  `x`, `y` and `by` to perform the merging of datasets.
#' @param anl_name (`character(1)`)
#'  Name of the analysis dataset.
#'
#' @inherit merge_expression_module return
#'
#' @seealso [merge_expression_module()]
#'
#' @examples
#' library(shiny)
#' library(teal.data)
#' library(teal.widgets)
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
#' data_list <- list(
#'   ADSL = reactive(ADSL),
#'   ADLB = reactive(ADLB)
#' )
#'
#' join_keys <- join_keys(
#'   join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")),
#'   join_key("ADSL", "ADLB", c("STUDYID", "USUBJID")),
#'   join_key("ADLB", "ADLB", c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"))
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
#'
#' ui <- bslib::page_fluid(
#'   bslib::layout_sidebar(
#'     tags$div(
#'       verbatimTextOutput("expr"),
#'       dataTableOutput("data")
#'     ),
#'     sidebar = tagList(
#'       data_extract_ui("adsl_var", label = "ADSL selection", adsl_extract),
#'       data_extract_ui("adlb_var", label = "ADLB selection", adlb_extract)
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data_q <- qenv()
#'
#'   data_q <- eval_code(
#'     data_q,
#'     "ADSL <- data.frame(
#'         STUDYID = 'A',
#'         USUBJID = LETTERS[1:10],
#'         SEX = rep(c('F', 'M'), 5),
#'         AGE = rpois(10, 30),
#'         BMRKR1 = rlnorm(10)
#'       )"
#'   )
#'
#'   data_q <- eval_code(
#'     data_q,
#'     "ADLB <- expand.grid(
#'         STUDYID = 'A',
#'         USUBJID = LETTERS[1:10],
#'         PARAMCD = c('ALT', 'CRP', 'IGA'),
#'         AVISIT = c('SCREENING', 'BASELINE', 'WEEK 1 DAY 8', 'WEEK 2 DAY 15'),
#'         AVAL = rlnorm(120),
#'         CHG = rlnorm(120)
#'       )"
#'   )
#'
#'   selector_list <- data_extract_multiple_srv(
#'     list(adsl_var = adsl_extract, adlb_var = adlb_extract),
#'     datasets = data_list
#'   )
#'   merged_data <- merge_expression_srv(
#'     selector_list = selector_list,
#'     datasets = data_list,
#'     join_keys = join_keys,
#'     merge_function = "dplyr::left_join"
#'   )
#'
#'   code_merge <- reactive({
#'     for (exp in merged_data()$expr) data_q <- eval_code(data_q, exp)
#'     data_q
#'   })
#'
#'   output$expr <- renderText(paste(merged_data()$expr, collapse = "\n"))
#'   output$data <- renderDataTable(code_merge()[["ANL"]])
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#' @export
#'
merge_expression_srv <- function(id = "merge_id",
                                 selector_list,
                                 datasets,
                                 join_keys,
                                 merge_function = "dplyr::full_join",
                                 anl_name = "ANL") {
  UseMethod("merge_expression_srv", datasets)
}

#' @rdname merge_expression_srv
#' @export
merge_expression_srv.reactive <- function(id = "merge_id",
                                          selector_list,
                                          datasets,
                                          join_keys,
                                          merge_function = "dplyr::full_join",
                                          anl_name = "ANL") {
  checkmate::assert_class(isolate(datasets()), "teal_data")
  datasets_new <- convert_teal_data(datasets)
  if (is.reactive(datasets) && inherits(isolate(datasets()), "teal_data")) {
    join_keys <- isolate(teal.data::join_keys(datasets()))
  }
  merge_expression_srv(id, selector_list, datasets_new, join_keys, merge_function, anl_name)
}

#' @rdname merge_expression_srv
#' @export
merge_expression_srv.list <- function(id = "merge_id",
                                      selector_list,
                                      datasets,
                                      join_keys,
                                      merge_function = "dplyr::full_join",
                                      anl_name = "ANL") {
  checkmate::assert_list(datasets, names = "named")
  checkmate::assert_string(anl_name)
  stopifnot(make.names(anl_name) == anl_name)
  checkmate::assert_class(selector_list, "reactive")
  checkmate::assert_class(join_keys, "join_keys")

  moduleServer(
    id,
    function(input, output, session) {
      logger::log_debug(
        "merge_expression_srv initialized with: { paste(names(datasets), collapse = ', ') } datasets."
      )

      reactive({
        checkmate::assert_list(selector_list(), names = "named", types = "reactive")
        merge_fun_name <- if (inherits(merge_function, "reactive")) merge_function() else merge_function
        check_merge_function(merge_fun_name)

        # function to filter out selectors which are NULL or only have validator
        f <- function(x) {
          is.null(x) || (length(names(x)) == 1 && names(x) == "iv")
        }

        ds <- Filter(Negate(f), lapply(selector_list(), function(x) x()))
        validate(need(length(ds) > 0, "At least one dataset needs to be selected"))
        merge_datasets(
          selector_list = ds,
          datasets = datasets,
          join_keys = join_keys,
          merge_function = merge_fun_name,
          anl_name = anl_name
        )
      })
    }
  )
}
