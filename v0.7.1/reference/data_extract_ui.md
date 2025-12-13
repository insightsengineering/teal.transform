# `teal` data extraction module user-interface

Data extraction module.

## Usage

``` r
data_extract_ui(id, label, data_extract_spec, is_single_dataset = FALSE)
```

## Arguments

- id:

  (`character`) shiny input unique identifier.

- label:

  (`character`) Label above the data extract input.

- data_extract_spec:

  (`list` of `data_extract_spec`) This is the outcome of listing
  [`data_extract_spec()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_spec.md)
  constructor calls.

- is_single_dataset:

  (`logical`) `FALSE` to display the dataset widget.

## Value

Shiny
[`shiny::selectInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)`s`
that allow to define how to extract data from a specific dataset. The
input elements will be returned inside a
[shiny::div](https://rdrr.io/pkg/shiny/man/reexports.html) container.

## Details

There are three inputs that will be rendered

1.  Dataset select Optional. If more than one
    [data_extract_spec](https://insightsengineering.github.io/teal.transform/reference/data_extract_spec.md)
    is handed over to the function, a shiny
    [shiny::selectInput](https://rdrr.io/pkg/shiny/man/selectInput.html)
    will be rendered. Else just the name of the dataset is given.

2.  Filter Panel Optional. If the
    [data_extract_spec](https://insightsengineering.github.io/teal.transform/reference/data_extract_spec.md)
    contains a filter element a shiny
    [shiny::selectInput](https://rdrr.io/pkg/shiny/man/selectInput.html)
    will be rendered with the options to filter the dataset.

3.  Select panel A shiny
    [shiny::selectInput](https://rdrr.io/pkg/shiny/man/selectInput.html)
    to select columns from the dataset to go into the analysis.

The output can be analyzed using `data_extract_srv(...)`.

This functionality should be used in the encoding panel of your `teal`
app. It will allow app-developers to specify a
[`data_extract_spec()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_spec.md)
object. This object should be used to `teal` module variables being
filtered data from CDISC datasets.

You can use this function in the same way as any
[`shiny module`](https://shiny.rstudio.com/articles/modules.html) UI.
The corresponding server module can be found in
[`data_extract_srv()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_srv.md).

## Examples

``` r
library(shiny)
library(teal.widgets)

adtte_filters <- filter_spec(
  vars = c("PARAMCD", "CNSR"),
  sep = "-",
  choices = c("OS-1" = "OS-1", "OS-0" = "OS-0", "PFS-1" = "PFS-1"),
  selected = "OS-1",
  multiple = FALSE,
  label = "Choose endpoint and Censor"
)

response_spec <- data_extract_spec(
  dataname = "ADTTE",
  filter = adtte_filters,
  select = select_spec(
    choices = c("AVAL", "BMRKR1", "AGE"),
    selected = c("AVAL", "BMRKR1"),
    multiple = TRUE,
    fixed = FALSE,
    label = "Column"
  )
)
# Call to use inside your teal module UI function
bslib::layout_sidebar(
  tableOutput("table"),
  sidebar = tags$div(
    data_extract_ui(
      id = "regressor",
      label = "Regressor Variable",
      data_extract_spec = response_spec
    )
  )
)
#> <div class="container-fluid">
#>   <div class="bslib-sidebar-layout bslib-mb-spacing html-fill-item" data-bslib-sidebar-init="TRUE" data-collapsible-desktop="true" data-collapsible-mobile="true" data-open-desktop="open" data-open-mobile="closed" data-require-bs-caller="layout_sidebar()" data-require-bs-version="5" style="--_sidebar-width:250px;">
#>     <div class="main bslib-gap-spacing html-fill-container">
#>       <div id="table" class="shiny-html-output shiny-table-output"></div>
#>     </div>
#>     <aside id="bslib-sidebar-9657" class="sidebar" hidden>
#>       <div class="sidebar-content bslib-gap-spacing">
#>         <div>
#>           <div>
#>             <div class="data-extract">
#>               <label>Regressor Variable</label>
#>               <span class="help-block">
#>                 Dataset:
#>                 <code>ADTTE</code>
#>               </span>
#>               <div>
#>                 <div>
#>                   <div class="filter_spec">
#>                     <div class="shinyjs-hide">
#>                       <script>
#>         $(function() {
#>           $('#regressor-dataset_ADTTE_singleextract-filter1-col').on('change', function(e) {
#>             var select_concat = $(this).val().length ? $(this).val().join(', ') : 'NULL';
#>             $('#regressor-dataset_ADTTE_singleextract-filter1-col_selected_text').html(select_concat);
#>           })
#>         })</script>
#>                       <div>
#>                         <div id="regressor-dataset_ADTTE_singleextract-filter1-col_input" style="display: none;">
#>                           <div class="form-group shiny-input-container">
#>                             <label class="control-label shiny-label-null" for="regressor-dataset_ADTTE_singleextract-filter1-col" id="regressor-dataset_ADTTE_singleextract-filter1-col-label"></label>
#>                             <select data-actions-box="true" data-none-selected-text="- Nothing selected -" data-allow-clear="false" data-max-options="Inf" data-show-subtext="true" data-live-search="false" data-container="body" data-state-input="true" id="regressor-dataset_ADTTE_singleextract-filter1-col" class="selectpicker form-control" autocomplete="off" multiple="multiple"><option value="PARAMCD" selected>PARAMCD</option>
#> <option value="CNSR" selected>CNSR</option></select>
#>                           </div>
#>                         </div>
#>                         <div id="regressor-dataset_ADTTE_singleextract-filter1-col_fixed" style="display: block;">
#>                           <label class="control-label"></label>
#>                           <code id="regressor-dataset_ADTTE_singleextract-filter1-col_selected_text">PARAMCD, CNSR</code>
#>                         </div>
#>                       </div>
#>                     </div>
#>                     <div>
#>                       <script>
#>         $(function() {
#>           $('#regressor-dataset_ADTTE_singleextract-filter1-vals').on('change', function(e) {
#>             var select_concat = $(this).val().length ? $(this).val().join(', ') : 'NULL';
#>             $('#regressor-dataset_ADTTE_singleextract-filter1-vals_selected_text').html(select_concat);
#>           })
#>         })</script>
#>                       <div>
#>                         <div id="regressor-dataset_ADTTE_singleextract-filter1-vals_input" style="display: block;">
#>                           <div class="form-group shiny-input-container">
#>                             <label class="control-label" id="regressor-dataset_ADTTE_singleextract-filter1-vals-label" for="regressor-dataset_ADTTE_singleextract-filter1-vals">Choose endpoint and Censor</label>
#>                             <select data-actions-box="false" data-none-selected-text="- Nothing selected -" data-allow-clear="true" data-max-options="1" data-show-subtext="true" data-live-search="false" data-container="body" data-state-input="true" id="regressor-dataset_ADTTE_singleextract-filter1-vals" class="selectpicker form-control" autocomplete="off" multiple="multiple"><option value="OS-1" selected>OS-1</option>
#> <option value="OS-0">OS-0</option>
#> <option value="PFS-1">PFS-1</option></select>
#>                           </div>
#>                         </div>
#>                         <div id="regressor-dataset_ADTTE_singleextract-filter1-vals_fixed" style="display: none;">
#>                           <label class="control-label">Choose endpoint and Censor</label>
#>                           <code id="regressor-dataset_ADTTE_singleextract-filter1-vals_selected_text">OS-1</code>
#>                         </div>
#>                       </div>
#>                     </div>
#>                   </div>
#>                 </div>
#>                 <div>
#>                   <script>
#>         $(function() {
#>           $('#regressor-dataset_ADTTE_singleextract-select').on('change', function(e) {
#>             var select_concat = $(this).val().length ? $(this).val().join(', ') : 'NULL';
#>             $('#regressor-dataset_ADTTE_singleextract-select_selected_text').html(select_concat);
#>           })
#>         })</script>
#>                   <div>
#>                     <div id="regressor-dataset_ADTTE_singleextract-select_input" style="display: block;">
#>                       <div class="form-group shiny-input-container">
#>                         <label class="control-label" id="regressor-dataset_ADTTE_singleextract-select-label" for="regressor-dataset_ADTTE_singleextract-select">Column</label>
#>                         <select data-actions-box="true" data-none-selected-text="- Nothing selected -" data-allow-clear="false" data-max-options="Inf" data-show-subtext="true" data-live-search="false" data-container="body" data-state-input="true" id="regressor-dataset_ADTTE_singleextract-select" class="selectpicker form-control" autocomplete="off" multiple="multiple"><option value="AVAL" selected>AVAL</option>
#> <option value="BMRKR1" selected>BMRKR1</option>
#> <option value="AGE">AGE</option></select>
#>                       </div>
#>                     </div>
#>                     <div id="regressor-dataset_ADTTE_singleextract-select_fixed" style="display: none;">
#>                       <label class="control-label">Column</label>
#>                       <code id="regressor-dataset_ADTTE_singleextract-select_selected_text">AVAL, BMRKR1</code>
#>                     </div>
#>                   </div>
#>                 </div>
#>                 <div class="form-group shiny-input-container shinyjs-disabled shinyjs-hide">
#>                   <div class="checkbox">
#>                     <label>
#>                       <input id="regressor-dataset_ADTTE_singleextract-reshape" type="checkbox" class="shiny-input-checkbox"/>
#>                       <span>Reshape long to wide format</span>
#>                     </label>
#>                   </div>
#>                 </div>
#>               </div>
#>             </div>
#>           </div>
#>         </div>
#>       </div>
#>     </aside>
#>     <button class="collapse-toggle" type="button" title="Toggle sidebar" aria-expanded="true" aria-controls="bslib-sidebar-9657"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-chevron-left collapse-icon" style="height:;width:;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img" ><path fill-rule="evenodd" d="M11.354 1.646a.5.5 0 0 1 0 .708L5.707 8l5.647 5.646a.5.5 0 0 1-.708.708l-6-6a.5.5 0 0 1 0-.708l6-6a.5.5 0 0 1 .708 0z"></path></svg></button>
#>     <script data-bslib-sidebar-init>bslib.Sidebar.initCollapsibleAll()</script>
#>   </div>
#> </div>
```
