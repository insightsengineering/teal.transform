# Choices selected

Construct a single list containing available choices, the default
selected value, and additional settings such as to order the choices
with the selected elements appearing first or whether to block the user
from making selections.

Can be used in UI input elements such as
[`teal.widgets::optionalSelectInput()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/optionalSelectInput.html).

## Usage

``` r
choices_selected(
  choices,
  selected = if (inherits(choices, "delayed_data")) NULL else choices[1],
  keep_order = FALSE,
  fixed = FALSE
)

is.choices_selected(x)
```

## Arguments

- choices:

  (`character`) vector of possible choices or `delayed_data` object.

  See
  [`variable_choices()`](https://insightsengineering.github.io/teal.transform/reference/variable_choices.md)
  and
  [`value_choices()`](https://insightsengineering.github.io/teal.transform/reference/value_choices.md).

- selected:

  (`character`) vector of preselected options, (`delayed_choices`)
  object or (`delayed_data`) object.

  If `delayed_data` object then `choices` must also be `delayed_data`
  object. If not supplied it will default to the first element of
  `choices` if `choices` is a vector, or `NULL` if `choices` is a
  `delayed_data` object.

- keep_order:

  (`logical`) In case of `FALSE` the selected variables will be on top
  of the drop-down field.

- fixed:

  (`logical`) optional, whether to block user to select choices.

- x:

  (`choices_selected`) object to check.

## Value

`choices_selected` returns list of `choices_selected`, encapsulating the
specified `choices`, `selected`, `keep_order` and `fixed`.

`is.choices_selected` returns `TRUE` if `x` inherits from a
`choices_selected` object, `FALSE` otherwise.

## Details

Please note that the order of selected will always follow the order of
choices. The `keep_order` argument is set to false which will run the
following code inside:

    choices <- c(selected, setdiff(choices, selected))

In case you want to keep your specific order of choices, set
`keep_order` to `TRUE`.

## Functions

- `is.choices_selected()`: Check if an object is a choices_selected
  class

## Examples

``` r
library(shiny)
library(teal.widgets)

ADSL <- teal.data::rADSL
choices_selected(variable_choices(ADSL), "SEX")
#> $choices
#> number of choices: 55 
#> 
#> SEX: Sex
#> STUDYID: Study Identifier
#> USUBJID: Unique Subject Identifier
#> SUBJID: Subject Identifier for the Study
#> SITEID: Study Site Identifier
#> AGE: Age
#> AGEU: Age Units
#> RACE: Race
#> ETHNIC: Ethnicity
#> COUNTRY: Country
#> DTHFL: Subject Death Flag
#> INVID: Investigator Identifier
#> INVNAM: Investigator Name
#> ARM: Description of Planned Arm
#> ARMCD: Planned Arm Code
#> ACTARM: Description of Actual Arm
#> ACTARMCD: Actual Arm Code
#> TRT01P: Planned Treatment for Period 01
#> TRT01A: Actual Treatment for Period 01
#> TRT02P: Planned Treatment for Period 02
#> TRT02A: Actual Treatment for Period 02
#> REGION1: Geographic Region 1
#> STRATA1: Stratification Factor 1
#> STRATA2: Stratification Factor 2
#> BMRKR1: Continuous Level Biomarker 1
#> BMRKR2: Categorical Level Biomarker 2
#> ITTFL: Intent-To-Treat Population Flag
#> SAFFL: Safety Population Flag
#> BMEASIFL: Response Evaluable Population Flag
#> BEP01FL: Biomarker Evaluable Population Flag
#> AEWITHFL: AE Leading to Drug Withdrawal Flag
#> RANDDT: Date of Randomization
#> TRTSDTM: Datetime of First Exposure to Treatment
#> TRTEDTM: Datetime of Last Exposure to Treatment
#> TRT01SDTM: Datetime of First Exposure to Treatment in Period 01
#> TRT01EDTM: Datetime of Last Exposure in Period 01
#> TRT02SDTM: Datetime of First Exposure to Treatment in Period 02
#> TRT02EDTM: Datetime of Last Exposure to Treatment in Period 02
#> AP01SDTM: Period 01 Start Datetime
#> AP01EDTM: Period 01 End Datetime
#> AP02SDTM: Period 02 Start Datetime
#> AP02EDTM: Period 02 End Datetime
#> EOSSTT: End of Study Status
#> EOTSTT: End of Treatment Status
#> EOSDT: End of Study Date
#> EOSDY: End of Study Relative Day
#> DCSREAS: Reason for Discontinuation from Study
#> DTHDT: Date of Death
#> DTHCAUS: Cause of Death
#> DTHCAT: Cause of Death Category
#> LDDTHELD: Elapsed Days from Last Dose to Death
#> LDDTHGR1: Last Dose to Death - Days Elapsed Grp 1
#> LSTALVDT: Date Last Known Alive
#> DTHADY: Relative Day of Death
#> ADTHAUT: Autopsy Performed
#> 
#> 
#> $selected
#> [1] "SEX"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"

# How to select nothing
# use an empty character
choices_selected(
  choices = c("", "A", "B", "C"),
  selected = ""
)
#> $choices
#> [1] ""  "A" "B" "C"
#> 
#> $selected
#> [1] ""
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"

# How to allow the user to select nothing
# use an empty character
choices_selected(
  choices = c("A", "", "B", "C"),
  selected = "A"
)
#> $choices
#> [1] "A" ""  "B" "C"
#> 
#> $selected
#> [1] "A"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"


# How to make Nothing the Xth choice
# just use keep_order
choices_selected(
  choices = c("A", "", "B", "C"),
  selected = "A",
  keep_order = TRUE
)
#> $choices
#> [1] "A" ""  "B" "C"
#> 
#> $selected
#> [1] "A"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"


# How to give labels to selections
# by adding names - choices will be replaced by "name" in UI, not in code
choices_selected(
  choices = c("name for A" = "A", "Name for nothing" = "", "name for b" = "B", "name for C" = "C"),
  selected = "A"
)
#> $choices
#>       name for A Name for nothing       name for b       name for C 
#>              "A"               ""              "B"              "C" 
#> 
#> $selected
#> [1] "A"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"

# by using choices_labeled
# labels will be shown behind the choice
choices_selected(
  choices = choices_labeled(
    c("A", "", "B", "C"),
    c("name for A", "nothing", "name for B", "name for C")
  ),
  selected = "A"
)
#> $choices
#> number of choices: 4 
#> 
#> A: name for A
#> : nothing
#> B: name for B
#> C: name for C
#> 
#> 
#> $selected
#> [1] "A"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"

# Passing a `delayed_data` object to `selected`
choices_selected(
  choices = variable_choices("ADSL"),
  selected = variable_choices("ADSL", subset = c("STUDYID"))
)
#> choices_selected with delayed data:  ADSL
#> $ choices
#>   variable_choices with delayed data: ADSL
#>   $ data
#>   [1] "ADSL"
#>   $ subset
#>   NULL
#>   $ key
#>   NULL
#> $ selected
#>   variable_choices with delayed data: ADSL
#>   $ data
#>   [1] "ADSL"
#>   $ subset
#>   [1] "STUDYID"
#>   $ key
#>   NULL
#> $ keep_order
#> [1] FALSE
#> $ fixed
#> [1] FALSE

# Passing `delayed_choices` object - semantically identical objects:
choices_selected(choices = letters, selected = letters)
#> $choices
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z"
#> 
#> $selected
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"
choices_selected(choices = letters, selected = all_choices())
#> $choices
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z"
#> 
#> $selected
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"

choices_selected(
  choices = setNames(LETTERS[1:5], paste("Letter", LETTERS[1:5])),
  selected = "E"
)
#> $choices
#> Letter E Letter A Letter B Letter C Letter D 
#>      "E"      "A"      "B"      "C"      "D" 
#> 
#> $selected
#> [1] "E"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"
choices_selected(
  choices = setNames(LETTERS[1:5], paste("Letter", LETTERS[1:5])),
  selected = last_choice()
)
#> $choices
#> Letter E Letter A Letter B Letter C Letter D 
#>      "E"      "A"      "B"      "C"      "D" 
#> 
#> $selected
#> Letter E 
#>      "E" 
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"

# functional form (subsetting for factor variables only) of choices_selected
# with delayed data loading
choices_selected(variable_choices("ADSL", subset = function(data) {
  idx <- vapply(data, is.factor, logical(1))
  names(data)[idx]
}))
#> choices_selected with delayed data:  ADSL
#> $ choices
#>   variable_choices with delayed data: ADSL
#>   $ data
#>   [1] "ADSL"
#>   $ subset
#>   function (data) 
#> {
#>     idx <- vapply(data, is.factor, logical(1))
#>     names(data)[idx]
#> }
#> <environment: 0x561de1623a80>
#>   $ key
#>   NULL
#> $ selected
#> NULL
#> $ keep_order
#> [1] FALSE
#> $ fixed
#> [1] FALSE

cs <- choices_selected(
  choices = c("A", "B", "C"),
  selected = "A"
)

ui <- bslib::page_fluid(
  optionalSelectInput(
    inputId = "id",
    choices = cs$choices,
    selected = cs$selected
  )
)

server <- function(input, output, session) {}
if (interactive()) {
  shinyApp(ui, server)
}
```
