# Variable label extraction and custom selection from data

Wrapper on
[choices_labeled](https://insightsengineering.github.io/teal.transform/reference/choices_labeled.md)
to label variables basing on existing labels in data.

## Usage

``` r
variable_choices(data, subset = NULL, fill = FALSE, key = NULL)

# S3 method for class 'character'
variable_choices(data, subset = NULL, fill = FALSE, key = NULL)

# S3 method for class 'data.frame'
variable_choices(data, subset = NULL, fill = TRUE, key = NULL)
```

## Arguments

- data:

  (`data.frame` or `character`) If `data.frame`, then data to extract
  labels from. If `character`, then name of the dataset to extract data
  from once available.

- subset:

  (`character` or `function`) If `character`, then a vector of column
  names. If `function`, then this function is used to determine the
  possible columns (e.g. all factor columns). In this case, the function
  must take only single argument "data" and return a character vector.

  See examples for more details.

- fill:

  (`logical(1)`) if `TRUE`, the function will return variable names for
  columns with non-existent labels; otherwise will return `NA` for them.

- key:

  (`character`) vector with names of the variables, which are part of
  the primary key of the `data` argument.

  This is an optional argument, which allows to identify variables
  associated with the primary key and display the appropriate icon for
  them in the
  [`teal.widgets::optionalSelectInput()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/optionalSelectInput.html)
  widget.

## Value

Named `character` vector with additional attributes or `delayed_data`
object.

## Examples

``` r
library(teal.data)
ADRS <- rADRS
variable_choices(ADRS)
#> number of choices: 65 
#> 
#> STUDYID: Study Identifier
#> USUBJID: Unique Subject Identifier
#> SUBJID: Subject Identifier for the Study
#> SITEID: Study Site Identifier
#> AGE: Age
#> AGEU: Age Units
#> SEX: Sex
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
#> ASEQ: Analysis Sequence Number
#> RSSEQ: Sequence Number
#> PARAM: Parameter
#> PARAMCD: Parameter Code
#> AVAL: Analysis Value
#> AVALC: Analysis Value (C)
#> ADTM: Analysis Datetime
#> ADY: Analysis Relative Day
#> AVISIT: Analysis Visit
#> AVISITN: Analysis Visit (N)
#> 
variable_choices(ADRS, subset = c("PARAM", "PARAMCD"))
#> number of choices: 2 
#> 
#> PARAM: Parameter
#> PARAMCD: Parameter Code
#> 
variable_choices(ADRS, subset = c("", "PARAM", "PARAMCD"))
#> number of choices: 3 
#> 
#> : 
#> PARAM: Parameter
#> PARAMCD: Parameter Code
#> 
variable_choices(
  ADRS,
  subset = c("", "PARAM", "PARAMCD"),
  key = default_cdisc_join_keys["ADRS", "ADRS"]
)
#> number of choices: 3 
#> 
#> : 
#> PARAM: Parameter
#> PARAMCD: Parameter Code
#> 

# delayed version
variable_choices("ADRS", subset = c("USUBJID", "STUDYID"))
#> variable_choices with delayed data: ADRS
#> $ data
#> [1] "ADRS"
#> $ subset
#> [1] "USUBJID" "STUDYID"
#> $ key
#> NULL

# functional subset (with delayed data) - return only factor variables
variable_choices("ADRS", subset = function(data) {
  idx <- vapply(data, is.factor, logical(1))
  names(data)[idx]
})
#> variable_choices with delayed data: ADRS
#> $ data
#> [1] "ADRS"
#> $ subset
#> function (data) 
#> {
#>     idx <- vapply(data, is.factor, logical(1))
#>     names(data)[idx]
#> }
#> <environment: 0x561de5c10eb0>
#> $ key
#> NULL
```
