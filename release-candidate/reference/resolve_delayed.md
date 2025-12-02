# Resolve delayed inputs by evaluating the code within the provided datasets

Resolve delayed inputs by evaluating the code within the provided
datasets

## Usage

``` r
resolve_delayed(x, datasets, keys)

# S3 method for class 'FilteredData'
resolve_delayed(
  x,
  datasets,
  keys = sapply(datasets$datanames(), datasets$get_keys, simplify = FALSE)
)

# S3 method for class 'list'
resolve_delayed(x, datasets, keys = NULL)
```

## Arguments

- x:

  (`delayed_data`, `list`) to resolve.

- datasets:

  (`FilteredData` or named `list`) to use as a reference to resolve `x`.

- keys:

  (named `list`) with primary keys for each dataset from `datasets`.
  `names(keys)` should match `names(datasets)`.

## Value

Resolved object.

## Methods (by class)

- `resolve_delayed(FilteredData)`: Default values for `keys` parameters
  is extracted from `datasets`.

- `resolve_delayed(list)`: Generic method when `datasets` argument is a
  named list.

## Examples

``` r
library(shiny)

ADSL <- teal.data::rADSL
isolate({
  data_list <- list(ADSL = reactive(ADSL))

  # value_choices example
  v1 <- value_choices("ADSL", "SEX", "SEX")
  v1
  resolve_delayed(v1, data_list)

  # variable_choices example
  v2 <- variable_choices("ADSL", c("BMRKR1", "BMRKR2"))
  v2
  resolve_delayed(v2, data_list)

  # data_extract_spec example
  adsl_filter <- filter_spec(
    vars = variable_choices("ADSL", "SEX"),
    sep = "-",
    choices = value_choices("ADSL", "SEX", "SEX"),
    selected = "F",
    multiple = FALSE,
    label = "Choose endpoint and Censor"
  )

  adsl_select <- select_spec(
    label = "Select variable:",
    choices = variable_choices("ADSL", c("BMRKR1", "BMRKR2")),
    selected = "BMRKR1",
    multiple = FALSE,
    fixed = FALSE
  )

  adsl_de <- data_extract_spec(
    dataname = "ADSL",
    select = adsl_select,
    filter = adsl_filter
  )

  resolve_delayed(adsl_filter, datasets = data_list)
  resolve_delayed(adsl_select, datasets = data_list)
  resolve_delayed(adsl_de, datasets = data_list)

  # nested list (arm_ref_comp)
  arm_ref_comp <- list(
    ARMCD = list(
      ref = variable_choices("ADSL"),
      comp = variable_choices("ADSL")
    )
  )

  resolve_delayed(arm_ref_comp, datasets = data_list)
})
#> $ARMCD
#> $ARMCD$ref
#> number of choices: 55 
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
#> 
#> 
#> $ARMCD$comp
#> number of choices: 55 
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
#> 
#> 
#> 
```
