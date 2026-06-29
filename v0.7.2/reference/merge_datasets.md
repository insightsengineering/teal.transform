# Merge the datasets on the keys

Combines/merges multiple datasets with specified keys attribute.

## Usage

``` r
merge_datasets(
  selector_list,
  datasets,
  join_keys,
  merge_function = "dplyr::full_join",
  anl_name = "ANL"
)
```

## Arguments

- selector_list:

  (`reactive`) output from
  [`data_extract_multiple_srv()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_multiple_srv.md)
  or a reactive named list of outputs from
  [`data_extract_srv()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_srv.md).
  When using a reactive named list, the names must be identical to the
  shiny ids of the respective
  [`data_extract_ui()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_ui.md).

- datasets:

  (named `list` of `reactive` or non-`reactive` `data.frame`) object
  containing data as a list of `data.frame`. When passing a list of
  non-reactive `data.frame` objects, they are converted to reactive
  `data.frame` objects internally.

- join_keys:

  (`join_keys`) of variables used as join keys for each of the datasets
  in `datasets`. This will be used to extract the `keys` of every
  dataset.

- merge_function:

  (`character(1)` or `reactive`) A character string of a function that
  accepts the arguments `x`, `y` and `by` to perform the merging of
  datasets.

- anl_name:

  (`character(1)`) Name of the analysis dataset.

## Value

`merged_dataset` list containing:

- `expr` (`list` of `call`) code needed to replicate merged dataset;

- `columns_source` (`list`) of column names selected for particular
  selector; Each list element contains named character vector where:

  - Values are the names of the columns in the `ANL`. In case if the
    same column name is selected in more than one selector it gets
    prefixed by the id of the selector. For example if two
    `data_extract` have id `x`, `y`, then their duplicated selected
    variable (for example `AGE`) is prefixed to be `x.AGE` and `y.AGE`;

  - Names of the vector denote names of the variables in the input
    dataset;

  - `attr(,"dataname")` to indicate which dataset variable is merged
    from;

  - `attr(, "always selected")` to denote the names of the variables
    which need to be always selected;

- `keys` (`list`) the keys of the merged dataset;

- `filter_info` (`list`) The information given by the user. This
  information defines the filters that are applied on the data.
  Additionally it defines the variables that are selected from the data
  sets.

## Details

Internally this function uses calls to allow reproducibility.

This function is often used inside a `teal` module server function with
the `selectors` being the output of `data_extract_srv` or
`data_extract_multiple_srv`.

    # inside teal module server function

    response <- data_extract_srv(
      id = "reponse",
      data_extract_spec = response_spec,
      datasets = datasets
    )
    regressor <- data_extract_srv(
      id = "regressor",
      data_extract_spec = regressor_spec,
      datasets = datasets
    )
    merged_data <- merge_datasets(list(regressor(), response()))

## Examples

``` r
library(shiny)
library(teal.data)

X <- data.frame(A = c(1, 1:3), B = 2:5, D = 1:4, E = letters[1:4], G = letters[6:9])
Y <- data.frame(A = c(1, 1, 2), B = 2:4, C = c(4, 4:5), E = letters[4:6], G = letters[1:3])
join_keys <- join_keys(join_key("X", "Y", c("A", "B")))

selector_list <- list(
  list(
    dataname = "X",
    filters = NULL,
    select = "E",
    keys = c("A", "B"),
    reshape = FALSE,
    internal_id = "x"
  ),
  list(
    dataname = "Y",
    filters = NULL,
    select = "G",
    keys = c("A", "C"),
    reshape = FALSE,
    internal_id = "y"
  )
)

data_list <- list(X = reactive(X), Y = reactive(Y))

merged_datasets <- isolate(
  merge_datasets(
    selector_list = selector_list,
    datasets = data_list,
    join_keys = join_keys
  )
)

paste(merged_datasets$expr)
#> [1] "library(magrittr)"                                        
#> [2] "ANL_1 <- X %>% dplyr::select(A, B, E)"                    
#> [3] "ANL_2 <- Y %>% dplyr::select(A, B, G)"                    
#> [4] "ANL <- ANL_1"                                             
#> [5] "ANL <- dplyr::full_join(ANL, ANL_2, by = c(\"A\", \"B\"))"
```
