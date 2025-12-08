# teal.transform 0.7.1

### Miscellaneous

* Fixed the pipe behavior by adding `magrittr` library call when the generated expression is using `%>%` operator.

# teal.transform 0.7.0

### Enhancements

* Update all UI components to use `bslib` (#250).

# teal.transform 0.6.0

### Enhancements

* Added utility functions `first_choice`, `last_choice`, `first_choices`, and `last_choices` to increase the repertoire of specifying choices in delayed data, previously only served by `all_choices`.
* Allowed `value_choices` to use `delayed_variable_choices` objects for `var_choices`.
It is now possible to define a `data_extract_spec` without naming any variables.
* Replace the example data generated using `scda` with `random.cdisc.data`.
* Change log level from trace to debug for several functions.

# teal.transform 0.5.0

### Breaking changes

* Namespace from `magrittr` package is no longer available when loading `teal.transform`. Only re-exports pipe `%>%` operator from `dplyr` package.

### Miscellaneous

* Removed `magrittr` from package dependencies.
* Specified minimal version of package dependencies.

# teal.transform 0.4.0

### Miscellaneous

* Removed `scda` package dependency from examples.
* Deprecated `data_merge_module()` and `data_merge_srv()`.
* The `get_relabel_call` function now returns a call to `teal.data::col_relabel` instead of one to `formatters::var_relabel`.
* Update installation instruction

# teal.transform 0.3.0

### New Features

* Added `Queue` class with updated unit tests.

### Enhancements

* Updated error messages for `choices_labeled()`, `variable_choices()` and `value_choices()` to be more informative.
* Updated `data_extract` to be compatible with `shinyvalidate`: `data_extract_srv` and `data_extract_multiple_srv` accept `shinyvalidate` validation rules and their return objects now include `shinyvalidate::InputValidator`(s).
* Added helper function `compose_and_enable_validators` to simplify enabling `shinyvalidate::InputValidator`s returned from `data_extract_multiple_srv`.

### Bug fixes

* Fixed a bug in `data_extract_filter_srv` when app developer sets a selected value.

### Miscellaneous

* Examples use `scda.2022` instead of `scda.2021`

# teal.transform 0.2.0

### Breaking changes

* Updated the data merge functionality to no longer expect input `datanames` with the suffix `_FILTERED` following changes to the output of `teal.slice::FilteredData$get_call()`.
* Updated the data merge downstream functions to accept a `joinKeys` object for the `join_keys` argument.

### New features

* `data_extract_ui`, `data_extract_srv`, `data_extract_multiple_srv` can be initialized by a list of (optionally `reactive`) `data.frame` objects.
* Added new modules `merge_expression_srv` and `merge_expression_module`, updates of `data_merge_srv` and `data_merge_module`
(which will be deprecated in future releases) respectively, where `datasets` argument takes a list of (optionally `reactive`) `data.frame` objects and a new argument `join_keys` that accepts a `joinKeys` object.

### Enhancements
* Updated the examples and the tests to use `teal.slice::init_filtered_data` to initialize a `FilteredData` object.
* Updated the vignettes and the `README` content.
* Updated `data_merge_module` and `merge_expression_module` to accept `NULL` `data_extract` inputs.

# teal.transform 0.1.1

### New features
* Added a formatting function `format_data_extract` for the output of `data_extract_srv`.

### Breaking changes
* Removed the (previously deprecated) `input_id` argument to `data_merge_module`.
* Updated `choices_selected` so that all `selected` values must be valid `choices`. When using delayed resolving the invalid selected are removed and a warning is thrown to the logs, in other cases an error is thrown.

### Miscellaneous
* Added a template to the `pkgdown` site.
* Added names to the `id` parameter of `chunks$push` calls.
* Updated package authors.
* Added vignettes for extracting and merging data.

# teal.transform 0.1.0

## Changes (from behavior when functionality was part of `teal`)

### New features
* Added new argument `ordered` for `select_spec()` to flag whether order of the selection should be tracked.

### Miscellaneous
* The `get_relabel_call` function now returns `formatters::var_relabel` instead of `teal.data::var_relabel`.
