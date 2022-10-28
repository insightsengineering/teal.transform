# teal.transform 0.2.0.9001

* Examples use `scda.2022` instead of `scda.2021`

### Enhancements

* Updated error messages for `choices_labeled()`, `variable_choices()` and `value_choices()` to be more informative.

# teal.transform 0.2.0

### Breaking changes

* Updated the data merge functionality to no longer expect input datanames with the suffix `_FILTERED` following changes to the output of `teal.slice::FilteredData$get_call()`.
* Updated the data merge downstream functions to accept a `joinKeys` object for the `join_keys` argument.

### New features

* `data_extract_ui`, `data_extract_srv`, `data_extract_multiple_srv` can be initialized by a list of (optionally `reactive`) `data.frame` objects.
* Added new modules `merge_expression_srv` and `merge_expression_module`, updates of `data_merge_srv` and `data_merge_module` 
(which will be deprecated in future releases) respectively, where `datasets` argument takes a list of (optionally `reactive`) `data.frame` objects and a new argument `join_keys` that accepts a `joinKeys` object.

### Enhancements
* Updated the examples and the tests to use `teal.slice::init_filtered_data` to initialize a `FilteredData` object.
* Updated the vignettes and the README content.
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
