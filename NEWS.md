# teal.transform 0.1.1.9013

### Breaking changes

* Following changes to the output of `teal.slice::FilteredData$get_call()` the data merge functionality no longer expects input datanames to have the suffix `_FILTERED`.

### New features

* `data_extract_ui`, `data_extract_srv`, `data_extract_multiple_srv` can be initialized by the list of (optionally `reactive`) `data.frame` objects.
* Added new modules `merge_expression_srv` and `merge_expression_module`, updates of `data_merge_srv` and `data_merge_module` 
(which will be deprecated in future releases) respectively, where `datasets` argument takes a list of (optionally `reactive`) `data.frame` objects and a new argument `join_keys`.

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
