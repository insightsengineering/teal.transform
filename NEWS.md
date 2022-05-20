# teal.transform 0.1.0.9011

### New features
* Added a formatting function for the output of `data_extract_srv` - `format_data_extract`.

### Breaking changes
* Removed the (previously deprecated) `input_id` argument to `data_merge_module`.
* All `selected` values must be valid `choices` when calling `choices_selected`. When using delayed resolving the invalid selected are removed and a warning is thrown to the logs, in other cases an error is thrown.

### Miscellaneous
* Added a template to the `pkgdown` site.
* Added names to the `id` parameter of `chunks$push`.


# teal.transform 0.1.0

## Changes (from behavior when functionality was part of `teal`)

### New features
* Added new argument `ordered` for `select_spec()` to flag whether order of the selection should be tracked.

### Misc
* The `get_relabel_call` function now returns `formatters::var_relabel` instead of `teal.data::var_relabel`.
