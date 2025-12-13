# Package index

## Data extract constructors

Functions used to create transform instructions for `teal` applications.

- [`add_no_selected_choices()`](https://insightsengineering.github.io/teal.transform/reference/add_no_selected_choices.md)
  : Add empty choice to choices selected

- [`all_choices()`](https://insightsengineering.github.io/teal.transform/reference/delayed_choices.md)
  [`first_choice()`](https://insightsengineering.github.io/teal.transform/reference/delayed_choices.md)
  [`last_choice()`](https://insightsengineering.github.io/teal.transform/reference/delayed_choices.md)
  [`first_choices()`](https://insightsengineering.github.io/teal.transform/reference/delayed_choices.md)
  [`last_choices()`](https://insightsengineering.github.io/teal.transform/reference/delayed_choices.md)
  :

  Bare constructor for `delayed_choices` object

- [`check_no_multiple_selection()`](https://insightsengineering.github.io/teal.transform/reference/check_no_multiple_selection.md)
  :

  Checks that the `extract_input` specification does not allow multiple
  selection

- [`choices_labeled()`](https://insightsengineering.github.io/teal.transform/reference/choices_labeled.md)
  [`print(`*`<choices_labeled>`*`)`](https://insightsengineering.github.io/teal.transform/reference/choices_labeled.md)
  :

  Set "`<choice>:<label>`" type of names

- [`choices_selected()`](https://insightsengineering.github.io/teal.transform/reference/choices_selected.md)
  [`is.choices_selected()`](https://insightsengineering.github.io/teal.transform/reference/choices_selected.md)
  : Choices selected

- [`data_extract_spec()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_spec.md)
  :

  Data extract input for `teal` modules

- [`filter_spec()`](https://insightsengineering.github.io/teal.transform/reference/filter_spec.md)
  : Data extract filter specification

- [`get_extract_datanames()`](https://insightsengineering.github.io/teal.transform/reference/get_extract_datanames.md)
  :

  Gets names of the datasets from a list of `data_extract_spec` objects

- [`no_selected_as_NULL()`](https://insightsengineering.github.io/teal.transform/reference/no_selected_as_NULL.md)
  : Check select choices for no choice made

- [`select_spec()`](https://insightsengineering.github.io/teal.transform/reference/select_spec.md)
  [`select_spec.delayed_data()`](https://insightsengineering.github.io/teal.transform/reference/select_spec.md)
  [`select_spec.default()`](https://insightsengineering.github.io/teal.transform/reference/select_spec.md)
  : Column selection input specification

- [`split_by_sep()`](https://insightsengineering.github.io/teal.transform/reference/split_by_sep.md)
  : Split by separator (matched exactly)

- [`value_choices()`](https://insightsengineering.github.io/teal.transform/reference/value_choices.md)
  : Value labeling and filtering based on variable relationship

- [`variable_choices()`](https://insightsengineering.github.io/teal.transform/reference/variable_choices.md)
  : Variable label extraction and custom selection from data

- [`resolve()`](https://insightsengineering.github.io/teal.transform/reference/resolve.md)
  : Resolve delayed inputs by evaluating the code within the provided
  datasets

- [`resolve_delayed()`](https://insightsengineering.github.io/teal.transform/reference/resolve_delayed.md)
  : Resolve delayed inputs by evaluating the code within the provided
  datasets

## Data extract modules

`teal` modules to generate transform expressions

- [`data_extract_multiple_srv()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_multiple_srv.md)
  :

  Creates a named list of `data_extract_srv` output

- [`data_extract_ui()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_ui.md)
  :

  `teal` data extraction module user-interface

- [`data_extract_srv()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_srv.md)
  : Extraction of the selector(s) details

- [`datanames_input()`](https://insightsengineering.github.io/teal.transform/reference/datanames_input.md)
  : Help text with available datasets input

- [`merge_expression_module()`](https://insightsengineering.github.io/teal.transform/reference/merge_expression_module.md)
  : Merge expression module

- [`merge_expression_srv()`](https://insightsengineering.github.io/teal.transform/reference/merge_expression_srv.md)
  : Data merge module server

- [`get_dataset_prefixed_col_names()`](https://insightsengineering.github.io/teal.transform/reference/get_dataset_prefixed_col_names.md)
  : Returns non-key column names from data

- [`get_anl_relabel_call()`](https://insightsengineering.github.io/teal.transform/reference/get_anl_relabel_call.md)
  : Gets the relabel call

- [`get_merge_call()`](https://insightsengineering.github.io/teal.transform/reference/get_merge_call.md)
  : Get merge call from a list of selectors

- [`get_relabel_call()`](https://insightsengineering.github.io/teal.transform/reference/get_relabel_call.md)
  : Create relabel call from named character

- [`is_single_dataset()`](https://insightsengineering.github.io/teal.transform/reference/is_single_dataset.md)
  : Verify uniform dataset source across data extract specification

- [`list_extract_spec()`](https://insightsengineering.github.io/teal.transform/reference/list_extract_spec.md)
  : Make sure that the extract specification is in list format

- [`merge_datasets()`](https://insightsengineering.github.io/teal.transform/reference/merge_datasets.md)
  : Merge the datasets on the keys

- [`compose_and_enable_validators()`](https://insightsengineering.github.io/teal.transform/reference/compose_and_enable_validators.md)
  :

  Function to compose `validators` from `data_extract_multiple_srv`

## Human-readable formatting of a data extract object

- [`format_data_extract()`](https://insightsengineering.github.io/teal.transform/reference/format_data_extract.md)
  : Formatting data extracts
