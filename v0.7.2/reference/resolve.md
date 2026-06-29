# Resolve delayed inputs by evaluating the code within the provided datasets

Resolve delayed inputs by evaluating the code within the provided
datasets

## Usage

``` r
resolve(x, datasets, keys = NULL)

# S3 method for class 'delayed_variable_choices'
resolve(x, datasets, keys)

# S3 method for class 'delayed_value_choices'
resolve(x, datasets, keys)

# S3 method for class 'delayed_choices_selected'
resolve(x, datasets, keys)

# S3 method for class 'delayed_select_spec'
resolve(x, datasets, keys)

# S3 method for class 'delayed_filter_spec'
resolve(x, datasets, keys)

# S3 method for class 'delayed_data_extract_spec'
resolve(x, datasets, keys)

# S3 method for class 'list'
resolve(x, datasets, keys)

# Default S3 method
resolve(x, datasets, keys)
```

## Arguments

- x:

  (`delayed_data`) object to resolve.

- datasets:

  (named `list` of `data.frame`) to use in evaluation.

- keys:

  (named `list` of `character`) to be used as the keys for each dataset.
  The names of this list must be exactly the same as for datasets.

## Value

Resolved object.

## Methods (by class)

- `resolve(delayed_variable_choices)`: Call
  [`variable_choices()`](https://insightsengineering.github.io/teal.transform/reference/variable_choices.md)
  on the delayed `variable_choices` object.

- `resolve(delayed_value_choices)`: Call
  [`value_choices()`](https://insightsengineering.github.io/teal.transform/reference/value_choices.md)
  on the delayed `value_choices` object.

- `resolve(delayed_choices_selected)`: Call
  [`select_spec()`](https://insightsengineering.github.io/teal.transform/reference/select_spec.md)
  on the delayed `choices_selected` object.

- `resolve(delayed_select_spec)`: Call
  [`select_spec()`](https://insightsengineering.github.io/teal.transform/reference/select_spec.md)
  on the delayed specification.

- `resolve(delayed_filter_spec)`: Call
  [`filter_spec()`](https://insightsengineering.github.io/teal.transform/reference/filter_spec.md)
  on the delayed specification.

- `resolve(delayed_data_extract_spec)`: Call
  [`data_extract_spec()`](https://insightsengineering.github.io/teal.transform/reference/data_extract_spec.md)
  on the delayed specification.

- `resolve(list)`: Iterates over elements of the list and recursively
  calls `resolve`.

- `resolve(default)`: Default method that does nothing and returns `x`
  itself.

## Note

This is an internal function that is used by
[`resolve_delayed()`](https://insightsengineering.github.io/teal.transform/reference/resolve_delayed.md).
All the methods are used internally only.
