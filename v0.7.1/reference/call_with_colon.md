# Create a call using a function in a given namespace

The dot arguments in `...` need to be quoted because they will be
evaluated otherwise.

## Usage

``` r
call_with_colon(name, ..., unlist_args = list())
```

## Arguments

- name:

  `character` function name, possibly using namespace colon `::`, also
  works with `:::` (sometimes needed, but strongly discouraged).

- ...:

  arguments to pass to function with name `name`.

- unlist_args:

  `list` extra arguments passed in a single list, avoids the use of
  `do.call` with this function.

## Value

`call`.
