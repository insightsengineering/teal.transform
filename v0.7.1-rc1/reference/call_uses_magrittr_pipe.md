# Check if a call or list of calls uses the pipe operator (%\>%)

Recursively searches through a call or list of calls to determine if the
pipe operator `%>%` is used anywhere.

## Usage

``` r
call_uses_magrittr_pipe(x)
```

## Arguments

- x:

  (`call`, `name`, or `list` of calls) The call(s) to check.

## Value

`logical(1)` `TRUE` if `%>%` is found, `FALSE` otherwise.
