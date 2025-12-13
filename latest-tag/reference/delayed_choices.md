# Bare constructor for `delayed_choices` object

Special S3 structures that delay selection of possible choices in a
`filter_spec`, `select_spec` or `choices_selected` object.

## Usage

``` r
all_choices()

first_choice()

last_choice()

first_choices(n)

last_choices(n)
```

## Arguments

- n:

  positive (`integer`-like) number of first/last items to subset to

## Value

Object of class `delayed_data, delayed_choices`, which is a function
that returns the appropriate subset of its argument. `all_choices`,
`first_choices`, and `last_choices` structures also have an additional
class for internal use.

## Examples

``` r
# These pairs of structures represent semantically identical specifications:
choices_selected(choices = letters, selected = letters)
#> $choices
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z"
#> 
#> $selected
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"
choices_selected(choices = letters, selected = all_choices())
#> $choices
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z"
#> 
#> $selected
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"

choices_selected(choices = letters, selected = letters[1])
#> $choices
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z"
#> 
#> $selected
#> [1] "a"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"
choices_selected(choices = letters, selected = first_choice())
#> $choices
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z"
#> 
#> $selected
#> [1] "a"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"

choices_selected(choices = letters, selected = letters[length(letters)])
#> $choices
#>  [1] "z" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r"
#> [20] "s" "t" "u" "v" "w" "x" "y"
#> 
#> $selected
#> [1] "z"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"
choices_selected(choices = letters, selected = last_choice())
#> $choices
#>  [1] "z" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r"
#> [20] "s" "t" "u" "v" "w" "x" "y"
#> 
#> $selected
#> [1] "z"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"

choices_selected(choices = letters, selected = head(letters, 4))
#> $choices
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z"
#> 
#> $selected
#> [1] "a" "b" "c" "d"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"
choices_selected(choices = letters, selected = first_choices(4))
#> $choices
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z"
#> 
#> $selected
#> [1] "a" "b" "c" "d"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"

choices_selected(choices = letters, selected = tail(letters, 4))
#> $choices
#>  [1] "w" "x" "y" "z" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
#> [20] "p" "q" "r" "s" "t" "u" "v"
#> 
#> $selected
#> [1] "w" "x" "y" "z"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"
choices_selected(choices = letters, selected = last_choices(4))
#> $choices
#>  [1] "w" "x" "y" "z" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
#> [20] "p" "q" "r" "s" "t" "u" "v"
#> 
#> $selected
#> [1] "w" "x" "y" "z"
#> 
#> $fixed
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "choices_selected"

filter_spec(
  vars = c("selected_variable"),
  choices = c("value1", "value2", "value3"),
  selected = "value3"
)
#> filter_spec with delayed data:
#> $ vars_choices
#> [1] "selected_variable"
#> $ vars_selected
#> [1] "selected_variable"
#> $ vars_label
#> NULL
#> $ vars_fixed
#> [1] TRUE
#> $ vars_multiple
#> [1] TRUE
#> $ choices
#> [1] "value1" "value2" "value3"
#> $ selected
#> [1] "value3"
#> $ label
#> [1] "Filter by"
#> $ multiple
#> [1] FALSE
#> $ fixed
#> [1] FALSE
#> $ sep
#> [1] " - "
#> $ drop_keys
#> [1] FALSE
#> $ dataname
#> NULL
#> $ initialized
#> [1] FALSE
filter_spec(
  vars = c("selected_variable"),
  choices = c("value1", "value2", "value3"),
  selected = last_choice()
)
#> filter_spec with delayed data:
#> $ vars_choices
#> [1] "selected_variable"
#> $ vars_selected
#> [1] "selected_variable"
#> $ vars_label
#> NULL
#> $ vars_fixed
#> [1] TRUE
#> $ vars_multiple
#> [1] TRUE
#> $ choices
#> [1] "value1" "value2" "value3"
#> $ selected
#> [1] "value3"
#> $ label
#> [1] "Filter by"
#> $ multiple
#> [1] FALSE
#> $ fixed
#> [1] FALSE
#> $ sep
#> [1] " - "
#> $ drop_keys
#> [1] FALSE
#> $ dataname
#> NULL
#> $ initialized
#> [1] FALSE
```
