# R6 Class - A First-In-First-Out Abstract Data Type

Abstract data type that stores and returns any number of elements.

## Details

A `Queue` object stores all elements in a single vector, thus all data
types can be stored, but silent coercion may occur.

Elements are returned in the same order that they were added.

## Methods

### Public methods

- [`Queue$push()`](#method-Queue-push)

- [`Queue$get()`](#method-Queue-get)

- [`Queue$pop()`](#method-Queue-pop)

- [`Queue$remove()`](#method-Queue-remove)

- [`Queue$empty()`](#method-Queue-empty)

- [`Queue$size()`](#method-Queue-size)

- [`Queue$print()`](#method-Queue-print)

- [`Queue$clone()`](#method-Queue-clone)

------------------------------------------------------------------------

### Method `push()`

Adds element(s) to `Queue`.

#### Usage

    Queue$push(new_elements)

#### Arguments

- `new_elements`:

  vector of elements to add.

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Returns all contents of the `Queue` object.

#### Usage

    Queue$get()

#### Returns

Single vector containing all `Queue` contents.

------------------------------------------------------------------------

### Method `pop()`

Returns the first (oldest) element of the `Queue` and removes it.

#### Usage

    Queue$pop()

#### Returns

vector of length 1 containing the first element of `Queue` or `NULL` if
`Queue` is empty.

------------------------------------------------------------------------

### Method [`remove()`](https://rdrr.io/r/base/rm.html)

Removes the oldest occurrence of specified element(s) from `Queue`.
Relies on implicit type conversions of R identify elements to remove.

#### Usage

    Queue$remove(elements)

#### Arguments

- `elements`:

  vector of elements to remove from `Queue`.

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method `empty()`

Removes all elements from `Queue`.

#### Usage

    Queue$empty()

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method `size()`

Returns the number of elements in `Queue`.

#### Usage

    Queue$size()

#### Returns

`integer(1)`.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints this `Queue`.

#### Usage

    Queue$print(...)

#### Arguments

- `...`:

  Additional arguments to this method, ignored.

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Queue$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
