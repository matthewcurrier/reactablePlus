# Wire up server-side school search

Call this once in your Shiny server function (or module server). It
observes the shared `school_search` input sent by all school pickers and
routes results back to the correct widget.

## Usage

``` r
useSchoolSearch(input, session, search_fn, limit = 25L)
```

## Arguments

- input:

  The Shiny `input` object.

- session:

  The Shiny `session` object.

- search_fn:

  A function that takes `(query, limit)` and returns a data frame of
  matching schools.

- limit:

  Maximum number of results to return. Default 25.

## Value

Invisible `NULL`. Called for its side effect.
