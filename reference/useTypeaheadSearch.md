# Wire up server-side typeahead search

Call this once in your Shiny server function (or module server). It
observes the shared `school_search` input sent by all search pickers and
routes results back to the correct widget.

`useSchoolSearch()` is a deprecated alias for `useTypeaheadSearch()`. It
will be removed in a future release.

## Usage

``` r
useTypeaheadSearch(input, session, search_fn, limit = 25L)

useSchoolSearch(...)
```

## Arguments

- input:

  The Shiny `input` object.

- session:

  The Shiny `session` object.

- search_fn:

  A function that takes `(query, limit)` and returns a data frame of
  matching results.

- limit:

  Maximum number of results to return. Default 25.

- ...:

  Arguments passed to `useTypeaheadSearch()`.

## Value

Invisible `NULL`. Called for its side effect.
