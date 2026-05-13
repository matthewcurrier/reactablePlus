# Collect inline Shiny input values into a tidy tibble

Reads each inline input from the Shiny `input` object using the
convention `paste0(row_id, "_", col_name)`. If an input has not been
touched by the user (i.e. it is `NULL`), the value is taken from
`fallback_df` instead. Gating is enforced server-side: if any condition
in a field's `gate` list fails for a given row, that field's value is
forced to `NA` regardless of the DOM state.

## Usage

``` r
collect_inputs(
  input,
  row_ids,
  col_spec,
  id_col,
  fallback_df = NULL,
  input_overrides = NULL
)
```

## Arguments

- input:

  The Shiny `input` object from `moduleServer`.

- row_ids:

  A vector of row identifier values (typically the id_col column of the
  display data frame).

- col_spec:

  A validated col_spec list.

- id_col:

  A single character string naming the id column.

- fallback_df:

  A tibble from
  [`merge_existing_data()`](https://matthewcurrier.github.io/reactablePlus/reference/merge_existing_data.md)
  used as a fallback when an input has not yet been set by the user.

- input_overrides:

  A `reactiveValues` of server-controlled overrides. Checked before
  `input[[local_id]]` so that reset and deselection wipes take effect in
  the same reactive flush without a browser round-trip.

## Value

A tibble with one row per element of `row_ids` and one column per
col_spec entry, plus the id column.
