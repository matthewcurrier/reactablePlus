# Validate user-supplied input values against col_spec rules

Checks every row × col_spec-column combination in `df` for:

- Numeric columns: must be non-NA, a whole number, and \>= `min`.

- Dropdown columns: must be non-NA and one of the declared choice
  values.

## Usage

``` r
validate_row_inputs(df, row_spec, col_spec)
```

## Arguments

- df:

  A tibble produced by
  [`collect_inputs()`](https://matthewcurrier.github.io/reactablePlus/reference/collect_inputs.md),
  containing the id_col and one column per col_spec entry.

- row_spec:

  A validated row_spec list (used to identify the id column).

- col_spec:

  A validated col_spec list.

## Value

A list with two fields:

- `valid`:

  Logical. `TRUE` only when no errors are found.

- `errors`:

  A list of error entries, each a list of `row_id`, `col_name`, and
  `message`.
