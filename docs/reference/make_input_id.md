# Generate a stable, unique input element ID

Combines a namespace prefix, a row identifier, and a column name into a
single dash-separated string suitable for use as an HTML element ID or
for CSS targeting.

## Usage

``` r
make_input_id(ns_prefix, row_id, col_name)
```

## Arguments

- ns_prefix:

  A single non-empty character string — typically the Shiny module
  namespace.

- row_id:

  A scalar (integer or character) uniquely identifying the row. Must not
  be `NA`.

- col_name:

  A single non-empty character string matching a `col_name` field in a
  col_spec entry.

## Value

A single character string of the form
`"<ns_prefix>-<row_id>-<col_name>"`.
