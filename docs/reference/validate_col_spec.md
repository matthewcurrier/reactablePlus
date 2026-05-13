# Validate a col_spec list

Checks that a col_spec is well-formed before any rendering or data
manipulation occurs. Throws an informative error on the first problem
found.

## Usage

``` r
validate_col_spec(col_spec, row_spec = NULL)
```

## Arguments

- col_spec:

  A non-empty list of column definition lists. Each entry must contain
  `col_name` (string), `label` (string), and `type` (one of `"dropdown"`
  or `"numeric"`). Dropdown entries also require `choices` (a list of
  lists, each with `label` and `value`, all values of the same type).
  Numeric entries also require `min` (numeric).

- row_spec:

  A validated row_spec list, or `NULL`. Required when any `gate`
  condition uses `type = "selected"`, which requires
  `row_spec$selectable == TRUE`.

## Value

Called for its side-effect. Returns invisibly on success.
