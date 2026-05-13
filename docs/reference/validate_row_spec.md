# Validate a row_spec list

Checks that a row_spec is well-formed before any rendering or data
manipulation occurs. Throws an informative error on the first problem
found.

## Usage

``` r
validate_row_spec(row_spec)
```

## Arguments

- row_spec:

  A list with fields `id_col` (string) and `display_cols` (list of
  lists, each with `col_name` and `label`).

## Value

Called for its side-effect. Returns invisibly on success.
