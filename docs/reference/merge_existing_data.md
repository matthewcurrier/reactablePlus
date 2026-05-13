# Merge previously saved values onto the display data frame

Left-joins `existing_data` onto `data_df` using the id column declared
in `row_spec`. Rows in `data_df` with no match in `existing_data`
receive `NA` for every col_spec column. Rows in `existing_data` that are
absent from `data_df` are silently dropped.

## Usage

``` r
merge_existing_data(data_df, existing_data, row_spec, col_spec)
```

## Arguments

- data_df:

  A tibble containing at least the id_col and display columns declared
  in `row_spec`.

- existing_data:

  A tibble of previously saved values (id_col + col_spec columns), or
  `NULL` for a blank start.

- row_spec:

  A validated row_spec list.

- col_spec:

  A validated col_spec list.

## Value

A tibble with exactly the id_col, display_cols, and col_spec columns —
in that order. One row per row in `data_df`.
