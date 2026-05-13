# Server function for the editable table Shiny module

Renders a reactable table with one inline Shiny input per col_spec
column per row. When row_spec\$selectable is TRUE, a checkbox column is
prepended and clicking anywhere in a row toggles that row's selection.

## Usage

``` r
editable_table_server(
  id,
  data_r,
  row_spec,
  col_spec,
  existing_data_r = NULL,
  reactable_options = list()
)
```

## Arguments

- id:

  A string. The module namespace ID.

- data_r:

  A reactive data frame containing at least the columns declared in
  `row_spec`.

- row_spec:

  A validated row_spec list (see `validate_row_spec`).

- col_spec:

  A validated col_spec list (see `validate_col_spec`).

- existing_data_r:

  A reactive data frame of previously saved values, or `NULL` (default)
  for a blank start.

- reactable_options:

  A plain list of additional arguments passed to
  [`reactable::reactable()`](https://glin.github.io/reactable/reference/reactable.html).
  Module-controlled arguments (`data`, `columns`, `onClick`) are
  protected and cannot be overridden. Defaults that can be overridden
  include `sortable`, `pagination`, `highlight`, `bordered`, and
  `striped`.

## Value

A named list of two reactives:

- `current_data`:

  A reactive tibble with id_col + one column per col_spec entry, updated
  continuously as the user interacts.

- `selected_ids`:

  A reactive vector of id_col values for checked rows. `NULL` when
  `row_spec$selectable` is not TRUE.

## Details

A Reset button wipes all inputs and clears all selections.
