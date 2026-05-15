# Build a table configuration

Creates a declarative configuration object that drives
[`config_table_ui()`](https://matthewcurrier.github.io/reactablePlus/reference/config_table_ui.md)
/
[`config_table_server()`](https://matthewcurrier.github.io/reactablePlus/reference/config_table_server.md).
The config describes which rows and columns appear, which picker widget
each column uses, and how cross-column interactions (mutual exclusion,
fill-down, gating) work.

## Usage

``` r
table_config(
  row_keys,
  row_labels,
  columns,
  selectable = FALSE,
  show_reset = FALSE,
  gear_toggles = NULL,
  interactions = list(),
  from_saved_fn = NULL,
  to_output_fn = NULL,
  toolbar_stats_fn = NULL,
  search_fn_col = NULL,
  badge_col = NULL,
  badge_label = "Label",
  badge_render_fn = NULL,
  row_class_fn = NULL,
  year_col = NULL,
  year_range = c(1990L, 2050L)
)
```

## Arguments

- row_keys:

  Character vector of row identifiers (e.g. grade keys).

- row_labels:

  Character vector of display labels (same length as `row_keys`).

- columns:

  List of column specs built with
  [`widget_col()`](https://matthewcurrier.github.io/reactablePlus/reference/widget_col.md).

- selectable:

  Logical. If `TRUE`, a checkbox column is prepended and row selection
  is tracked. Required when any column has a `gate` condition with
  `type = "selected"`. Default `FALSE`.

- show_reset:

  Logical. If `TRUE`, a Reset button is shown in the toolbar that clears
  all inputs and deselects all rows. Default `FALSE`.

- gear_toggles:

  Named list of toggle definitions for
  [`gearPopoverInput()`](https://matthewcurrier.github.io/reactablePlus/reference/gearPopoverInput.md).
  Pass `NULL` for no gear icon.

- interactions:

  List with optional `mutual_exclusion` and `fill_down` entries. See
  Details.

- from_saved_fn:

  Function `(db_row, col_specs)` -\> named list. Converts one row of
  saved data into a row-state list keyed by column ID. Called once per
  row when `data_r()` provides a data frame.

- to_output_fn:

  Function `(row_state, row_key)` -\> single-row data.frame. Converts
  one row of state back to the output schema.

- toolbar_stats_fn:

  Function `(rows, row_keys)` -\>
  [shiny::HTML](https://rdrr.io/pkg/shiny/man/reexports.html) string for
  the toolbar. Pass `NULL` for no toolbar.

- search_fn_col:

  Character. Column ID that requires server-side search (i.e. uses
  [`useTypeaheadSearch()`](https://matthewcurrier.github.io/reactablePlus/reference/useTypeaheadSearch.md)).
  Pass `NULL` if no column needs search. Default `NULL`.

- badge_col:

  Character or NULL. When non-NULL, a static label column is shown using
  `row_labels`. Default `NULL` (no badge).

- badge_label:

  Character. Column header for the badge column. Default `"Label"`.
  Ignored when `badge_col` is `NULL`.

- badge_render_fn:

  Function `(row_key, row_label)` -\> HTML string for the badge cell.
  Default `NULL` renders `<span>row_label</span>`. Supply a custom
  function for domain-specific styling (e.g. grade badges with CSS
  classes).

- row_class_fn:

  Function `(row_key, row_state)` -\> CSS class string or `NULL`.
  Applied per row via reactable's `rowClass`. Default `NULL` (no row
  classes).

- year_col:

  Character or NULL. When non-NULL, an editable year spinner column is
  shown. Default `NULL` (no year column).

- year_range:

  Integer vector of length 2, e.g. `c(1990, 2050)`.

## Value

A `table_config` list (S3 class `"table_config"`).

## Details

### Interactions

`interactions$mutual_exclusion`: a list of rules, each a list with
`when_on` (column ID), `clears` (column ID), and `display` (HTML string
to show in the cleared column's cell).

`interactions$fill_down`: a list with `column` (column ID),
`range_check_fn` (optional function `(row_key, value) -> logical` that
returns `TRUE` when the row is in range for fill-down), and optionally
`input_name` (character — the Shiny input name the widget JS sends;
defaults to `paste0(column, "_fill_down")`).

### Gating

Columns with `gate` conditions (set via `widget_col(gate = ...)`) are
automatically disabled until all conditions are met. Controller columns
referenced in `gate` conditions are automatically marked
`triggers_rerender = TRUE` so the table updates when they change.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- table_config(
  row_keys   = c("PK", "K", "01"),
  row_labels = c("PreK", "K", "1st"),
  columns    = list(
    widget_col("school", "search_picker", "School", min_width = 300),
    widget_col("attendance", "attendance_picker", "Attendance", width = 200)
  ),
  badge_col = "grade",
  badge_label = "Grade",
  badge_render_fn = function(row_key, row_label) {
    css_class <- paste0("grade-badge g-", row_key)
    sprintf('<span class="%s">%s</span>', css_class, row_label)
  }
)
} # }
```
