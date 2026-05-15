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
  row_keys = NULL,
  row_labels = NULL,
  columns,
  row_id_col = NULL,
  row_label_col = NULL,
  row_label_fn = NULL,
  display_cols = NULL,
  selectable = FALSE,
  click_to_select = FALSE,
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
  year_range = c(1990L, 2050L),
  appendable = FALSE,
  allow_delete = TRUE,
  min_rows = 0L,
  max_rows = NULL
)
```

## Arguments

- row_keys:

  Character vector of row identifiers (e.g. grade keys). Required in
  static mode. In dynamic mode, defaults to `character(0)` — rows are
  derived from `source_data` at runtime.

- row_labels:

  Character vector of display labels (same length as `row_keys`).
  Required in static mode. In dynamic mode, defaults to `character(0)` —
  labels are derived via `row_label_col` or `row_label_fn`.

- columns:

  List of column specs built with
  [`widget_col()`](https://matthewcurrier.github.io/reactablePlus/reference/widget_col.md).

- row_id_col:

  `character(1)` or `NULL`. The name of the column in `source_data` that
  uniquely identifies each row. When non-NULL the config operates in
  dynamic mode. Default `NULL` (static mode).

- row_label_col:

  `character(1)` or `NULL`. The name of the column in `source_data` to
  use as the display label. Ignored when `row_id_col` is `NULL`. Exactly
  one of `row_label_col` or `row_label_fn` must be supplied in dynamic
  mode.

- row_label_fn:

  `function` or `NULL`. A function `(source_data_row)` -\>
  `character(1)` that computes a display label from a single row (a
  one-row data frame) of `source_data`. Ignored when `row_id_col` is
  `NULL`. Exactly one of `row_label_col` or `row_label_fn` must be
  supplied in dynamic mode.

- display_cols:

  List of
  [`display_col()`](https://matthewcurrier.github.io/reactablePlus/reference/display_col.md)
  specs, or `NULL`. Read-only columns whose values are drawn from
  `source_data` in dynamic mode. Displayed between the badge column and
  the editable widget columns. Requires dynamic mode (`row_id_col` must
  be set). Default `NULL` (no display columns).

- selectable:

  Logical. If `TRUE`, a checkbox column is prepended and row selection
  is tracked. Required when any column has a `gate` condition with
  `type = "selected"`. Default `FALSE`.

- click_to_select:

  Logical. If `TRUE`, clicking on the badge column or any display column
  toggles the row's selection checkbox. Requires `selectable = TRUE`.
  Widget columns are excluded — they have their own click interactions.
  Default `FALSE`.

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

- appendable:

  `logical(1)`. When `TRUE`, users can add and remove rows at runtime
  through "Add Row" and per-row "Delete" buttons rendered by the module.
  Mutually exclusive with dynamic mode (`row_id_col` must be `NULL`). In
  appendable mode, `row_keys` and `row_labels` default to `character(0)`
  — the table starts empty (or with `min_rows` blank rows) and grows as
  the user adds entries. Default `FALSE`.

- allow_delete:

  `logical(1)`. When `TRUE` (and `appendable` is `TRUE`), each row shows
  a "Delete" button. Deletion is prevented when the table is at
  `min_rows`. Ignored when `appendable` is `FALSE`. Default `TRUE`.

- min_rows:

  `integer(1)`. The minimum number of rows the table maintains in
  appendable mode. The module seeds this many blank rows on startup and
  prevents deletion below this count. Must be non-negative. Ignored when
  `appendable` is `FALSE`. Default `0L`.

- max_rows:

  `integer(1)` or `NULL`. The maximum number of rows allowed in
  appendable mode. When non-`NULL`, the "Add Row" button is disabled
  once the table reaches this count. Must be `>= min_rows` when both are
  set. Ignored when `appendable` is `FALSE`. Default `NULL` (unlimited).

## Value

A `table_config` list (S3 class `"table_config"`).

## Details

Two modes are supported:

**Static mode** (default): rows are fixed at config time via `row_keys`
/ `row_labels`. Use when the set of editable rows is known up front
(e.g. a grade roster).

**Dynamic mode**: rows are derived at runtime from a reactive
`source_data` data frame passed to
[`config_table_server()`](https://matthewcurrier.github.io/reactablePlus/reference/config_table_server.md).
Enable by supplying `row_id_col` and either `row_label_col` or
`row_label_fn`. In this mode `row_keys` / `row_labels` are optional
(default to `character(0)`).

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

### Dynamic mode

When `row_id_col` is non-NULL, the config is in dynamic mode. Rows are
not fixed at config time — they are derived from the `source_data`
reactive at runtime. Each time `source_data` changes, the module merges
the new row set with existing state: user-entered values for rows that
survive are preserved, new rows receive `empty_value` defaults, and
departed rows' state is retained internally so it restores if those rows
reappear.

### Appendable mode

When `appendable` is `TRUE`, the table becomes a variable-length input
collector where the **user** controls the row set — adding blank rows,
deleting individual rows, and optionally resetting to the minimum. This
is mutually exclusive with dynamic mode.

Row keys are generated automatically as `"row_1"`, `"row_2"`, …
(incrementing, never reused in a session). The module renders "Add Row"
and per-row "Delete" buttons. `min_rows` seeds the initial table and
prevents deletion below that count. `max_rows` caps the upper bound.

## Examples

``` r
if (FALSE) { # \dontrun{
# Static mode — rows fixed at config time
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

# Dynamic mode — rows derived from source_data at runtime
cfg_dyn <- table_config(
  row_id_col    = "student_id",
  row_label_col = "student_name",
  columns = list(
    widget_col("status", "dropdown", "Status",
      options = list(choices = c("Active", "Inactive"))
    ),
    widget_col("score", "numeric", "Score",
      options = list(min = 0, max = 100)
    )
  ),
  selectable = TRUE,
  to_output_fn = function(row_state, row_key) {
    data.frame(
      id     = row_key,
      status = row_state$status %||% NA_character_,
      score  = row_state$score %||% NA_real_,
      stringsAsFactors = FALSE
    )
  }
)

# Appendable mode — user adds and removes rows
cfg_app <- table_config(
  appendable = TRUE,
  min_rows   = 1L,
  max_rows   = 10L,
  columns = list(
    widget_col("fruit", "dropdown", "Fruit",
      options = list(choices = c("Apple", "Banana", "Cherry"))
    ),
    widget_col("qty", "numeric", "Quantity",
      options = list(min = 1, max = 100)
    )
  ),
  show_reset = TRUE,
  to_output_fn = function(row_state, row_key) {
    data.frame(
      fruit = row_state$fruit %||% NA_character_,
      qty   = row_state$qty %||% NA_real_,
      stringsAsFactors = FALSE
    )
  }
)
} # }
```
