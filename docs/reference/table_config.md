# Build a table configuration

Creates a declarative configuration object that drives
[`config_table_ui()`](https://matthewcurrier.github.io/reactablePlus/reference/config_table_ui.md)
/
[`config_table_server()`](https://matthewcurrier.github.io/reactablePlus/reference/config_table_server.md).
The config describes which rows and columns appear, which picker widget
each column uses, and how cross-column interactions (mutual exclusion,
fill-down) work.

## Usage

``` r
table_config(
  row_keys,
  row_labels,
  columns,
  gear_toggles = NULL,
  interactions = list(),
  from_saved_fn = NULL,
  to_output_fn = NULL,
  toolbar_stats_fn = NULL,
  search_fn_col = NULL,
  badge_col = "grade",
  year_col = "school_year",
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
  [`useSchoolSearch()`](https://matthewcurrier.github.io/reactablePlus/reference/useSchoolSearch.md)).
  Pass `NULL` if no column needs search. Default `NULL`.

- badge_col:

  Character. Column ID rendered as a static badge from the row
  key/label. Default `"grade"`. Pass `NULL` to omit.

- year_col:

  Character. Column ID for the editable year spinner. Default
  `"school_year"`. Pass `NULL` to omit.

- year_range:

  Integer vector of length 2, e.g. `c(1990, 2050)`.

## Value

A `table_config` list (S3 class `"table_config"`).

## Details

### Interactions

`interactions$mutual_exclusion`: a list of rules, each a list with
`when_on` (column ID), `clears` (column ID), and `display` (HTML string
to show in the cleared column's cell).

`interactions$fill_down`: a list with `column` (column ID) and
`range_check` (logical — if TRUE, uses
[`gradeInRange()`](https://matthewcurrier.github.io/reactablePlus/reference/gradeInRange.md)
to limit).

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- table_config(
  row_keys   = c("PK", "K", "01"),
  row_labels = c("PreK", "K", "1st"),
  columns    = list(
    widget_col("school", "school_picker", "School", min_width = 300),
    widget_col("attendance", "attendance_picker", "Attendance", width = 200)
  )
)
} # }
```
