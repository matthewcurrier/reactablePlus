# Build a widget column specification

Defines a single column in a
[`table_config()`](https://matthewcurrier.github.io/reactablePlus/reference/table_config.md).
Each widget column maps to a picker widget type (`school_picker`,
`attendance_picker`, `homeschool_picker`, `notes_input`, or `custom`)
and carries widget-specific options.

## Usage

``` r
widget_col(
  id,
  type,
  label,
  width = NULL,
  min_width = NULL,
  options = list(),
  gear_toggle = NULL,
  triggers_rerender = FALSE,
  empty_value = NULL,
  render_cell_fn = NULL,
  validate_fn = NULL
)
```

## Arguments

- id:

  Character. Column ID — becomes the key in row state and the prefix for
  Shiny input IDs (e.g. `"school"` -\> `input$school_PK`).

- type:

  Character. Widget type: `"school_picker"`, `"attendance_picker"`,
  `"homeschool_picker"`, `"notes_input"`, or `"custom"`.

- label:

  Character. Column header text.

- width:

  Integer or NULL. Fixed column width in pixels.

- min_width:

  Integer or NULL. Minimum column width.

- options:

  Named list of widget-specific options passed to the widget constructor
  (e.g. `list(show_nces_id = TRUE)` for school picker,
  `list(sections = ...)` for attendance picker).

- gear_toggle:

  Character or NULL. The gear toggle key that controls this column's
  visibility. When the toggle is off, the column is hidden (set to
  `show = FALSE`).

- triggers_rerender:

  Logical. If TRUE, changes to this column bump the render key (forcing
  a full table re-render). Needed for columns involved in mutual
  exclusion. Default `FALSE`.

- empty_value:

  The default value for an empty row. Default `NULL`.

- render_cell_fn:

  Function `(ns, row_key, row_state, col_spec, settings)` -\> HTML
  string. Only needed for `type = "custom"`.

- validate_fn:

  Function `(value)` -\> validated value. Controls how raw input is
  cleaned before storage. Default `NULL` uses type-appropriate
  validation.

## Value

A `widget_col` list (S3 class `"widget_col"`).
