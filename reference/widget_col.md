# Build a widget column specification

Defines a single column in a
[`table_config()`](https://matthewcurrier.github.io/reactablePlus/reference/table_config.md).
Each column maps to either a picker widget or a primitive inline input
and carries type-specific options.

## Usage

``` r
widget_col(
  id,
  type,
  label,
  width = NULL,
  min_width = NULL,
  options = list(),
  gate = NULL,
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

  Character. One of:

  **Picker widgets** (complex popover-based inputs): `"search_picker"`,
  `"attendance_picker"`, `"homeschool_picker"`, `"notes_input"`.

  **Primitive inputs** (inline HTML form controls): `"dropdown"`,
  `"numeric"`, `"date"`, `"checkbox"`, `"toggle"`, `"text"`.

  **Extensible**: `"custom"` (requires `render_cell_fn`).

- label:

  Character. Column header text.

- width:

  Integer or NULL. Fixed column width in pixels.

- min_width:

  Integer or NULL. Minimum column width.

- options:

  Named list of type-specific options:

  **Picker widgets**: passed to the widget constructor (e.g.
  `list(show_nces_id = TRUE)` for search picker, `list(sections = ...)`
  for attendance picker).

  **dropdown**: `choices` (character vector, named character vector, or
  list of `list(label, value)`; see
  [`normalize_choices()`](https://matthewcurrier.github.io/reactablePlus/reference/normalize_choices.md)).
  Required unless `choices_fn` is provided. `placeholder` (default
  `"-- Select --"`). `choices_fn` (optional function `(row_state)` -\>
  choices; when provided, called at render time to compute choices
  dynamically — the return value is normalized via
  [`normalize_choices()`](https://matthewcurrier.github.io/reactablePlus/reference/normalize_choices.md)).
  `choices_depends_on` (optional character vector of column IDs whose
  changes should trigger a re-render so the cascading choices update;
  columns listed here are auto-marked `triggers_rerender = TRUE` by
  [`table_config()`](https://matthewcurrier.github.io/reactablePlus/reference/table_config.md)).

  **numeric**: `min` (required), `max`, `step`.

  **date**: `min_date`, `max_date` (YYYY-MM-DD strings).

  **text**: `max_chars`, `placeholder`.

  **checkbox** / **toggle**: no extra options.

- gate:

  A list of conditions that must ALL pass for this column's input to be
  enabled. When any condition fails, the input renders locked
  (disabled + dimmed) and its value is forced to `empty_value` in
  `get_data()`. Two condition types are supported:

  `list(type = "value", col_id = "status", values = c("active"))` — the
  column whose `id` matches `col_id` must have a value in `values` for
  this row.

  `list(type = "selected")` — the row must be selected (requires
  `selectable = TRUE` in
  [`table_config()`](https://matthewcurrier.github.io/reactablePlus/reference/table_config.md)).

  Default `NULL` (no gating — always enabled).

- gear_toggle:

  Character or NULL. The gear toggle key that controls this column's
  visibility. When the toggle is off, the column is hidden (set to
  `show = FALSE`).

- triggers_rerender:

  Logical. If TRUE, changes to this column bump the render key (forcing
  a full table re-render). Needed for columns involved in mutual
  exclusion. Automatically set to TRUE by
  [`table_config()`](https://matthewcurrier.github.io/reactablePlus/reference/table_config.md)
  for columns that act as gate controllers. Default `FALSE`.

- empty_value:

  The default value for an empty row. When `NULL` (the default), a
  type-appropriate blank is used: `NA` for dropdown/date, `options$min`
  (or `0`) for numeric, `FALSE` for checkbox/toggle, `""` for
  text/notes_input, `NULL` for pickers.

- render_cell_fn:

  Function `(ns, row_key, row_state, col_spec, settings)` -\> HTML
  string. Only needed for `type = "custom"`.

- validate_fn:

  Function `(value)` -\> validated value. Controls how raw input is
  cleaned before storage. Default `NULL` uses type-appropriate
  validation.

## Value

A `widget_col` list (S3 class `"widget_col"`).

## Examples

``` r
# Primitive dropdown
widget_col("status", "dropdown", "Status",
  options = list(choices = c("Active", "Inactive", "Pending"))
)
#> $id
#> [1] "status"
#> 
#> $type
#> [1] "dropdown"
#> 
#> $label
#> [1] "Status"
#> 
#> $width
#> NULL
#> 
#> $min_width
#> NULL
#> 
#> $options
#> $options$choices
#> $options$choices[[1]]
#> $options$choices[[1]]$label
#> [1] "Active"
#> 
#> $options$choices[[1]]$value
#> [1] "Active"
#> 
#> 
#> $options$choices[[2]]
#> $options$choices[[2]]$label
#> [1] "Inactive"
#> 
#> $options$choices[[2]]$value
#> [1] "Inactive"
#> 
#> 
#> $options$choices[[3]]
#> $options$choices[[3]]$label
#> [1] "Pending"
#> 
#> $options$choices[[3]]$value
#> [1] "Pending"
#> 
#> 
#> 
#> 
#> $gate
#> NULL
#> 
#> $gear_toggle
#> NULL
#> 
#> $triggers_rerender
#> [1] FALSE
#> 
#> $empty_value
#> [1] NA
#> 
#> $render_cell_fn
#> NULL
#> 
#> $validate_fn
#> NULL
#> 
#> attr(,"class")
#> [1] "widget_col"

# Numeric gated by a dropdown value
widget_col("score", "numeric", "Score",
  options = list(min = 0, max = 100),
  gate = list(
    list(type = "value", col_id = "status", values = c("active"))
  )
)
#> $id
#> [1] "score"
#> 
#> $type
#> [1] "numeric"
#> 
#> $label
#> [1] "Score"
#> 
#> $width
#> NULL
#> 
#> $min_width
#> NULL
#> 
#> $options
#> $options$min
#> [1] 0
#> 
#> $options$max
#> [1] 100
#> 
#> 
#> $gate
#> $gate[[1]]
#> $gate[[1]]$type
#> [1] "value"
#> 
#> $gate[[1]]$col_id
#> [1] "status"
#> 
#> $gate[[1]]$values
#> [1] "active"
#> 
#> 
#> 
#> $gear_toggle
#> NULL
#> 
#> $triggers_rerender
#> [1] FALSE
#> 
#> $empty_value
#> [1] 0
#> 
#> $render_cell_fn
#> NULL
#> 
#> $validate_fn
#> NULL
#> 
#> attr(,"class")
#> [1] "widget_col"

# Picker widget
widget_col("school", "search_picker", "School", min_width = 300)
#> $id
#> [1] "school"
#> 
#> $type
#> [1] "search_picker"
#> 
#> $label
#> [1] "School"
#> 
#> $width
#> NULL
#> 
#> $min_width
#> [1] 300
#> 
#> $options
#> list()
#> 
#> $gate
#> NULL
#> 
#> $gear_toggle
#> NULL
#> 
#> $triggers_rerender
#> [1] FALSE
#> 
#> $empty_value
#> NULL
#> 
#> $render_cell_fn
#> NULL
#> 
#> $validate_fn
#> NULL
#> 
#> attr(,"class")
#> [1] "widget_col"

# Cascading dropdown — choices depend on another column's value
widget_col("city", "dropdown", "City",
  options = list(
    choices = c("-- pick a state first --"),
    choices_fn = function(row_state) {
      switch(row_state$state %||% "",
        "CA" = c("LA" = "la", "SF" = "sf"),
        "NY" = c("NYC" = "nyc", "Buffalo" = "buf"),
        c("-- pick a state first --")
      )
    },
    choices_depends_on = "state"
  )
)
#> $id
#> [1] "city"
#> 
#> $type
#> [1] "dropdown"
#> 
#> $label
#> [1] "City"
#> 
#> $width
#> NULL
#> 
#> $min_width
#> NULL
#> 
#> $options
#> $options$choices
#> $options$choices[[1]]
#> $options$choices[[1]]$label
#> [1] "-- pick a state first --"
#> 
#> $options$choices[[1]]$value
#> [1] "-- pick a state first --"
#> 
#> 
#> 
#> $options$choices_fn
#> function (row_state) 
#> {
#>     switch(row_state$state %||% "", CA = c(LA = "la", SF = "sf"), 
#>         NY = c(NYC = "nyc", Buffalo = "buf"), c("-- pick a state first --"))
#> }
#> <environment: 0x55fd6e8567a0>
#> 
#> $options$choices_depends_on
#> [1] "state"
#> 
#> 
#> $gate
#> NULL
#> 
#> $gear_toggle
#> NULL
#> 
#> $triggers_rerender
#> [1] FALSE
#> 
#> $empty_value
#> [1] NA
#> 
#> $render_cell_fn
#> NULL
#> 
#> $validate_fn
#> NULL
#> 
#> attr(,"class")
#> [1] "widget_col"
```
