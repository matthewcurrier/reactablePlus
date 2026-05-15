# Getting Started with reactablePlus

reactablePlus extends [reactable](https://glin.github.io/reactable/)
with inline editing and popover-based picker widgets for Shiny. Build
editable tables declaratively with
[`table_config()`](https://matthewcurrier.github.io/reactablePlus/reference/table_config.md)
and
[`widget_col()`](https://matthewcurrier.github.io/reactablePlus/reference/widget_col.md),
wire them up with
[`config_table_ui()`](https://matthewcurrier.github.io/reactablePlus/reference/config_table_ui.md)
/
[`config_table_server()`](https://matthewcurrier.github.io/reactablePlus/reference/config_table_server.md),
and get a fully interactive table with gating, row selection, reset, and
live data output.

## Installation

``` r

# install.packages("pak")
pak::pak("matthewcurrier/reactablePlus")
```

## Quick start

A working editable table in under 30 lines:

``` r

library(shiny)
library(reactablePlus)

cfg <- table_config(
  row_keys   = c("row1", "row2", "row3"),
  row_labels = c("Alice", "Bob", "Carol"),

  columns = list(
    widget_col("role", "dropdown", "Role",
      options = list(choices = c("Analyst", "Engineer", "Manager"))
    ),
    widget_col("score", "numeric", "Score",
      options = list(min = 0, max = 100, step = 1)
    ),
    widget_col("active", "checkbox", "Active"),
    widget_col("notes", "text", "Notes",
      options = list(placeholder = "Optional notes...", max_chars = 200)
    )
  ),

  to_output_fn = function(row_state, row_key) {
    data.frame(
      id     = row_key,
      role   = row_state$role %||% NA_character_,
      score  = row_state$score %||% NA_real_,
      active = isTRUE(row_state$active),
      notes  = row_state$notes %||% "",
      stringsAsFactors = FALSE
    )
  }
)

ui <- fluidPage(
  config_table_ui("demo", cfg)
)

server <- function(input, output, session) {
  result <- config_table_server("demo", cfg)

  observe({
    print(result$get_data())
  })
}

shinyApp(ui, server)
```

## Architecture at a glance

reactablePlus is organized in two layers. Use either independently or
combine them.

**Layer 1 – Picker widgets.** Standalone Shiny inputs
(`searchPickerInput`, `attendancePickerInput`, `homeschoolPickerInput`,
`notesInput`, `gearPopoverInput`) with custom JS bindings. Use them
anywhere you’d use a regular Shiny input.

**Layer 2 – Config-driven table module.** A Shiny module
(`config_table_ui` / `config_table_server`) that builds an editable
reactable from a
[`table_config()`](https://matthewcurrier.github.io/reactablePlus/reference/table_config.md)
object. Supports both primitive inputs (dropdown, numeric, date,
checkbox, toggle, text) and popover picker widgets – with gating, row
selection, reset, mutual exclusion, and fill-down. Tables can be static
(rows fixed at config time) or dynamic (rows derived from a reactive
`source_data` data frame, with state preservation across changes).

## Primitive input types

[`widget_col()`](https://matthewcurrier.github.io/reactablePlus/reference/widget_col.md)
supports six inline HTML input types. Type-specific options go in the
`options` list.

``` r

columns <- list(
  # Dropdown -- choices can be a character vector, named vector, or
  # list of list(label, value). All three are normalized internally.
  widget_col("status", "dropdown", "Status",
    options = list(
      choices = c("Active" = "active", "On Leave" = "leave", "Terminated" = "term")
    )
  ),

  # Numeric -- min is required; max and step are optional
  widget_col("hours", "numeric", "Hours",
    options = list(min = 0, max = 80, step = 0.5)
  ),

  # Date -- with optional bounds
  widget_col("start", "date", "Start Date",
    options = list(min_date = "2020-01-01", max_date = "2030-12-31")
  ),

  # Checkbox -- no extra options
  widget_col("enrolled", "checkbox", "Enrolled"),

  # Toggle -- rendered as an on/off button
  widget_col("billable", "toggle", "Billable"),

  # Text -- with optional max_chars and placeholder
  widget_col("notes", "text", "Notes",
    options = list(max_chars = 500, placeholder = "Enter notes...")
  )
)
```

## Dropdown choices: three formats

All three are normalized via
[`normalize_choices()`](https://matthewcurrier.github.io/reactablePlus/reference/normalize_choices.md).
Use the simplest format that fits.

``` r

# 1. Plain vector -- label equals value
widget_col("color", "dropdown", "Color",
  options = list(choices = c("Red", "Blue", "Green"))
)

# 2. Named vector -- names become display labels
widget_col("status", "dropdown", "Status",
  options = list(choices = c("Active" = "active", "On Leave" = "leave"))
)

# 3. List of lists -- for non-character values
widget_col("rating", "dropdown", "Rating",
  options = list(choices = list(
    list(label = "Good", value = 3),
    list(label = "OK", value = 2),
    list(label = "Poor", value = 1)
  ))
)
```

## Gating (conditional inputs)

Use the `gate` parameter on
[`widget_col()`](https://matthewcurrier.github.io/reactablePlus/reference/widget_col.md)
to lock a column until conditions are met. Gates are purely
server-driven – no client-side JavaScript needed.

``` r

cfg <- table_config(
  row_keys   = c("r1", "r2", "r3"),
  row_labels = c("Item A", "Item B", "Item C"),

  columns = list(
    widget_col("status", "dropdown", "Status",
      options = list(choices = c("Active" = "active", "Inactive" = "inactive"))
    ),
    # Score is locked until Status = "active"
    widget_col("score", "numeric", "Score",
      options = list(min = 0, max = 100),
      gate = list(
        list(type = "value", col_id = "status", values = c("active"))
      )
    )
  ),

  to_output_fn = function(row_state, row_key) {
    data.frame(
      id     = row_key,
      status = row_state$status %||% NA_character_,
      score  = row_state$score %||% NA_real_,
      stringsAsFactors = FALSE
    )
  }
)
```

[`table_config()`](https://matthewcurrier.github.io/reactablePlus/reference/table_config.md)
automatically marks controller columns (here, `"status"`) as
`triggers_rerender = TRUE` so the table re-renders when they change.
Gated values are enforced server-side in `get_data()` – a locked
column’s value is forced to its `empty_value`.

Gates can also require row selection:

``` r

widget_col("priority", "dropdown", "Priority",
  options = list(choices = c("High", "Medium", "Low")),
  gate = list(
    list(type = "selected"),
    list(type = "value", col_id = "status", values = c("active"))
  )
)
```

All conditions use AND logic – every condition must pass for the input
to unlock.

## Row selection and reset

Enable row selection with `selectable = TRUE` and a reset button with
`show_reset = TRUE`:

``` r

cfg <- table_config(
  row_keys   = c("r1", "r2"),
  row_labels = c("Item A", "Item B"),
  selectable = TRUE,
  show_reset = TRUE,

  columns = list(
    widget_col("notes", "text", "Notes")
  ),

  to_output_fn = function(row_state, row_key) {
    data.frame(
      id       = row_key,
      selected = isTRUE(row_state$.selected),
      notes    = row_state$notes %||% "",
      stringsAsFactors = FALSE
    )
  }
)

# In the server:
result <- config_table_server("demo", cfg)

# result$get_data()     -- reactive data frame with gate enforcement
# result$selected_ids() -- reactive character vector of selected row keys
```

Reset clears all inputs to their `empty_value` and deselects all rows in
a single reactive flush.

To let users click on non-interactive columns (badge, display columns)
to toggle selection, add `click_to_select = TRUE`:

``` r

cfg <- table_config(
  row_id_col    = "student_id",
  row_label_col = "name",

  display_cols = list(
    display_col("email", "Email")
  ),

  columns = list(
    widget_col("notes", "text", "Notes")
  ),

  selectable      = TRUE,
  click_to_select = TRUE,
  badge_col       = "student",
  badge_label     = "Student",

  to_output_fn = function(row_state, row_key) {
    data.frame(
      id       = row_key,
      selected = isTRUE(row_state$.selected),
      notes    = row_state$notes %||% "",
      stringsAsFactors = FALSE
    )
  }
)
```

Clicking a student name or email toggles the checkbox. Widget columns
(dropdowns, text inputs, etc.) are not affected — they keep their normal
interaction.

## Dynamic rows

Static mode (the examples above) is perfect when you know the rows at
config time. But some use cases need rows that appear, disappear, or
change reactively — a filtered roster, search results, records from a
database query. Dynamic mode handles this.

Enable it by setting `row_id_col` instead of `row_keys` / `row_labels`:

``` r

students <- data.frame(
  student_id = paste0("s", 1:6),
  name = c("Amara", "Ben", "Clara", "David", "Elena", "Femi"),
  email = paste0(
    c("amara", "ben", "clara", "david", "elena", "femi"),
    "@school.edu"
  ),
  dept = c("Eng", "Eng", "Sci", "Sci", "Arts", "Arts"),
  stringsAsFactors = FALSE
)

cfg <- table_config(
  row_id_col    = "student_id",
  row_label_col = "name",

  columns = list(
    widget_col("status", "dropdown", "Status",
      options = list(choices = c("Active" = "active", "On Leave" = "leave"))
    ),
    widget_col("score", "numeric", "Score",
      options = list(min = 0, max = 100)
    )
  ),

  to_output_fn = function(row_state, row_key) {
    data.frame(
      student_id = row_key,
      status     = row_state$status %||% NA_character_,
      score      = row_state$score %||% NA_real_,
      stringsAsFactors = FALSE
    )
  }
)
```

In the server, pass a reactive data frame as `source_data`. Each time it
changes, the module derives the new row set, preserves user-entered
values for surviving rows, and assigns defaults for new ones:

``` r

server <- function(input, output, session) {
  filtered <- reactive({
    students[students$dept == input$dept_filter, ]
  })

  result <- config_table_server("demo", cfg,
    source_data = filtered
  )
}
```

If a row leaves (because the filter changed) and later returns, its
edits are restored — the module retains departed rows’ state internally.

For computed labels, pass a function instead of a column name:

``` r

cfg <- table_config(
  row_id_col   = "id",
  row_label_fn = function(row) paste(row$first, row$last),
  columns = list(widget_col("x", "text", "X"))
)
```

## Mid-session reset

When the same table needs to switch between different data contexts
(e.g. opening Report B after Report A), use `reset_signal` to replace
both column values and selection state atomically:

``` r

server <- function(input, output, session) {
  reset <- reactiveVal(NULL)

  observeEvent(input$switch_report, {
    saved <- load_report(input$report_id)
    reset(list(
      data     = saved,              # data.frame parsed via from_saved_fn
      selected = saved$id[saved$sel] # character vector, or NULL to clear
    ))
  })

  result <- config_table_server("tbl", cfg,
    source_data  = filtered,
    reset_signal = reset
  )
}
```

The signal is ignored when `NULL` (including the initial
`reactiveVal(NULL)` at startup). Only non-NULL values trigger a state
replacement. This works in both static and dynamic mode.

## Display columns

In dynamic mode, you often want to show read-only context from
`source_data` alongside the editable columns — an email address, a
department, an enrollment date. Use
[`display_col()`](https://matthewcurrier.github.io/reactablePlus/reference/display_col.md):

``` r

cfg <- table_config(
  row_id_col    = "student_id",
  row_label_col = "name",

  display_cols = list(
    display_col("email", "Email", min_width = 180),
    display_col("dept", "Dept", width = 100)
  ),

  columns = list(
    widget_col("status", "dropdown", "Status",
      options = list(choices = c("Active" = "active", "Inactive" = "inactive"))
    )
  ),

  to_output_fn = function(row_state, row_key) {
    data.frame(
      student_id = row_key,
      status     = row_state$status %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }
)
```

Display columns appear between the badge column and the editable widget
columns. They accept `width`, `min_width`, and an optional `render_fn`
for custom HTML rendering:

``` r

display_col("status", "Status", render_fn = function(value, row_key) {
  cls <- if (identical(value, "active")) "text-success" else "text-muted"
  sprintf('<span class="%s">%s</span>', cls, value)
})
```

## Picker widgets in tables

For complex inputs – typeahead search, multi-field popovers, rating
scales – use the picker widget types:

``` r

cfg <- table_config(
  row_keys   = c("PK", "K", "01"),
  row_labels = c("PreK", "K", "1st"),

  columns = list(
    widget_col("school", "search_picker", "School", min_width = 300),
    widget_col("attendance", "attendance_picker", "Attendance", width = 200),
    widget_col("notes", "notes_input", "Notes", min_width = 150)
  ),

  # Show a grade badge column
  badge_col   = "grade",
  badge_label = "Grade",
  badge_render_fn = function(row_key, row_label) {
    css_class <- paste0("grade-badge g-", row_key)
    sprintf('<span class="%s">%s</span>', css_class, row_label)
  }
)
```

Picker widgets require
[`useReactablePlus()`](https://matthewcurrier.github.io/reactablePlus/reference/useReactablePlus.md)
in the UI (which
[`config_table_ui()`](https://matthewcurrier.github.io/reactablePlus/reference/config_table_ui.md)
includes automatically) and
[`bindPickersOnRender()`](https://matthewcurrier.github.io/reactablePlus/reference/bindPickersOnRender.md)
for binding initialization (which
[`config_table_server()`](https://matthewcurrier.github.io/reactablePlus/reference/config_table_server.md)
handles internally).

## Using picker widgets standalone

Every picker widget works as a standalone Shiny input outside of tables.
Call
[`useReactablePlus()`](https://matthewcurrier.github.io/reactablePlus/reference/useReactablePlus.md)
in your UI to load the JS and CSS.

``` r

library(shiny)
library(reactablePlus)

ui <- fluidPage(
  useReactablePlus(),
  searchPickerInput("my_school", grade_key = "PK",
                    grade_label = "PreK"),
  attendancePickerInput("my_att", grade_label = "PreK"),
  notesInput("my_notes", placeholder = "Type here...")
)

server <- function(input, output, session) {
  observeEvent(input$my_school, {
    message("School selected: ", input$my_school$name)
  })
}

shinyApp(ui, server)
```

## Embedding widgets inside a custom reactable

When placing widgets inside `reactable::colDef(cell = ...)` outside of
`config_table`, two extra steps are needed because reactable’s
React-based rendering doesn’t process Shiny input bindings
automatically.

1.  Call
    [`useReactablePlus()`](https://matthewcurrier.github.io/reactablePlus/reference/useReactablePlus.md)
    in the UI to load JS/CSS at the page level.
2.  Wrap the reactable widget with
    [`bindPickersOnRender()`](https://matthewcurrier.github.io/reactablePlus/reference/bindPickersOnRender.md)
    to initialize input bindings after the table DOM is created.

``` r

library(shiny)
library(reactablePlus)

ui <- fluidPage(
  useReactablePlus(),
  reactable::reactableOutput("tbl")
)

server <- function(input, output, session) {
  output$tbl <- reactable::renderReactable({
    tbl <- reactable::reactable(
      data.frame(grade = c("PK", "K"), notes = c("", "")),
      columns = list(
        notes = reactable::colDef(
          html = TRUE,
          cell = function(value, index) {
            as.character(notesInput(
              paste0("note_", index),
              value = value,
              placeholder = "Add a note"
            ))
          }
        )
      )
    )
    bindPickersOnRender(tbl)
  })
}

shinyApp(ui, server)
```

## Configurable hooks

[`table_config()`](https://matthewcurrier.github.io/reactablePlus/reference/table_config.md)
provides hooks that keep the core module generic while letting
domain-specific apps customize appearance:

- `badge_col` / `badge_label` / `badge_render_fn` – opt-in label column
  with custom rendering (e.g., grade badges with CSS classes)
- `row_class_fn(row_key, row_state)` – custom CSS classes per row
- `to_output_fn(row_state, row_key)` – marshal row state into a data
  frame for your output schema
- `from_saved_fn(db_row, col_specs)` – parse saved data into row state
- `toolbar_stats_fn(rows, row_keys)` – dynamic toolbar content
- `year_col` / `year_range` – opt-in editable year spinner column
- `row_id_col` / `row_label_col` / `row_label_fn` – dynamic row mode
- `display_cols` – read-only columns from `source_data` (dynamic mode)

## Grade utilities

For PreK–12 education apps, reactablePlus exports helper functions in
`grade-utils.R`:

- [`gradeChoices()`](https://matthewcurrier.github.io/reactablePlus/reference/gradeChoices.md)
  – named character vector of the 14-grade ladder
- [`gradeLabelMap()`](https://matthewcurrier.github.io/reactablePlus/reference/gradeChoices.md)
  – grade key to display label lookup
- [`gradeIndex()`](https://matthewcurrier.github.io/reactablePlus/reference/gradeIndex.md)
  – grade key to numeric index for ordering
- [`gradeInRange()`](https://matthewcurrier.github.io/reactablePlus/reference/gradeInRange.md)
  – check if a grade falls within a school’s range

These are convenience exports, not required by the table infrastructure.

## Next steps

- Browse the
  [Reference](https://matthewcurrier.github.io/reactablePlus/articles/reference/index.md)
  for full documentation of every exported function.
- See
  [`table_config()`](https://matthewcurrier.github.io/reactablePlus/reference/table_config.md)
  for mutual exclusion, fill-down, and gear toggle configuration.
- See
  [`widget_col()`](https://matthewcurrier.github.io/reactablePlus/reference/widget_col.md)
  for the full list of types, options, and gate conditions.
