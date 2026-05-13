# Getting Started with reactablePlus

reactablePlus extends [reactable](https://glin.github.io/reactable/)
with inline editing widgets and two Shiny module layers for building
editable tables. This article walks through the key concepts.

## Installation

``` r

# install.packages("pak")
pak::pak("OWNER/reactablePlus")
```

## Two ways to build editable tables

reactablePlus offers two abstraction levels. Pick the one that fits your
use case.

### 1. Raw-spec module (maximum flexibility)

The lower-level module accepts plain lists describing rows and columns.
You control every column type (dropdown, numeric, checkbox, toggle,
date, text) and can wire gating, selection, and reset behavior.

``` r

library(shiny)
library(reactablePlus)

row_spec <- list(
  id_col   = "id",
  id_vals  = c("row1", "row2", "row3"),
  labels   = c("Alice", "Bob", "Carol"),
  selected = rep(TRUE, 3)
)

col_spec <- list(
  list(col_name = "role",  type = "dropdown",
       choices = c("Analyst", "Engineer", "Manager")),
  list(col_name = "score", type = "numeric",
       min = 0, max = 100, step = 1),
  list(col_name = "active", type = "checkbox")
)

ui <- fluidPage(
  editable_table_ui("demo")
)

server <- function(input, output, session) {
  editable_table_server(
    "demo",
    data_r     = reactive(NULL),
    row_spec   = row_spec,
    col_spec   = col_spec
  )
}

shinyApp(ui, server)
```

Use
[`validate_col_spec()`](https://matthewcurrier.github.io/reactablePlus/reference/validate_col_spec.md)
and
[`validate_row_spec()`](https://matthewcurrier.github.io/reactablePlus/reference/validate_row_spec.md)
to catch configuration errors before the app launches.

### 2. Config-driven module (widget-powered)

The higher-level module uses
[`table_config()`](https://matthewcurrier.github.io/reactablePlus/reference/table_config.md)
and
[`widget_col()`](https://matthewcurrier.github.io/reactablePlus/reference/widget_col.md)
to declaratively describe a table that renders picker widgets in each
cell. It handles mutual exclusion, fill-down, gear toggles, and data
marshaling out of the box.

``` r

library(shiny)
library(reactablePlus)

cfg <- table_config(
  row_keys   = c("PK", "K", "01", "02"),
  row_labels = c("PreK", "Kindergarten", "1st Grade", "2nd Grade"),
  columns    = list(
    widget_col("school", "school_picker", "School", min_width = 300),
    widget_col("attendance", "attendance_picker", "Attendance", width = 200),
    widget_col("notes", "notes_input", "Notes", min_width = 150)
  )
)

ui <- fluidPage(
  config_table_ui("history", cfg)
)

server <- function(input, output, session) {
  config_table_server("history", cfg,
    data_r    = reactive(NULL),
    search_fn = function(query, grade_key) {
      # Return a data frame of matching schools
      data.frame(name = "Example School", nces_id = "123456")
    }
  )
}

shinyApp(ui, server)
```

## Using picker widgets standalone

You don’t have to use the table modules. Every picker widget works as a
standalone Shiny input.

``` r

ui <- fluidPage(
  useReactablePlus(),
  schoolPickerInput("my_school", grade_key = "PK",
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

## Embedding widgets inside reactable

When placing widgets inside `reactable::colDef(cell = ...)`, two things
are required:

1.  Call
    [`useReactablePlus()`](https://matthewcurrier.github.io/reactablePlus/reference/useReactablePlus.md)
    in the UI to load JS/CSS at the page level (reactable’s React
    renderer doesn’t process `htmlDependency` objects).

2.  Wrap the reactable widget with
    [`bindPickersOnRender()`](https://matthewcurrier.github.io/reactablePlus/reference/bindPickersOnRender.md)
    to initialize Shiny input bindings after the table DOM is created.

``` r

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

## Next steps

- Browse the
  [Reference](https://matthewcurrier.github.io/reactablePlus/articles/reference/index.md)
  for full documentation of every function.
- See the [Config-Driven Table
  Module](https://matthewcurrier.github.io/reactablePlus/articles/reference/config_table_ui.md)
  docs for mutual exclusion, fill-down, and gear toggle configuration.
- See the [Raw-Spec Table
  Module](https://matthewcurrier.github.io/reactablePlus/articles/reference/editable_table_ui.md)
  docs for gating, selection, and reset behavior.
