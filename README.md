# reactablePlus

Enhanced interactive tables with inline editors for Shiny.

## Overview

`reactablePlus` extends [reactable](https://glin.github.io/reactable/) with
inline editing widgets and a declarative configuration layer for building
editable tables in Shiny applications.

### Widget library

- **SchoolPicker** — typeahead school search with popover
- **AttendancePicker** — multi-section attendance rating popover
- **HomeschoolPicker** — homeschool details popover
- **NotesInput** — inline text input with Shiny binding
- **GearPopover** — settings gear icon with toggle switches

### Config-driven editable table module

A Shiny module (`config_table_ui` / `config_table_server`) driven by
declarative `table_config` and `widget_col` objects. The module handles
rendering, input collection, validation, selection gating, reset, and data
marshaling without any domain-specific knowledge.

Three row modes are supported:

- **Static** — rows fixed at config time via `row_keys` / `row_labels`
- **Dynamic** — rows derived from a reactive `source_data` data frame, with
  state preservation across changes
- **Appendable** — users add and remove rows at runtime through Add Row /
  Delete buttons, with configurable `min_rows` and `max_rows` constraints

## Installation

```r
# install.packages("pak")
pak::pak("OWNER/reactablePlus")
```

## Quick start

```r
library(shiny)
library(reactablePlus)

ui <- fluidPage(
  useReactablePlus(),
  reactableOutput("table")
)

server <- function(input, output, session) {
  output$table <- renderReactable({
    tbl <- reactable(my_data, columns = list(
      notes = colDef(html = TRUE, cell = function(value, index) {
        as.character(notesInput(paste0("notes_", index), value = value))
      })
    ))
    bindPickersOnRender(tbl)
  })
}

shinyApp(ui, server)
```

## License
MIT
