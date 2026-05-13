# reactablePlus

Enhanced interactive tables with inline editors for Shiny.

## Overview

`reactablePlus` extends [reactable](https://glin.github.io/reactable/)
with inline editing widgets and a declarative configuration layer for
building editable tables in Shiny applications.

### Widget library

- **SchoolPicker** — typeahead school search with popover
- **AttendancePicker** — multi-section attendance rating popover
- **HomeschoolPicker** — homeschool details popover
- **NotesInput** — inline text input with Shiny binding
- **GearPopover** — settings gear icon with toggle switches

### Generic editable table module

A Shiny module (`editable_table_ui` / `editable_table_server`) driven by
declarative `row_spec`, `col_spec`, and `table_config` objects. The
module handles rendering, input collection, validation, selection
gating, reset, and data marshaling without any domain-specific
knowledge.

## Installation

``` r

# install.packages("pak")
pak::pak("OWNER/reactablePlus")
```

## Quick start

``` r

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
