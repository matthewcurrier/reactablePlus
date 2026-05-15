# Package index

## Setup

Load widget dependencies and wire picker bindings inside reactable.

- [`useReactablePlus()`](https://matthewcurrier.github.io/reactablePlus/reference/useReactablePlus.md)
  [`useSchoolHistory()`](https://matthewcurrier.github.io/reactablePlus/reference/useReactablePlus.md)
  : Include all reactablePlus widget dependencies
- [`popoverDep()`](https://matthewcurrier.github.io/reactablePlus/reference/popoverDep.md)
  : Popover core dependency (shared JS + CSS)
- [`bindPickersOnRender()`](https://matthewcurrier.github.io/reactablePlus/reference/bindPickersOnRender.md)
  : Bind picker inputs after a reactable renders
- [`runExample()`](https://matthewcurrier.github.io/reactablePlus/reference/runExample.md)
  : Run a reactablePlus example app

## Picker Widgets

Input constructors and update functions for inline cell editors. Each
widget has a Shiny input binding, so its value is available via
`input$<id>` on the server side.

- [`searchPickerInput()`](https://matthewcurrier.github.io/reactablePlus/reference/searchPickerInput.md)
  [`schoolPickerInput()`](https://matthewcurrier.github.io/reactablePlus/reference/searchPickerInput.md)
  : Search picker input
- [`updateSearchPickerInput()`](https://matthewcurrier.github.io/reactablePlus/reference/updateSearchPickerInput.md)
  [`updateSchoolPickerInput()`](https://matthewcurrier.github.io/reactablePlus/reference/updateSearchPickerInput.md)
  : Update a search picker from the server
- [`useTypeaheadSearch()`](https://matthewcurrier.github.io/reactablePlus/reference/useTypeaheadSearch.md)
  [`useSchoolSearch()`](https://matthewcurrier.github.io/reactablePlus/reference/useTypeaheadSearch.md)
  : Wire up server-side typeahead search
- [`attendancePickerInput()`](https://matthewcurrier.github.io/reactablePlus/reference/attendancePickerInput.md)
  : Attendance picker input
- [`updateAttendancePickerInput()`](https://matthewcurrier.github.io/reactablePlus/reference/updateAttendancePickerInput.md)
  : Update an attendance picker from the server
- [`homeschoolPickerInput()`](https://matthewcurrier.github.io/reactablePlus/reference/homeschoolPickerInput.md)
  : Homeschool picker input
- [`updateHomeschoolPickerInput()`](https://matthewcurrier.github.io/reactablePlus/reference/updateHomeschoolPickerInput.md)
  : Update a homeschool picker from the server
- [`notesInput()`](https://matthewcurrier.github.io/reactablePlus/reference/notesInput.md)
  : Notes text input
- [`updateNotesInput()`](https://matthewcurrier.github.io/reactablePlus/reference/updateNotesInput.md)
  : Update a notes input from the server
- [`gearPopoverInput()`](https://matthewcurrier.github.io/reactablePlus/reference/gearPopoverInput.md)
  : Gear popover settings input
- [`updateGearPopoverInput()`](https://matthewcurrier.github.io/reactablePlus/reference/updateGearPopoverInput.md)
  : Update a gear popover from the server

## Config-Driven Table Module

A Shiny module that renders an editable table from a declarative
`table_config` object. Handles widget dispatch, mutual exclusion,
fill-down, gear toggles, and data marshaling.

- [`config_table_ui()`](https://matthewcurrier.github.io/reactablePlus/reference/config_table_ui.md)
  : Config-driven Editable Table UI
- [`config_table_server()`](https://matthewcurrier.github.io/reactablePlus/reference/config_table_server.md)
  : Config-driven Editable Table Server
- [`table_config()`](https://matthewcurrier.github.io/reactablePlus/reference/table_config.md)
  : Build a table configuration
- [`widget_col()`](https://matthewcurrier.github.io/reactablePlus/reference/widget_col.md)
  : Build a widget column specification
- [`normalize_choices()`](https://matthewcurrier.github.io/reactablePlus/reference/normalize_choices.md)
  : Normalize dropdown choices to list-of-lists format

## Grade Utilities

Helper functions for working with grade-level keys (PK through 12).
Convenience utilities for education-domain applications; not required by
the core table infrastructure.

- [`gradeChoices()`](https://matthewcurrier.github.io/reactablePlus/reference/gradeChoices.md)
  [`gradeLabelMap()`](https://matthewcurrier.github.io/reactablePlus/reference/gradeChoices.md)
  : Grade choices and labels
- [`gradeIndex()`](https://matthewcurrier.github.io/reactablePlus/reference/gradeIndex.md)
  : Grade ordering index
- [`gradeInRange()`](https://matthewcurrier.github.io/reactablePlus/reference/gradeInRange.md)
  : Check if a grade falls within a school's grade range
