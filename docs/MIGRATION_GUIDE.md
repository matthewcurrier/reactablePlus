# Migration Guide: schoolhistory Downstream Package

## Overview

With the creation of `reactablePlus`, the `schoolhistory` package
becomes a thin domain layer that `Imports: reactablePlus`. It retains
only the school-history-specific configuration and demo apps.

## Files That Stay in schoolhistory

| File                      | Purpose                                 |
|---------------------------|-----------------------------------------|
| `school-history-preset.R` | `school_history_config()`, wrappers     |
| `demo-attendance.R`       | Standalone attendance demo app          |
| `demo-school.R`           | Standalone school picker demo           |
| `demo-combined.R`         | Combined demo                           |
| `demo-diagnostic.R`       | Diagnostic demo                         |
| `demo-generic.R`          | Generic editable table demo             |
| `demo-w-mod.R`            | Full module demo (`mod_school_history`) |
| `app.R`                   | Default Shiny app entry point           |
| `testapp.R`               | Test app for shinytest2                 |
| `test-shinytest2.R`       | Integration tests                       |

## Required Renames in schoolhistory Code

### Function renames

| Old name | New name (from reactablePlus) |
|----|----|
| `col_spec()` | [`widget_col()`](https://matthewcurrier.github.io/reactablePlus/reference/widget_col.md) |
| `editable_table_ui(id, config)` | `config_table_ui(id, config)` |
| [`editable_table_server()`](https://matthewcurrier.github.io/reactablePlus/reference/editable_table_server.md) | [`config_table_server()`](https://matthewcurrier.github.io/reactablePlus/reference/config_table_server.md) |
| [`useSchoolHistory()`](https://matthewcurrier.github.io/reactablePlus/reference/useReactablePlus.md) | [`useReactablePlus()`](https://matthewcurrier.github.io/reactablePlus/reference/useReactablePlus.md) |

### In `school-history-preset.R`

``` r

# BEFORE
school_history_config <- function() {
  table_config(
    columns = list(
      col_spec("school", "school_picker", "School", ...),
      col_spec("attendance", "attendance_picker", "Attendance", ...),
      ...
    ),
    ...
  )
}

school_history_ui <- function(id) {
  editable_table_ui(id, school_history_config())
}

school_history_server <- function(id, ...) {
  editable_table_server(id, school_history_config(), ...)
}

# AFTER
school_history_config <- function() {
  table_config(
    columns = list(
      widget_col("school", "school_picker", "School", ...),
      widget_col("attendance", "attendance_picker", "Attendance", ...),
      ...
    ),
    ...
  )
}

school_history_ui <- function(id) {
  config_table_ui(id, school_history_config())
}

school_history_server <- function(id, ...) {
  config_table_server(id, school_history_config(), ...)
}
```

### In demo apps

Replace all instances of: -
[`library(schoolhistory)`](https://rdrr.io/r/base/library.html) →
[`library(reactablePlus)`](https://github.com/OWNER/reactablePlus) (or
add both) -
[`useSchoolHistory()`](https://matthewcurrier.github.io/reactablePlus/reference/useReactablePlus.md)
→
[`useReactablePlus()`](https://matthewcurrier.github.io/reactablePlus/reference/useReactablePlus.md) -
`col_spec(...)` → `widget_col(...)` - `editable_table_ui(id, config)` →
`config_table_ui(id, config)` - `editable_table_server(...)` →
`config_table_server(...)`

### DESCRIPTION for schoolhistory

    Imports:
        reactablePlus,
        shiny,
        bslib,
        dplyr,
        reactable

## Phase 6: Integration Testing

After completing the renames, run the existing shinytest2 suite:

``` r

shinytest2::test_app("path/to/schoolhistory/testapp")
```

The tests in `test-shinytest2.R` should pass without modification once
the function renames are applied, since they test end-to-end behavior
rather than internal function names.
