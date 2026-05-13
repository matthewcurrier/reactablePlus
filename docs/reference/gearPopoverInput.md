# Gear popover settings input

Creates a gear icon button that opens a settings popover with
configurable toggle switches. Returns a named list of boolean values to
Shiny.

## Usage

``` r
gearPopoverInput(inputId, toggles = list())
```

## Arguments

- inputId:

  Character. The Shiny input ID.

- toggles:

  A named list of toggle definitions. Each element should be a list with
  `label` (character), `desc` (character, optional description), and
  `value` (logical, default state). The names become the keys in the
  returned value.

## Value

A tag with attached dependencies.

## Examples

``` r
if (FALSE) { # \dontrun{
gearPopoverInput("settings", toggles = list(
  show_year = list(
    label = "Show School Year column",
    desc  = "Display the school-year column in the table.",
    value = TRUE
  ),
  show_nces_id = list(
    label = "Show NCES ID",
    desc  = "Display the federal school identifier inline.",
    value = TRUE
  ),
  compact_rows = list(
    label = "Compact rows",
    desc  = "Tighten row height; hide secondary metadata.",
    value = FALSE
  ),
  show_homeschool = list(
    label = "Show Homeschool column",
    desc  = "Add a column to mark homeschool grades and capture details.",
    value = FALSE
  )
))
} # }
```
