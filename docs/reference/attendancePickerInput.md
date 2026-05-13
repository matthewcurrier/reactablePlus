# Attendance picker input

Creates an attendance picker widget that renders as a dashed-border
trigger button when empty and a pill summary when filled. Clicking opens
a popover with configurable radio groups and an optional notes textarea.

## Usage

``` r
attendancePickerInput(
  inputId,
  value = NULL,
  grade_label = NULL,
  sections = NULL,
  trigger_label = "+ Mark attendance",
  popover_title = "Attendance",
  show_notes = TRUE,
  notes_placeholder = "Absences, tardies, context for this school year…",
  width = "100%"
)
```

## Arguments

- inputId:

  Character. The Shiny input ID. The value returned to the server is a
  named list keyed by each section's `key`, plus `notes` if notes are
  enabled, or `NULL` if nothing is set.

- value:

  Optional initial value — a named list keyed by section keys with
  character values.

- grade_label:

  Optional display label for this row's grade (e.g. `"3rd grade"`). Used
  in the trigger's `aria-label` for screen reader context.

- sections:

  A list of section definitions for the popover body. Each section is a
  list with:

  key

  :   Character. The key in the returned value list (e.g. `"school"`,
      `"class_"`).

  label

  :   Character. The heading above the radio group.

  levels

  :   Character vector. The radio options.

  pill_prefix

  :   Character (optional). Text prepended to the pill display when this
      section is filled (e.g. `"Class: "` produces
      `"Class: Excellent"`). Default is no prefix — the raw level is
      shown.

  pill_icon

  :   Character (optional). Pill icon style: `"school"` (default for
      first section), `"pencil"`, or `NULL` for no icon.

  impact_display

  :   List (optional). If present, renders a sub-line instead of a pill
      for this section. Should have named entries mapping level values
      to display strings, e.g.
      `list(Yes = "\u26A0 impacts academics", No = "no academic impact")`.

  Defaults to the school history attendance sections (School Attendance,
  Class Attendance, Impacts Academics).

- trigger_label:

  Character. Text on the empty-state trigger button. Default
  `"+ Mark attendance"`.

- popover_title:

  Character. Popover header text. Default `"Attendance"`.

- show_notes:

  Logical. Whether to include a notes textarea in the popover. Default
  `TRUE`.

- notes_placeholder:

  Character. Placeholder for the notes textarea. Default
  `"Absences, tardies, context for this school year\u2026"`.

- width:

  CSS width. Default `"100%"`.

## Value

An
[htmltools::tagList](https://rstudio.github.io/htmltools/reference/tagList.html)
suitable for use in Shiny UI, including inside
`reactable::colDef(cell = ...)` renderers.

## Details

The popover's radio sections are fully configurable via the `sections`
parameter, making this widget reusable for any multi-field rating
scenario (behavior tracking, therapy progress, etc.).

## Examples

``` r
if (FALSE) { # \dontrun{
# Default school-history attendance (backward compatible)
attendancePickerInput("att_pk", grade_label = "PreK")

# Pre-filled
attendancePickerInput("att_k",
  value = list(school = "Good", class_ = "Excellent", impacts = "No"),
  grade_label = "Kindergarten"
)

# Custom sections for behavior tracking
attendancePickerInput("beh_pk",
  sections = list(
    list(key = "behavior", label = "Classroom Behavior",
         levels = c("Exemplary", "Meets Expectations",
                    "Needs Improvement", "Unsatisfactory")),
    list(key = "participation", label = "Participation",
         levels = c("Active", "Moderate", "Minimal"))
  ),
  trigger_label = "+ Rate behavior",
  popover_title = "Behavior",
  notes_placeholder = "Context, observations\u2026"
)
} # }

```
