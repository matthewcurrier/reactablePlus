# Homeschool picker input

Creates a homeschool picker widget. When empty, renders a dashed-border
trigger button. When filled, renders a pill summary with optional
provider and curriculum details.

## Usage

``` r
homeschoolPickerInput(
  inputId,
  value = NULL,
  grade_label = NULL,
  grade_key = NULL,
  providers = c("Mother", "Father", "Grandmother", "Grandfather", "Guardian", "Tutor",
    "Co-op", "Online program", "Other"),
  provider_label = "Who provided homeschooling?",
  curriculum_label = "Curriculum / program",
  curriculum_placeholder = "e.g. Time4Learning, Sonlight, custom",
  show_curriculum = TRUE,
  show_notes = TRUE,
  notes_placeholder = "Evaluator, co-op participation, any context…",
  trigger_label = "+ Mark homeschool",
  trigger_sub_label = "(details optional)",
  popover_title = "Homeschool details",
  popover_title_sub = "(optional)",
  filled_pill_label = "Homeschool",
  clear_label = "remove homeschool",
  ns = "",
  width = "100%"
)
```

## Arguments

- inputId:

  Character. The Shiny input ID. The value is a named list with `by`,
  `by_other`, `curriculum`, `notes` when active, or `NULL` when
  inactive.

- value:

  Optional initial value — a list with any combination of the fields
  above. Pass [`list()`](https://rdrr.io/r/base/list.html) for active
  with no details. Pass `NULL` (default) for inactive.

- grade_label:

  Optional display label for accessible naming.

- grade_key:

  The grade's two-char key (e.g. `"PK"`, `"03"`).

- providers:

  Character vector of provider dropdown options. If the last element is
  `"Other"`, an additional free-text input is shown when `"Other"` is
  selected. Default:
  `c("Mother", "Father", "Grandmother", "Grandfather", "Guardian", "Tutor", "Co-op", "Online program", "Other")`.

- provider_label:

  Character. Label above the provider dropdown. Default
  `"Who provided homeschooling?"`.

- curriculum_label:

  Character. Label above the curriculum text input. Default
  `"Curriculum / program"`.

- curriculum_placeholder:

  Character. Placeholder for the curriculum field. Default
  `"e.g. Time4Learning, Sonlight, custom"`.

- show_curriculum:

  Logical. Whether to include the curriculum field. Default `TRUE`.

- show_notes:

  Logical. Whether to include the notes textarea. Default `TRUE`.

- notes_placeholder:

  Character. Placeholder for the notes textarea. Default
  `"Evaluator, co-op participation, any context\u2026"`.

- trigger_label:

  Character. Text on the empty-state trigger button. Default
  `"+ Mark homeschool"`.

- trigger_sub_label:

  Character or `NULL`. Sub-label on the trigger button. Default
  `"(details optional)"`.

- popover_title:

  Character. Popover header text. Default `"Homeschool details"`.

- popover_title_sub:

  Character or `NULL`. Popover header sub-text. Default `"(optional)"`.

- filled_pill_label:

  Character. Text for the primary pill when active. Default
  `"Homeschool"`.

- clear_label:

  Character. Text for the footer clear/remove link. Default
  `"remove homeschool"`.

- ns:

  Shiny module namespace prefix. Default `""`.

- width:

  CSS width. Default `"100%"`.

## Value

A tag with attached dependencies.

## Details

Clicking the empty trigger immediately flags the row as active and opens
the details popover. The popover fields (provider, curriculum, notes)
are all optional.

The provider dropdown options, field labels, and trigger text are all
configurable, making this widget reusable for any "flag + details"
pattern (e.g. therapy provider tracking, tutoring arrangements).

## Examples

``` r
if (FALSE) { # \dontrun{
# Default school-history homeschool (backward compatible)
homeschoolPickerInput("hs_PK", grade_label = "PreK", grade_key = "PK")

# Pre-filled
homeschoolPickerInput("hs_K",
  value = list(by = "Mother", curriculum = "Time4Learning"),
  grade_label = "Kindergarten",
  grade_key = "K"
)

# Custom use: therapy provider tracking
homeschoolPickerInput("therapy_PK",
  providers = c("Occupational Therapist", "Speech Therapist",
                "Physical Therapist", "Behavioral Therapist", "Other"),
  provider_label = "Type of therapy",
  curriculum_label = "Provider / agency",
  curriculum_placeholder = "e.g. ABC Therapy Group",
  trigger_label = "+ Add therapy",
  trigger_sub_label = NULL,
  popover_title = "Therapy details",
  popover_title_sub = NULL,
  filled_pill_label = "Therapy",
  clear_label = "remove therapy",
  grade_label = "PreK",
  grade_key = "PK"
)
} # }
```
