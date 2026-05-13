# helper-fixtures.R
# Sourced automatically by testthat before every test file.
# All fixtures return fresh objects so tests cannot mutate shared state.

valid_row_spec <- function() {
  list(
    id_col       = "grade_id",
    display_cols = list(
      list(col_name = "grade_label", label = "Grade")
    )
  )
}

selectable_row_spec <- function() {
  list(
    id_col       = "grade_id",
    selectable   = TRUE,
    display_cols = list(
      list(col_name = "grade_label", label = "Grade")
    )
  )
}

# ── Gating fixtures ───────────────────────────────────────────────────────────

# Value-only gate: months_offset unlocks when progress is "early" or "late".
value_gate_col_spec <- function() {
  list(
    list(
      col_name = "progress",
      label    = "Progress",
      type     = "dropdown",
      choices  = list(
        list(label = "Early",   value = "early"),
        list(label = "On Time", value = "on_time"),
        list(label = "Late",    value = "late")
      )
    ),
    list(
      col_name = "months_offset",
      label    = "+/- Months",
      type     = "numeric",
      min      = -24L,
      gate     = list(
        list(type = "value", col_name = "progress", values = c("early", "late"))
      )
    )
  )
}

# Selection-only gate: progress unlocks only when the row is selected.
selection_gate_col_spec <- function() {
  list(
    list(
      col_name = "progress",
      label    = "Progress",
      type     = "dropdown",
      choices  = list(
        list(label = "Early",   value = "early"),
        list(label = "On Time", value = "on_time"),
        list(label = "Late",    value = "late")
      ),
      gate = list(
        list(type = "selected")
      )
    )
  )
}

# Chained gate: progress unlocks on selection; months_offset and notes unlock
# on BOTH selection AND progress being "early" or "late".
chained_gate_col_spec <- function() {
  list(
    list(
      col_name = "progress",
      label    = "Progress",
      type     = "dropdown",
      choices  = list(
        list(label = "Early",   value = "early"),
        list(label = "On Time", value = "on_time"),
        list(label = "Late",    value = "late")
      ),
      gate = list(
        list(type = "selected")
      )
    ),
    list(
      col_name = "months_offset",
      label    = "+/- Months",
      type     = "numeric",
      min      = -24L,
      gate     = list(
        list(type = "selected"),
        list(type = "value", col_name = "progress", values = c("early", "late"))
      )
    ),
    list(
      col_name  = "notes",
      label     = "Notes",
      type      = "text",
      max_chars = 200L,
      gate      = list(
        list(type = "selected"),
        list(type = "value", col_name = "progress", values = c("early", "late"))
      )
    )
  )
}

# Input dfs for gating tests
value_gate_unlocked_df <- function() {
  tibble::tibble(
    grade_id      = c(6L, 7L, 8L),
    progress      = c("early", "late", "early"),
    months_offset = c(-2L, 3L, -1L)
  )
}

value_gate_mixed_df <- function() {
  tibble::tibble(
    grade_id      = c(6L, 7L, 8L),
    progress      = c("early", "on_time", "late"),
    months_offset = c(-2L, 5L, 1L)
  )
}

valid_col_spec <- function() {
  list(
    list(
      col_name = "attendance",
      label    = "Attendance",
      type     = "dropdown",
      choices  = list(
        list(label = "Good",         value = "good"),
        list(label = "Satisfactory", value = "satisfactory"),
        list(label = "Bad",          value = "bad")
      )
    ),
    list(
      col_name = "suspensions",
      label    = "Suspensions",
      type     = "numeric",
      min      = 0L
    )
  )
}

valid_grades_df <- function() {
  tibble::tibble(
    grade_id    = c(6L, 7L, 8L),
    grade_label = c("6th", "7th", "8th")
  )
}

# Partial existing data: only grades 6 and 7 have been saved previously.
# Grade 8 is intentionally absent to exercise the NA-fill path.
valid_existing_data <- function() {
  tibble::tibble(
    grade_id    = c(6L, 7L),
    attendance  = c("good", "satisfactory"),
    suspensions = c(0L, 2L)
  )
}

# A fully populated, valid input data frame (all three grades filled in).
valid_inputs_df <- function() {
  tibble::tibble(
    grade_id    = c(6L, 7L, 8L),
    attendance  = c("good", "satisfactory", "bad"),
    suspensions = c(0L, 2L, 1L)
  )
}

# ── New-type fixtures ─────────────────────────────────────────────────────────

valid_date_col_spec_entry <- function() {
  list(
    col_name = "enrollment_date",
    label    = "Enrollment Date",
    type     = "date"
  )
}

valid_date_col_spec_entry_with_bounds <- function() {
  list(
    col_name = "enrollment_date",
    label    = "Enrollment Date",
    type     = "date",
    min_date = "2020-01-01",
    max_date = "2030-12-31"
  )
}

valid_checkbox_col_spec_entry <- function() {
  list(
    col_name = "has_iep",
    label    = "Has IEP",
    type     = "checkbox"
  )
}

valid_toggle_col_spec_entry <- function() {
  list(
    col_name = "active",
    label    = "Active",
    type     = "toggle"
  )
}

valid_text_col_spec_entry <- function() {
  list(
    col_name = "notes",
    label    = "Notes",
    type     = "text"
  )
}

valid_text_col_spec_entry_with_max <- function() {
  list(
    col_name  = "notes",
    label     = "Notes",
    type      = "text",
    max_chars = 200L
  )
}

# A col_spec containing all six supported types for integration-style tests.
valid_full_col_spec <- function() {
  list(
    list(
      col_name = "attendance",
      label    = "Attendance",
      type     = "dropdown",
      choices  = list(
        list(label = "Good",         value = "good"),
        list(label = "Satisfactory", value = "satisfactory"),
        list(label = "Bad",          value = "bad")
      )
    ),
    list(col_name = "suspensions",     label = "Suspensions",     type = "numeric",  min = 0L),
    list(col_name = "enrollment_date", label = "Enrollment Date", type = "date"),
    list(col_name = "has_iep",         label = "Has IEP",         type = "checkbox"),
    list(col_name = "active",          label = "Active",          type = "toggle"),
    list(col_name = "notes",           label = "Notes",           type = "text")
  )
}

# valid_inputs_df for the full col_spec
valid_full_inputs_df <- function() {
  tibble::tibble(
    grade_id        = c(6L, 7L, 8L),
    attendance      = c("good", "satisfactory", "bad"),
    suspensions     = c(0L, 2L, 1L),
    enrollment_date = as.Date(c("2023-09-01", "2023-09-01", "2023-09-01")),
    has_iep         = c(TRUE, FALSE, TRUE),
    active          = c(TRUE, TRUE, FALSE),
    notes           = c("Good student", "", "Needs support")
  )
}
