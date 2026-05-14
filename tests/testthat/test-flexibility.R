# Tests that verify the widgets accept the parameters needed for
# extensibility and handle edge cases gracefully.

# ── SearchPicker flexibility ─────────────────────────────────────────────

test_that("searchPickerInput works with minimal arguments", {
  tag <- searchPickerInput("sp_min")
  html <- as.character(tag)
  expect_match(html, 'id="sp_min"')
  expect_match(html, 'data-initial-value="null"')
})

test_that("searchPickerInput accepts school with grade range fields", {
  val <- list(
    id = "123",
    name = "Test",
    district = "D",
    city = "C",
    state = "MA",
    type = "Public",
    low_grade = "K",
    high_grade = "08"
  )
  tag <- searchPickerInput("sp_range", value = val)
  html <- as.character(tag)
  expect_match(html, "low_grade")
  expect_match(html, "high_grade")
})

test_that("searchPickerInput handles NULL optional fields", {
  tag <- searchPickerInput(
    "sp_null",
    grade_label = NULL,
    grade_key = NULL,
    show_nces_id = TRUE,
    ns = ""
  )
  expect_match(as.character(tag), 'id="sp_null"')
})

test_that("searchPickerInput accepts custom trigger and popover labels", {
  tag <- searchPickerInput(
    "sp_labels",
    trigger_label = "+ Find provider",
    popover_title = "Search providers",
    search_placeholder = "Type provider name...",
    empty_hint = "Search our provider directory",
    no_match_hint = "No providers found."
  )
  html <- as.character(tag)
  expect_match(html, 'data-trigger-label="\\+ Find provider"')
  expect_match(html, 'data-popover-title="Search providers"')
  expect_match(html, 'data-search-placeholder="Type provider name\\.\\.\\."')
  expect_match(html, 'data-empty-hint="Search our provider directory"')
  expect_match(html, 'data-no-match-hint="No providers found\\."')
})

test_that("searchPickerInput supports show_fill_down = FALSE", {
  tag <- searchPickerInput("sp_nofill", show_fill_down = FALSE)
  html <- as.character(tag)
  expect_match(html, 'data-show-fill-down="false"')
})

test_that("searchPickerInput defaults show_fill_down to TRUE", {
  tag <- searchPickerInput("sp_fill")
  html <- as.character(tag)
  expect_match(html, 'data-show-fill-down="true"')
})


# ── AttendancePicker flexibility ─────────────────────────────────────────

test_that("attendancePickerInput handles partial values", {
  val <- list(school = "Excellent")
  tag <- attendancePickerInput("att_partial", value = val)
  html <- as.character(tag)
  expect_match(html, "Excellent")
})

test_that("attendancePickerInput handles all NULL fields", {
  val <- list(school = NULL, class_ = NULL, impacts = NULL, notes = NULL)
  tag <- attendancePickerInput("att_allnull", value = val)
  expect_match(as.character(tag), 'id="att_allnull"')
})

test_that("attendancePickerInput uses default sections when none provided", {
  tag <- attendancePickerInput("att_default")
  html <- as.character(tag)
  # Default sections should include the three standard keys
  expect_match(html, "data-sections")
  expect_match(html, "School Attendance")
  expect_match(html, "Class Attendance")
  expect_match(html, "impacts")
})

test_that("attendancePickerInput accepts custom sections", {
  custom_sections <- list(
    list(
      key = "behavior",
      label = "Classroom Behavior",
      levels = c("Exemplary", "Meets Expectations", "Needs Improvement")
    ),
    list(
      key = "participation",
      label = "Participation",
      levels = c("Active", "Moderate", "Minimal")
    )
  )
  tag <- attendancePickerInput("att_custom", sections = custom_sections)
  html <- as.character(tag)
  expect_match(html, "behavior")
  expect_match(html, "Classroom Behavior")
  expect_match(html, "Exemplary")
  expect_match(html, "participation")
  expect_match(html, "Active")
  # Should NOT contain default sections
  expect_false(grepl("School Attendance", html))
})

test_that("attendancePickerInput accepts custom trigger and popover labels", {
  tag <- attendancePickerInput(
    "att_labels",
    trigger_label = "+ Rate behavior",
    popover_title = "Behavior Rating"
  )
  html <- as.character(tag)
  expect_match(html, 'data-trigger-label="\\+ Rate behavior"')
  expect_match(html, 'data-popover-title="Behavior Rating"')
})

test_that("attendancePickerInput supports show_notes = FALSE", {
  tag <- attendancePickerInput("att_nonotes", show_notes = FALSE)
  html <- as.character(tag)
  expect_match(html, 'data-show-notes="false"')
})

test_that("attendancePickerInput accepts custom notes placeholder", {
  tag <- attendancePickerInput(
    "att_placeholder",
    notes_placeholder = "Context, observations..."
  )
  html <- as.character(tag)
  expect_match(html, 'data-notes-placeholder="Context, observations\\.\\.\\."')
})

test_that("attendancePickerInput sections with impact_display encode correctly", {
  sections <- list(
    list(
      key = "quality",
      label = "Quality",
      levels = c("High", "Low"),
      impact_display = list(High = "above standard", Low = "below standard")
    )
  )
  tag <- attendancePickerInput("att_impact", sections = sections)
  html <- as.character(tag)
  expect_match(html, "impact_display")
  expect_match(html, "above standard")
  expect_match(html, "below standard")
})

test_that("attendancePickerInput sections with pill_prefix encode correctly", {
  sections <- list(
    list(
      key = "effort",
      label = "Effort",
      levels = c("High", "Medium", "Low"),
      pill_prefix = "Effort: ",
      pill_icon = "pencil"
    )
  )
  tag <- attendancePickerInput("att_pill", sections = sections)
  html <- as.character(tag)
  expect_match(html, "pill_prefix")
  expect_match(html, "Effort: ")
  expect_match(html, "pencil")
})


# ── HomeschoolPicker flexibility ─────────────────────────────────────────

test_that("homeschoolPickerInput works with defaults", {
  tag <- homeschoolPickerInput("hs_def")
  html <- as.character(tag)
  expect_match(html, 'id="hs_def"')
  expect_match(html, "data-config")
  # Default providers should be in config
  expect_match(html, "Mother")
  expect_match(html, "Guardian")
})

test_that("homeschoolPickerInput accepts custom providers", {
  tag <- homeschoolPickerInput(
    "hs_prov",
    providers = c("Occupational Therapist", "Speech Therapist", "Other")
  )
  html <- as.character(tag)
  expect_match(html, "Occupational Therapist")
  expect_match(html, "Speech Therapist")
  # Default providers should NOT be present
  expect_false(grepl('"Mother"', html))
})

test_that("homeschoolPickerInput accepts custom labels", {
  tag <- homeschoolPickerInput(
    "hs_labels",
    trigger_label = "+ Add therapy",
    trigger_sub_label = NULL,
    popover_title = "Therapy details",
    popover_title_sub = NULL,
    filled_pill_label = "Therapy",
    clear_label = "remove therapy",
    provider_label = "Type of therapy",
    curriculum_label = "Provider / agency",
    curriculum_placeholder = "e.g. ABC Therapy Group"
  )
  html <- as.character(tag)
  expect_match(html, "Add therapy")
  expect_match(html, "Therapy details")
  expect_match(html, "Type of therapy")
  expect_match(html, "Provider / agency")
})

test_that("homeschoolPickerInput supports show_curriculum = FALSE", {
  tag <- homeschoolPickerInput("hs_nocurr", show_curriculum = FALSE)
  html <- as.character(tag)
  # The config JSON should have show_curriculum: false
  config_match <- regmatches(html, regexpr('data-config="[^"]*"', html))
  config_json <- gsub('data-config="|"$', '', config_match)
  config_json <- gsub("&quot;", '"', config_json)
  config <- jsonlite::fromJSON(config_json)
  expect_false(config$show_curriculum)
})

test_that("homeschoolPickerInput supports show_notes = FALSE", {
  tag <- homeschoolPickerInput("hs_nonotes", show_notes = FALSE)
  html <- as.character(tag)
  config_match <- regmatches(html, regexpr('data-config="[^"]*"', html))
  config_json <- gsub('data-config="|"$', '', config_match)
  config_json <- gsub("&quot;", '"', config_json)
  config <- jsonlite::fromJSON(config_json)
  expect_false(config$show_notes)
})

test_that("homeschoolPickerInput passes value correctly", {
  val <- list(by = "Mother", curriculum = "Time4Learning")
  tag <- homeschoolPickerInput("hs_val", value = val)
  html <- as.character(tag)
  expect_match(html, "Mother")
  expect_match(html, "Time4Learning")
})

test_that("homeschoolPickerInput encodes NULL value as homeschool-off", {
  tag <- homeschoolPickerInput("hs_null")
  html <- as.character(tag)
  expect_match(html, 'data-initial-value="null"')
})

test_that("homeschoolPickerInput encodes empty list as homeschool-on", {
  tag <- homeschoolPickerInput("hs_empty", value = list())
  html <- as.character(tag)
  expect_match(html, 'data-initial-value="\\{\\}"')
})

test_that("homeschoolPickerInput custom providers without Other omit Other logic", {
  tag <- homeschoolPickerInput(
    "hs_noother",
    providers = c("Parent", "Tutor", "Agency")
  )
  html <- as.character(tag)
  expect_match(html, "Parent")
  expect_match(html, "Tutor")
  expect_match(html, "Agency")
  # "Other" should not appear in the providers
  config_match <- regmatches(html, regexpr('data-config="[^"]*"', html))
  config_json <- gsub('data-config="|"$', '', config_match)
  config_json <- gsub("&quot;", '"', config_json)
  config <- jsonlite::fromJSON(config_json)
  expect_false("Other" %in% config$providers)
})


# ── Backward compatibility ───────────────────────────────────────────────

test_that("all widgets work with zero optional arguments (pure backward compat)", {
  # These calls mirror exactly how the widgets were used before Phase 1
  s <- searchPickerInput("compat_s", grade_key = "PK", grade_label = "PreK")
  a <- attendancePickerInput("compat_a", grade_label = "PreK")
  h <- homeschoolPickerInput("compat_h", grade_label = "PreK", grade_key = "PK")

  # All should produce valid HTML without errors
  expect_match(as.character(s), 'id="compat_s"')
  expect_match(as.character(a), 'id="compat_a"')
  expect_match(as.character(h), 'id="compat_h"')

  # School picker should have default labels
  s_html <- as.character(s)
  expect_match(s_html, 'data-trigger-label="\\+ Pick school"')
  expect_match(s_html, 'data-show-fill-down="true"')

  # Attendance picker should have default sections
  a_html <- as.character(a)
  expect_match(a_html, "School Attendance")

  # Homeschool picker should have default providers
  h_html <- as.character(h)
  expect_match(h_html, "Mother")
  expect_match(h_html, "Guardian")
})
