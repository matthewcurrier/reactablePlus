# ── schoolPickerInput ─────────────────────────────────────────────────────

test_that("schoolPickerInput produces a div with correct class", {
  tag <- searchPickerInput("test_id", grade_key = "PK")
  html <- as.character(tag)
  expect_match(html, 'class="sh-school-picker pv-container"')
  expect_match(html, 'id="test_id"')
})

test_that("schoolPickerInput includes grade key data attribute", {
  tag <- searchPickerInput("s1", grade_key = "05", grade_label = "5th grade")
  html <- as.character(tag)
  expect_match(html, 'data-grade-key="05"')
  expect_match(html, 'data-grade-label="5th grade"')
})

test_that("schoolPickerInput encodes initial value as JSON", {
  val <- list(id = "123", name = "Test School", city = "Boston", state = "MA")
  tag <- searchPickerInput("s2", value = val)
  html <- as.character(tag)
  expect_match(html, "data-initial-value=")
  # JSON should contain the school name
  expect_match(html, "Test School")
})

test_that("schoolPickerInput sets null initial value when empty", {
  tag <- searchPickerInput("s3")
  html <- as.character(tag)
  expect_match(html, 'data-initial-value="null"')
})

test_that("schoolPickerInput sets show-nces-id attribute", {
  tag_on  <- searchPickerInput("s4", show_nces_id = TRUE)
  tag_off <- searchPickerInput("s5", show_nces_id = FALSE)
  expect_match(as.character(tag_on),  'data-show-nces-id="true"')
  expect_match(as.character(tag_off), 'data-show-nces-id="false"')
})

test_that("schoolPickerInput has attached dependencies", {
  tag <- searchPickerInput("s6")
  deps <- htmltools::htmlDependencies(tag)
  dep_names <- vapply(deps, `[[`, character(1), "name")
  expect_true("popover-core" %in% dep_names)
  expect_true("school-picker" %in% dep_names)
})

test_that("schoolPickerInput passes namespace attribute", {
  tag <- searchPickerInput("s7", ns = "mymodule-")
  html <- as.character(tag)
  expect_match(html, 'data-ns="mymodule-"')
})


# ── attendancePickerInput ────────────────────────────────────────────────

test_that("attendancePickerInput produces a div with correct class", {
  tag <- attendancePickerInput("att1")
  html <- as.character(tag)
  expect_match(html, 'class="sh-attendance-picker pv-container"')
  expect_match(html, 'id="att1"')
})

test_that("attendancePickerInput encodes pre-filled value", {
  val <- list(school = "Good", class_ = "Excellent", impacts = "No")
  tag <- attendancePickerInput("att2", value = val)
  html <- as.character(tag)
  expect_match(html, "Good")
  expect_match(html, "Excellent")
})

test_that("attendancePickerInput sets empty JSON when no value", {
  tag <- attendancePickerInput("att3")
  html <- as.character(tag)
  expect_match(html, 'data-initial-value="\\{\\}"')
})

test_that("attendancePickerInput has attached dependencies", {
  tag <- attendancePickerInput("att4")
  deps <- htmltools::htmlDependencies(tag)
  dep_names <- vapply(deps, `[[`, character(1), "name")
  expect_true("popover-core" %in% dep_names)
  expect_true("attendance-picker" %in% dep_names)
})

test_that("attendancePickerInput includes grade label", {
  tag <- attendancePickerInput("att5", grade_label = "3rd grade")
  html <- as.character(tag)
  expect_match(html, 'data-grade-label="3rd grade"')
})


# ── homeschoolPickerInput ────────────────────────────────────────────────

test_that("homeschoolPickerInput produces a div with correct class", {
  tag <- homeschoolPickerInput("hs1")
  html <- as.character(tag)
  expect_match(html, 'class="sh-homeschool-picker pv-container"')
})

test_that("homeschoolPickerInput encodes NULL as homeschool-off", {
  tag <- homeschoolPickerInput("hs2")
  html <- as.character(tag)
  expect_match(html, 'data-initial-value="null"')
})

test_that("homeschoolPickerInput encodes empty list as homeschool-on", {
  tag <- homeschoolPickerInput("hs3", value = list())
  html <- as.character(tag)
  # Empty list serializes to {} in JSON
  expect_match(html, 'data-initial-value="\\{\\}"')
})

test_that("homeschoolPickerInput encodes details", {
  val <- list(by = "Mother", curriculum = "Time4Learning")
  tag <- homeschoolPickerInput("hs4", value = val)
  html <- as.character(tag)
  expect_match(html, "Mother")
  expect_match(html, "Time4Learning")
})

test_that("homeschoolPickerInput has attached dependencies", {
  tag <- homeschoolPickerInput("hs5")
  deps <- htmltools::htmlDependencies(tag)
  dep_names <- vapply(deps, `[[`, character(1), "name")
  expect_true("popover-core" %in% dep_names)
  expect_true("homeschool-picker" %in% dep_names)
})


# ── notesInput ───────────────────────────────────────────────────────────

test_that("notesInput produces a div with correct class", {
  tag <- notesInput("n1")
  html <- as.character(tag)
  expect_match(html, 'class="sh-notes-input"')
  expect_match(html, 'id="n1"')
})

test_that("notesInput sets default placeholder", {
  tag <- notesInput("n2")
  html <- as.character(tag)
  expect_match(html, 'data-placeholder="Optional"')
})

test_that("notesInput accepts custom placeholder", {
  tag <- notesInput("n3", placeholder = "Enter curriculum details")
  html <- as.character(tag)
  expect_match(html, 'data-placeholder="Enter curriculum details"')
})

test_that("notesInput sets initial value", {
  tag <- notesInput("n4", value = "Some notes here")
  html <- as.character(tag)
  expect_match(html, 'data-initial-value="Some notes here"')
})

test_that("notesInput has attached dependencies", {
  tag <- notesInput("n5")
  deps <- htmltools::htmlDependencies(tag)
  dep_names <- vapply(deps, `[[`, character(1), "name")
  expect_true("popover-core" %in% dep_names)
  expect_true("notes-input" %in% dep_names)
})


# ── gearPopoverInput ────────────────────────────────────────────────────

test_that("gearPopoverInput produces a div with correct class", {
  tag <- gearPopoverInput("g1", toggles = list())
  html <- as.character(tag)
  expect_match(html, 'class="sh-gear-wrap"')
  expect_match(html, 'id="g1"')
})

test_that("gearPopoverInput serializes toggles as JSON", {
  tag <- gearPopoverInput("g2", toggles = list(
    show_year = list(label = "Show Year", value = TRUE),
    compact   = list(label = "Compact", desc = "Tighten rows", value = FALSE)
  ))
  html <- as.character(tag)
  expect_match(html, "show_year")
  expect_match(html, "Show Year")
  expect_match(html, "compact")
  expect_match(html, "Tighten rows")
})

test_that("gearPopoverInput preserves toggle default values in JSON", {
  tag <- gearPopoverInput("g3", toggles = list(
    a = list(label = "Alpha", value = TRUE),
    b = list(label = "Beta", value = FALSE)
  ))
  html <- as.character(tag)
  # The JSON should contain both true and false values
  expect_match(html, "true")
  expect_match(html, "false")
})

test_that("gearPopoverInput has attached dependencies", {
  tag <- gearPopoverInput("g4")
  deps <- htmltools::htmlDependencies(tag)
  dep_names <- vapply(deps, `[[`, character(1), "name")
  expect_true("popover-core" %in% dep_names)
  expect_true("gear-popover" %in% dep_names)
})

test_that("gearPopoverInput accepts empty toggles list", {
  tag <- gearPopoverInput("g5", toggles = list())
  html <- as.character(tag)
  expect_match(html, 'data-toggles="\\[\\]"')
})
