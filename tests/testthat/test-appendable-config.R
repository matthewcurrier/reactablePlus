# tests/testthat/test-appendable-config.R
#
# Phase 1 tests — appendable mode parameters in table_config().
# Covers: construction, validation, mutual exclusion with dynamic mode,
# min_rows / max_rows / allow_delete constraints, backward compatibility.


# ── Basic construction ────────────────────────────────────────────────────────

test_that("table_config appendable mode creates a valid config", {
  cfg <- table_config(
    appendable = TRUE,
    columns = list(
      widget_col("fruit", "dropdown", "Fruit",
        options = list(choices = c("Apple", "Banana"))
      )
    )
  )

  expect_s3_class(cfg, "table_config")
  expect_true(cfg$appendable)
  expect_false(cfg$dynamic)
  expect_equal(cfg$row_keys, character(0))
  expect_equal(cfg$row_labels, character(0))
})


test_that("table_config appendable mode stores defaults correctly", {
  cfg <- table_config(
    appendable = TRUE,
    columns = list(
      widget_col("x", "text", "X")
    )
  )

  expect_true(cfg$allow_delete)
  expect_equal(cfg$min_rows, 0L)
  expect_null(cfg$max_rows)
})


test_that("table_config appendable mode stores custom parameters", {
  cfg <- table_config(
    appendable = TRUE,
    allow_delete = FALSE,
    min_rows = 2L,
    max_rows = 10L,
    columns = list(
      widget_col("x", "text", "X")
    )
  )

  expect_false(cfg$allow_delete)
  expect_equal(cfg$min_rows, 2L)
  expect_equal(cfg$max_rows, 10L)
})


test_that("table_config appendable mode coerces numeric min/max to integer", {
  cfg <- table_config(
    appendable = TRUE,
    min_rows = 1,
    max_rows = 5,
    columns = list(
      widget_col("x", "text", "X")
    )
  )

  expect_identical(cfg$min_rows, 1L)
  expect_identical(cfg$max_rows, 5L)
})


test_that("table_config appendable mode with all widget types", {
  cfg <- table_config(
    appendable = TRUE,
    columns = list(
      widget_col("status", "dropdown", "Status",
        options = list(choices = c("Active", "Inactive"))
      ),
      widget_col("score", "numeric", "Score",
        options = list(min = 0, max = 100)
      ),
      widget_col("start", "date", "Start Date"),
      widget_col("active", "checkbox", "Active"),
      widget_col("enabled", "toggle", "Enabled"),
      widget_col("notes", "text", "Notes",
        options = list(placeholder = "Enter notes...")
      )
    )
  )

  expect_true(cfg$appendable)
  expect_length(cfg$columns, 6L)
})


# ── Mutual exclusion with other modes ─────────────────────────────────────────

test_that("table_config rejects appendable + dynamic mode", {
  expect_error(
    table_config(
      appendable = TRUE,
      row_id_col = "id",
      row_label_col = "name",
      columns = list(
        widget_col("x", "text", "X")
      )
    ),
    "mutually exclusive"
  )
})


test_that("table_config appendable = FALSE preserves static mode", {
  cfg <- table_config(
    appendable = FALSE,
    row_keys = c("r1", "r2"),
    row_labels = c("Row 1", "Row 2"),
    columns = list(
      widget_col("x", "text", "X")
    )
  )

  expect_false(cfg$appendable)
  expect_false(cfg$dynamic)
  expect_equal(cfg$row_keys, c("r1", "r2"))
})


test_that("table_config appendable = FALSE preserves dynamic mode", {
  cfg <- table_config(
    appendable = FALSE,
    row_id_col = "id",
    row_label_col = "name",
    columns = list(
      widget_col("x", "text", "X")
    )
  )

  expect_false(cfg$appendable)
  expect_true(cfg$dynamic)
})


# ── Appendable-specific parameter validation ──────────────────────────────────

test_that("table_config rejects negative min_rows", {
  expect_error(
    table_config(
      appendable = TRUE,
      min_rows = -1L,
      columns = list(widget_col("x", "text", "X"))
    ),
    "non-negative"
  )
})


test_that("table_config rejects NA min_rows", {
  expect_error(
    table_config(
      appendable = TRUE,
      min_rows = NA_integer_,
      columns = list(widget_col("x", "text", "X"))
    ),
    "non-negative"
  )
})


test_that("table_config rejects max_rows = 0", {
  expect_error(
    table_config(
      appendable = TRUE,
      max_rows = 0L,
      columns = list(widget_col("x", "text", "X"))
    ),
    "positive integer"
  )
})


test_that("table_config rejects max_rows < min_rows", {
  expect_error(
    table_config(
      appendable = TRUE,
      min_rows = 5L,
      max_rows = 3L,
      columns = list(widget_col("x", "text", "X"))
    ),
    "must be >= min_rows"
  )
})


test_that("table_config accepts max_rows = min_rows (fixed size)", {
  cfg <- table_config(
    appendable = TRUE,
    min_rows = 3L,
    max_rows = 3L,
    columns = list(widget_col("x", "text", "X"))
  )

  expect_equal(cfg$min_rows, 3L)
  expect_equal(cfg$max_rows, 3L)
})


test_that("table_config rejects non-logical allow_delete", {
  expect_error(
    table_config(
      appendable = TRUE,
      allow_delete = "yes",
      columns = list(widget_col("x", "text", "X"))
    ),
    "allow_delete must be TRUE or FALSE"
  )
})


test_that("table_config rejects NA allow_delete", {
  expect_error(
    table_config(
      appendable = TRUE,
      allow_delete = NA,
      columns = list(widget_col("x", "text", "X"))
    ),
    "allow_delete must be TRUE or FALSE"
  )
})


# ── Non-appendable configs ignore appendable params ───────────────────────────

test_that("static config stores appendable = FALSE and default values", {
  cfg <- table_config(
    row_keys = "r1",
    row_labels = "Row 1",
    columns = list(widget_col("x", "text", "X"))
  )

  expect_false(cfg$appendable)
  expect_false(cfg$allow_delete)
  expect_equal(cfg$min_rows, 0L)
  expect_null(cfg$max_rows)
})


test_that("dynamic config stores appendable = FALSE and default values", {
  cfg <- table_config(
    row_id_col = "id",
    row_label_col = "name",
    columns = list(widget_col("x", "text", "X"))
  )

  expect_false(cfg$appendable)
  expect_false(cfg$allow_delete)
  expect_equal(cfg$min_rows, 0L)
  expect_null(cfg$max_rows)
})


# ── Appendable composes with existing config features ─────────────────────────

test_that("table_config appendable + show_reset", {
  cfg <- table_config(
    appendable = TRUE,
    show_reset = TRUE,
    columns = list(widget_col("x", "text", "X"))
  )

  expect_true(cfg$appendable)
  expect_true(cfg$show_reset)
})


test_that("table_config appendable + selectable", {
  cfg <- table_config(
    appendable = TRUE,
    selectable = TRUE,
    columns = list(widget_col("x", "text", "X"))
  )

  expect_true(cfg$appendable)
  expect_true(cfg$selectable)
})


test_that("table_config appendable + gating", {
  cfg <- table_config(
    appendable = TRUE,
    columns = list(
      widget_col("status", "dropdown", "Status",
        options = list(choices = c("Active", "Inactive"))
      ),
      widget_col("score", "numeric", "Score",
        options = list(min = 0),
        gate = list(
          list(type = "value", col_id = "status", values = "Active")
        )
      )
    )
  )

  expect_true(cfg$appendable)
  # Gate wiring should still auto-mark the controller

  status_col <- cfg$columns[[1]]
  expect_true(status_col$triggers_rerender)
})


test_that("table_config appendable + gear_toggles", {
  cfg <- table_config(
    appendable = TRUE,
    gear_toggles = list(
      showNotes = list(label = "Notes", value = TRUE)
    ),
    columns = list(
      widget_col("notes", "text", "Notes", gear_toggle = "showNotes")
    )
  )

  expect_true(cfg$appendable)
  expect_equal(cfg$columns[[1]]$gear_toggle, "showNotes")
})


test_that("table_config appendable + to_output_fn", {
  output_fn <- function(row_state, row_key) {
    data.frame(
      key = row_key,
      value = row_state$x %||% "",
      stringsAsFactors = FALSE
    )
  }

  cfg <- table_config(
    appendable = TRUE,
    columns = list(widget_col("x", "text", "X")),
    to_output_fn = output_fn
  )

  expect_true(cfg$appendable)
  expect_identical(cfg$to_output_fn, output_fn)
})


test_that("table_config appendable + toolbar_stats_fn", {
  stats_fn <- function(rows, row_keys) {
    shiny::HTML(sprintf("<b>%d rows</b>", length(row_keys)))
  }

  cfg <- table_config(
    appendable = TRUE,
    columns = list(widget_col("x", "text", "X")),
    toolbar_stats_fn = stats_fn
  )

  expect_true(cfg$appendable)
  expect_identical(cfg$toolbar_stats_fn, stats_fn)
})


# ── Optional seeding with row_keys/row_labels ────────────────────────────────

test_that("table_config appendable allows optional seed row_keys", {
  cfg <- table_config(
    appendable = TRUE,
    row_keys = c("seed_1", "seed_2"),
    row_labels = c("Seed Row 1", "Seed Row 2"),
    columns = list(widget_col("x", "text", "X"))
  )

  expect_true(cfg$appendable)
  expect_equal(cfg$row_keys, c("seed_1", "seed_2"))
  expect_equal(cfg$row_labels, c("Seed Row 1", "Seed Row 2"))
})


test_that("table_config appendable defaults row_keys to character(0)", {
  cfg <- table_config(
    appendable = TRUE,
    columns = list(widget_col("x", "text", "X"))
  )

  expect_equal(cfg$row_keys, character(0))
  expect_equal(cfg$row_labels, character(0))
  expect_equal(cfg$label_map, stats::setNames(list(), character(0)))
})


# ── Backward compatibility — existing static/dynamic unchanged ────────────────

test_that("static mode still requires row_keys and row_labels", {
  expect_error(
    table_config(
      columns = list(widget_col("x", "text", "X"))
    ),
    "row_keys and row_labels are required"
  )
})


test_that("static config from before appendable still works", {
  cfg <- table_config(
    row_keys = c("PK", "K", "01"),
    row_labels = c("PreK", "K", "1st"),
    columns = list(
      widget_col("school", "search_picker", "School", min_width = 300)
    ),
    badge_col = "grade",
    badge_label = "Grade"
  )

  expect_s3_class(cfg, "table_config")
  expect_false(cfg$appendable)
  expect_false(cfg$dynamic)
  expect_equal(cfg$row_keys, c("PK", "K", "01"))
})


test_that("dynamic config from before appendable still works", {
  cfg <- table_config(
    row_id_col = "student_id",
    row_label_col = "student_name",
    columns = list(
      widget_col("grade", "dropdown", "Grade",
        options = list(choices = c("A", "B", "C"))
      )
    )
  )

  expect_s3_class(cfg, "table_config")
  expect_false(cfg$appendable)
  expect_true(cfg$dynamic)
  expect_equal(cfg$row_keys, character(0))
})
