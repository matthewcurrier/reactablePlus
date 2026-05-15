# tests/testthat/test-dynamic-rows.R
#
# Tests for the dynamic rows extension to config_table.
# Covers: table_config() dynamic mode validation,
#         .build_empty_row_state(), .derive_source_keys_labels(),
#         .merge_dynamic_rows(), .rows_from_saved() with effective_keys,
#         .build_table_df() with effective_keys/labels.


# ── table_config: static mode backward compatibility ─────────────────────────

test_that("table_config static mode works unchanged", {
  cfg <- table_config(
    row_keys = c("r1", "r2"),
    row_labels = c("Row 1", "Row 2"),
    columns = list(
      widget_col("score", "numeric", "Score", options = list(min = 0))
    )
  )
  expect_s3_class(cfg, "table_config")
  expect_false(cfg$dynamic)
  expect_null(cfg$row_id_col)
  expect_equal(cfg$row_keys, c("r1", "r2"))
  expect_equal(cfg$row_labels, c("Row 1", "Row 2"))
})


test_that("table_config static mode requires row_keys and row_labels", {
  expect_error(
    table_config(
      columns = list(
        widget_col("score", "numeric", "Score", options = list(min = 0))
      )
    ),
    "row_keys and row_labels are required"
  )

  expect_error(
    table_config(
      row_keys = c("r1"),
      columns = list(
        widget_col("score", "numeric", "Score", options = list(min = 0))
      )
    ),
    "row_keys and row_labels are required"
  )
})


# ── table_config: dynamic mode ───────────────────────────────────────────────

test_that("table_config dynamic mode with row_label_col", {
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
  expect_true(cfg$dynamic)
  expect_equal(cfg$row_id_col, "student_id")
  expect_equal(cfg$row_label_col, "student_name")
  expect_null(cfg$row_label_fn)
  # Keys / labels default to empty

  expect_equal(cfg$row_keys, character(0))
  expect_equal(cfg$row_labels, character(0))
})


test_that("table_config dynamic mode with row_label_fn", {
  label_fn <- function(row) paste(row$first, row$last)
  cfg <- table_config(
    row_id_col = "id",
    row_label_fn = label_fn,
    columns = list(
      widget_col("status", "dropdown", "Status",
        options = list(choices = c("Active", "Inactive"))
      )
    )
  )
  expect_true(cfg$dynamic)
  expect_null(cfg$row_label_col)
  expect_identical(cfg$row_label_fn, label_fn)
})


test_that("table_config dynamic mode allows optional row_keys/labels", {
  cfg <- table_config(
    row_id_col = "id",
    row_label_col = "name",
    row_keys = c("seed_1"),
    row_labels = c("Seed Row"),
    columns = list(
      widget_col("x", "text", "X")
    )
  )
  expect_true(cfg$dynamic)
  expect_equal(cfg$row_keys, "seed_1")
  expect_equal(cfg$row_labels, "Seed Row")
})


test_that("table_config dynamic mode rejects missing label spec", {
  expect_error(
    table_config(
      row_id_col = "id",
      columns = list(widget_col("x", "text", "X"))
    ),
    "row_label_col or row_label_fn"
  )
})


test_that("table_config dynamic mode rejects both label specs", {
  expect_error(
    table_config(
      row_id_col = "id",
      row_label_col = "name",
      row_label_fn = function(r) r$name,
      columns = list(widget_col("x", "text", "X"))
    ),
    "not both"
  )
})


test_that("table_config dynamic mode validates row_id_col type", {
  expect_error(
    table_config(
      row_id_col = 42,
      row_label_col = "name",
      columns = list(widget_col("x", "text", "X"))
    )
  )

  expect_error(
    table_config(
      row_id_col = "",
      row_label_col = "name",
      columns = list(widget_col("x", "text", "X"))
    )
  )
})


# ── .build_empty_row_state ───────────────────────────────────────────────────

test_that(".build_empty_row_state produces correct structure", {
  cfg <- table_config(
    row_keys = c("r1"),
    row_labels = c("R1"),
    columns = list(
      widget_col("grade", "dropdown", "Grade",
        options = list(choices = c("A", "B"))
      ),
      widget_col("score", "numeric", "Score", options = list(min = 0)),
      widget_col("active", "checkbox", "Active")
    ),
    selectable = TRUE
  )

  row_state <- .build_empty_row_state(cfg, "r1")

  expect_type(row_state, "list")
  expect_equal(row_state$.row_key, "r1")
  expect_false(row_state$.selected)
  expect_true(is.na(row_state$grade))
  expect_equal(row_state$score, 0)
  expect_false(row_state$active)
})


test_that(".build_empty_row_state works for text columns", {
  cfg <- table_config(
    row_keys = "r1",
    row_labels = "R1",
    columns = list(
      widget_col("notes", "text", "Notes",
        options = list(placeholder = "Enter...")
      )
    )
  )

  row_state <- .build_empty_row_state(cfg, "r1")
  expect_equal(row_state$notes, "")
})


test_that(".build_empty_row_state with year column", {
  cfg <- table_config(
    row_keys = "r1",
    row_labels = "R1",
    columns = list(widget_col("x", "text", "X")),
    year_col = "school_year"
  )

  row_state <- .build_empty_row_state(cfg, "r1")
  expect_true(is.na(row_state$school_year))
  expect_true(is.integer(row_state$school_year))
})


# ── .build_empty_rows (refactored) ──────────────────────────────────────────

test_that(".build_empty_rows still works after refactor", {
  cfg <- table_config(
    row_keys = c("a", "b", "c"),
    row_labels = c("Alice", "Bob", "Carol"),
    columns = list(
      widget_col("score", "numeric", "Score", options = list(min = 0))
    )
  )

  rows <- .build_empty_rows(cfg)

  expect_type(rows, "list")
  expect_equal(names(rows), c("a", "b", "c"))
  expect_equal(rows$a$.row_key, "a")
  expect_equal(rows$b$score, 0)
})


# ── .derive_source_keys_labels ───────────────────────────────────────────────

test_that(".derive_source_keys_labels works with row_label_col", {
  cfg <- table_config(
    row_id_col = "id",
    row_label_col = "name",
    columns = list(widget_col("x", "text", "X"))
  )

  src <- data.frame(
    id = c("s1", "s2", "s3"),
    name = c("Alice", "Bob", "Carol"),
    email = c("a@x.com", "b@x.com", "c@x.com"),
    stringsAsFactors = FALSE
  )

  result <- .derive_source_keys_labels(cfg, src)

  expect_equal(result$keys, c("s1", "s2", "s3"))
  expect_equal(result$labels, c("Alice", "Bob", "Carol"))
})


test_that(".derive_source_keys_labels works with row_label_fn", {
  cfg <- table_config(
    row_id_col = "id",
    row_label_fn = function(row) paste(row$first, row$last),
    columns = list(widget_col("x", "text", "X"))
  )

  src <- data.frame(
    id = c("s1", "s2"),
    first = c("Alice", "Bob"),
    last = c("Johnson", "Smith"),
    stringsAsFactors = FALSE
  )

  result <- .derive_source_keys_labels(cfg, src)

  expect_equal(result$keys, c("s1", "s2"))
  expect_equal(result$labels, c("Alice Johnson", "Bob Smith"))
})


test_that(".derive_source_keys_labels coerces numeric IDs to character", {
  cfg <- table_config(
    row_id_col = "id",
    row_label_col = "name",
    columns = list(widget_col("x", "text", "X"))
  )

  src <- data.frame(
    id = c(101, 102),
    name = c("Alice", "Bob"),
    stringsAsFactors = FALSE
  )

  result <- .derive_source_keys_labels(cfg, src)
  expect_type(result$keys, "character")
  expect_equal(result$keys, c("101", "102"))
})


# ── .merge_dynamic_rows ──────────────────────────────────────────────────────

test_that(".merge_dynamic_rows preserves existing state for surviving rows", {
  cfg <- table_config(
    row_id_col = "id",
    row_label_col = "name",
    columns = list(
      widget_col("score", "numeric", "Score", options = list(min = 0))
    )
  )

  existing <- list(
    s1 = list(.row_key = "s1", score = 95),
    s2 = list(.row_key = "s2", score = 80)
  )

  merged <- .merge_dynamic_rows(cfg, c("s1", "s2"), existing)

  expect_equal(merged$s1$score, 95)
  expect_equal(merged$s2$score, 80)
})


test_that(".merge_dynamic_rows adds defaults for new rows", {
  cfg <- table_config(
    row_id_col = "id",
    row_label_col = "name",
    columns = list(
      widget_col("score", "numeric", "Score", options = list(min = 0)),
      widget_col("notes", "text", "Notes")
    )
  )

  existing <- list(
    s1 = list(.row_key = "s1", score = 95, notes = "Good")
  )

  merged <- .merge_dynamic_rows(cfg, c("s1", "s2"), existing)

  # s1 preserved

  expect_equal(merged$s1$score, 95)
  expect_equal(merged$s1$notes, "Good")
  # s2 gets defaults
  expect_equal(merged$s2$.row_key, "s2")
  expect_equal(merged$s2$score, 0)
  expect_equal(merged$s2$notes, "")
})


test_that(".merge_dynamic_rows retains departed rows at end", {
  cfg <- table_config(
    row_id_col = "id",
    row_label_col = "name",
    columns = list(
      widget_col("score", "numeric", "Score", options = list(min = 0))
    )
  )

  existing <- list(
    s1 = list(.row_key = "s1", score = 95),
    s2 = list(.row_key = "s2", score = 80),
    s3 = list(.row_key = "s3", score = 70)
  )

  # s2 leaves the visible set
  merged <- .merge_dynamic_rows(cfg, c("s1", "s3"), existing)

  # Visible rows come first in order
  expect_equal(names(merged)[1:2], c("s1", "s3"))
  # Departed row is retained
  expect_true("s2" %in% names(merged))
  expect_equal(merged$s2$score, 80)
})


test_that(".merge_dynamic_rows restores departed rows that reappear", {
  cfg <- table_config(
    row_id_col = "id",
    row_label_col = "name",
    columns = list(
      widget_col("score", "numeric", "Score", options = list(min = 0))
    )
  )

  # Step 1: s1 and s2 visible, s2 has user value
  existing <- list(
    s1 = list(.row_key = "s1", score = 95),
    s2 = list(.row_key = "s2", score = 80)
  )

  # Step 2: s2 leaves
  after_leave <- .merge_dynamic_rows(cfg, c("s1"), existing)
  expect_equal(after_leave$s2$score, 80) # still retained

  # Step 3: s2 returns
  after_return <- .merge_dynamic_rows(cfg, c("s1", "s2"), after_leave)
  expect_equal(after_return$s2$score, 80) # restored
  expect_equal(names(after_return)[1:2], c("s1", "s2")) # back in visible order
})


test_that(".merge_dynamic_rows handles empty existing state", {
  cfg <- table_config(
    row_id_col = "id",
    row_label_col = "name",
    columns = list(
      widget_col("x", "text", "X")
    )
  )

  merged <- .merge_dynamic_rows(cfg, c("r1", "r2"), list())

  expect_equal(names(merged), c("r1", "r2"))
  expect_equal(merged$r1$.row_key, "r1")
  expect_equal(merged$r1$x, "")
})


test_that(".merge_dynamic_rows handles empty new_keys", {
  cfg <- table_config(
    row_id_col = "id",
    row_label_col = "name",
    columns = list(
      widget_col("x", "text", "X")
    )
  )

  existing <- list(
    s1 = list(.row_key = "s1", x = "hello")
  )

  merged <- .merge_dynamic_rows(cfg, character(0), existing)

  # No visible rows, but departed row is retained
  expect_equal(length(merged), 1L)
  expect_equal(merged$s1$x, "hello")
})


# ── .rows_from_saved with effective_keys ─────────────────────────────────────

test_that(".rows_from_saved uses effective_keys when provided", {
  cfg <- table_config(
    row_id_col = "id",
    row_label_col = "name",
    columns = list(
      widget_col("grade", "dropdown", "Grade",
        options = list(choices = c("A", "B", "C"))
      )
    ),
    from_saved_fn = function(db_row, col_specs) {
      list(grade = as.character(db_row$grade))
    }
  )

  saved <- data.frame(
    id = c("s1", "s2", "s3"),
    grade = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )

  # Only s1 and s3 are in the effective key set
  result <- .rows_from_saved(cfg, saved, effective_keys = c("s1", "s3"))

  expect_equal(names(result), c("s1", "s3"))
  expect_equal(result$s1$grade, "A")
  expect_equal(result$s3$grade, "C")
  expect_null(result$s2) # s2 filtered out
})


test_that(".rows_from_saved falls back to config$row_keys when no effective_keys", {
 cfg <- table_config(
    row_keys = c("r1", "r2"),
    row_labels = c("R1", "R2"),
    columns = list(
      widget_col("val", "text", "Val")
    ),
    from_saved_fn = function(db_row, col_specs) {
      list(val = as.character(db_row$val))
    }
  )

  saved <- data.frame(
    id = c("r1", "r2"),
    val = c("hello", "world"),
    stringsAsFactors = FALSE
  )

  result <- .rows_from_saved(cfg, saved)
  expect_equal(names(result), c("r1", "r2"))
  expect_equal(result$r1$val, "hello")
})


# ── .build_table_df with effective_keys/labels ───────────────────────────────

test_that(".build_table_df uses effective_keys/labels when provided", {
  cfg <- table_config(
    row_id_col = "id",
    row_label_col = "name",
    columns = list(
      widget_col("score", "numeric", "Score", options = list(min = 0))
    )
  )

  current_rows <- list(
    s1 = list(.row_key = "s1", score = 95),
    s2 = list(.row_key = "s2", score = 80),
    departed = list(.row_key = "departed", score = 50)
  )

  tbl <- .build_table_df(
    cfg, current_rows, settings = list(),
    effective_keys = c("s1", "s2"),
    effective_labels = c("Alice", "Bob")
  )

  expect_equal(nrow(tbl), 2L)
  expect_equal(tbl$.row_key, c("s1", "s2"))
  expect_equal(tbl$.row_label, c("Alice", "Bob"))
  expect_equal(tbl$score, c(95, 80))
})


test_that(".build_table_df falls back to config for static mode", {
  cfg <- table_config(
    row_keys = c("r1", "r2"),
    row_labels = c("Row 1", "Row 2"),
    columns = list(
      widget_col("x", "text", "X")
    )
  )

  current_rows <- list(
    r1 = list(.row_key = "r1", x = "hello"),
    r2 = list(.row_key = "r2", x = "world")
  )

  tbl <- .build_table_df(cfg, current_rows, settings = list())

  expect_equal(nrow(tbl), 2L)
  expect_equal(tbl$.row_key, c("r1", "r2"))
  expect_equal(tbl$.row_label, c("Row 1", "Row 2"))
})


test_that(".build_table_df with selection column and effective_keys", {
  cfg <- table_config(
    row_id_col = "id",
    row_label_col = "name",
    columns = list(
      widget_col("x", "text", "X")
    ),
    selectable = TRUE
  )

  current_rows <- list(
    s1 = list(.row_key = "s1", .selected = TRUE, x = "a"),
    s2 = list(.row_key = "s2", .selected = FALSE, x = "b")
  )

  tbl <- .build_table_df(
    cfg, current_rows, settings = list(),
    effective_keys = c("s1", "s2"),
    effective_labels = c("Alice", "Bob")
  )

  expect_true(".selected" %in% names(tbl))
  expect_equal(tbl$.selected, c(TRUE, FALSE))
})


# ── display_col() ────────────────────────────────────────────────────────────

test_that("display_col creates a valid spec", {
  dc <- display_col("email", "Email", width = 200)
  expect_s3_class(dc, "display_col")
  expect_equal(dc$id, "email")
  expect_equal(dc$label, "Email")
  expect_equal(dc$width, 200)
  expect_null(dc$min_width)
  expect_null(dc$render_fn)
})


test_that("display_col accepts a render_fn", {
  render <- function(value, row_key) sprintf("<b>%s</b>", value)
  dc <- display_col("dept", "Department", render_fn = render)
  expect_identical(dc$render_fn, render)
})


test_that("display_col rejects empty id", {
  expect_error(display_col("", "Label"))
})


test_that("display_col rejects non-function render_fn", {
  expect_error(display_col("x", "X", render_fn = "not a function"))
})


# ── table_config with display_cols ───────────────────────────────────────────

test_that("table_config accepts display_cols in dynamic mode", {
  cfg <- table_config(
    row_id_col = "id",
    row_label_col = "name",
    display_cols = list(
      display_col("email", "Email", width = 200),
      display_col("dept", "Dept")
    ),
    columns = list(
      widget_col("score", "numeric", "Score", options = list(min = 0))
    )
  )
  expect_length(cfg$display_cols, 2L)
  expect_equal(cfg$display_cols[[1]]$id, "email")
  expect_equal(cfg$display_cols[[2]]$id, "dept")
})


test_that("table_config rejects display_cols in static mode", {
  expect_error(
    table_config(
      row_keys = c("r1"),
      row_labels = c("R1"),
      display_cols = list(display_col("x", "X")),
      columns = list(widget_col("y", "text", "Y"))
    ),
    "display_cols requires dynamic mode"
  )
})


test_that("table_config rejects display_col ID collision with widget_col", {
  expect_error(
    table_config(
      row_id_col = "id",
      row_label_col = "name",
      display_cols = list(display_col("score", "Score")),
      columns = list(
        widget_col("score", "numeric", "Score", options = list(min = 0))
      )
    ),
    "collide"
  )
})


test_that("table_config rejects non-display_col elements in display_cols", {
  expect_error(
    table_config(
      row_id_col = "id",
      row_label_col = "name",
      display_cols = list(list(id = "x", label = "X")),
      columns = list(widget_col("y", "text", "Y"))
    ),
    "display_col()"
  )
})


# ── .build_table_df with display columns ─────────────────────────────────────

test_that(".build_table_df includes display column values from source_snapshot", {
  cfg <- table_config(
    row_id_col = "id",
    row_label_col = "name",
    display_cols = list(
      display_col("email", "Email"),
      display_col("dept", "Dept")
    ),
    columns = list(
      widget_col("score", "numeric", "Score", options = list(min = 0))
    )
  )

  current_rows <- list(
    s1 = list(.row_key = "s1", score = 95),
    s2 = list(.row_key = "s2", score = 80)
  )

  src <- data.frame(
    id = c("s1", "s2"),
    name = c("Alice", "Bob"),
    email = c("alice@x.com", "bob@x.com"),
    dept = c("Engineering", "Marketing"),
    stringsAsFactors = FALSE
  )

  tbl <- .build_table_df(
    cfg, current_rows, settings = list(),
    effective_keys = c("s1", "s2"),
    effective_labels = c("Alice", "Bob"),
    source_snapshot = src
  )

  expect_true("email" %in% names(tbl))
  expect_true("dept" %in% names(tbl))
  expect_equal(tbl$email, c("alice@x.com", "bob@x.com"))
  expect_equal(tbl$dept, c("Engineering", "Marketing"))
})


test_that(".build_table_df handles missing display column gracefully", {
  cfg <- table_config(
    row_id_col = "id",
    row_label_col = "name",
    display_cols = list(
      display_col("nonexistent", "Missing Col")
    ),
    columns = list(widget_col("x", "text", "X"))
  )

  current_rows <- list(
    s1 = list(.row_key = "s1", x = "hello")
  )

  src <- data.frame(
    id = "s1", name = "Alice",
    stringsAsFactors = FALSE
  )

  tbl <- .build_table_df(
    cfg, current_rows, settings = list(),
    effective_keys = "s1",
    effective_labels = "Alice",
    source_snapshot = src
  )

  expect_true("nonexistent" %in% names(tbl))
  expect_true(is.na(tbl$nonexistent))
})


test_that(".build_table_df skips display columns when no source_snapshot", {
  cfg <- table_config(
    row_id_col = "id",
    row_label_col = "name",
    display_cols = list(display_col("email", "Email")),
    columns = list(widget_col("x", "text", "X"))
  )

  current_rows <- list(
    s1 = list(.row_key = "s1", x = "hello")
  )

  tbl <- .build_table_df(
    cfg, current_rows, settings = list(),
    effective_keys = "s1",
    effective_labels = "Alice",
    source_snapshot = NULL
  )

  expect_false("email" %in% names(tbl))
})
