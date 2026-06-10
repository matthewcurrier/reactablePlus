# tests/testthat/test-appendable-render.R
#
# Tests for appendable mode rendering helpers.
# Covers: .build_table_df with delete column, .build_col_defs with
# delete colDef, and interaction with existing features (gating,
# selection, primitive columns).


# ── .build_table_df: delete column ───────────────────────────────────────────

test_that(".build_table_df adds .delete column in appendable mode", {
  cfg <- table_config(
    appendable = TRUE,
    allow_delete = TRUE,
    columns = list(
      widget_col("x", "text", "X")
    )
  )

  current_rows <- list(
    row_1 = list(.row_key = "row_1", x = "hello"),
    row_2 = list(.row_key = "row_2", x = "world")
  )

  tbl <- .build_table_df(
    cfg, current_rows, settings = list(),
    effective_keys = c("row_1", "row_2"),
    effective_labels = c("", "")
  )

  expect_true(".delete" %in% names(tbl))
  expect_equal(nrow(tbl), 2L)
  expect_equal(tbl$.delete, c("", ""))
})


test_that(".build_table_df omits .delete when allow_delete is FALSE", {
  cfg <- table_config(
    appendable = TRUE,
    allow_delete = FALSE,
    columns = list(
      widget_col("x", "text", "X")
    )
  )

  current_rows <- list(
    row_1 = list(.row_key = "row_1", x = "hello")
  )

  tbl <- .build_table_df(
    cfg, current_rows, settings = list(),
    effective_keys = "row_1",
    effective_labels = ""
  )

  expect_false(".delete" %in% names(tbl))
})


test_that(".build_table_df omits .delete for static config", {
  cfg <- table_config(
    row_keys = c("r1"),
    row_labels = c("R1"),
    columns = list(widget_col("x", "text", "X"))
  )

  current_rows <- list(
    r1 = list(.row_key = "r1", x = "hi")
  )

  tbl <- .build_table_df(cfg, current_rows, settings = list())

  expect_false(".delete" %in% names(tbl))
})


test_that(".build_table_df handles empty appendable table", {
  cfg <- table_config(
    appendable = TRUE,
    allow_delete = TRUE,
    min_rows = 0L,
    columns = list(
      widget_col("x", "text", "X")
    )
  )

  tbl <- .build_table_df(
    cfg, list(), settings = list(),
    effective_keys = character(0),
    effective_labels = character(0)
  )

  expect_equal(nrow(tbl), 0L)
  expect_true(".delete" %in% names(tbl))
  expect_true("x" %in% names(tbl))
})


# ── .build_table_df: appendable with multiple column types ───────────────────

test_that(".build_table_df works with all primitive types in appendable mode", {
  cfg <- table_config(
    appendable = TRUE,
    columns = list(
      widget_col("status", "dropdown", "Status",
        options = list(choices = c("Active", "Inactive"))
      ),
      widget_col("score", "numeric", "Score",
        options = list(min = 0)
      ),
      widget_col("active", "checkbox", "Active"),
      widget_col("notes", "text", "Notes")
    )
  )

  current_rows <- list(
    row_1 = list(
      .row_key = "row_1",
      status = "Active",
      score = 85,
      active = TRUE,
      notes = "good"
    )
  )

  tbl <- .build_table_df(
    cfg, current_rows, settings = list(),
    effective_keys = "row_1",
    effective_labels = ""
  )

  expect_equal(tbl$status, "Active")
  expect_equal(tbl$score, 85)
  expect_true(tbl$active)
  expect_equal(tbl$notes, "good")
})


# ── .build_table_df: appendable + selectable ─────────────────────────────────

test_that(".build_table_df includes .selected column in appendable + selectable", {
  cfg <- table_config(
    appendable = TRUE,
    selectable = TRUE,
    columns = list(widget_col("x", "text", "X"))
  )

  current_rows <- list(
    row_1 = list(.row_key = "row_1", .selected = TRUE, x = "a"),
    row_2 = list(.row_key = "row_2", .selected = FALSE, x = "b")
  )

  tbl <- .build_table_df(
    cfg, current_rows, settings = list(),
    effective_keys = c("row_1", "row_2"),
    effective_labels = c("", "")
  )

  expect_true(".selected" %in% names(tbl))
  expect_equal(tbl$.selected, c(TRUE, FALSE))
})


# ── .build_col_defs: delete column ───────────────────────────────────────────

test_that(".build_col_defs includes .delete colDef in appendable mode", {
  cfg <- table_config(
    appendable = TRUE,
    allow_delete = TRUE,
    columns = list(widget_col("x", "text", "X"))
  )

  current_rows <- list(
    row_1 = list(.row_key = "row_1", x = "hi")
  )

  tbl <- .build_table_df(
    cfg, current_rows, settings = list(),
    effective_keys = "row_1",
    effective_labels = ""
  )

  col_defs <- .build_col_defs(
    cfg,
    ns = shiny::NS("test"),
    current_rows = current_rows,
    settings = list(),
    tbl = tbl,
    effective_keys = "row_1",
    effective_labels = ""
  )

  expect_true(".delete" %in% names(col_defs))
})


test_that(".build_col_defs omits .delete when allow_delete = FALSE", {
  cfg <- table_config(
    appendable = TRUE,
    allow_delete = FALSE,
    columns = list(widget_col("x", "text", "X"))
  )

  current_rows <- list(
    row_1 = list(.row_key = "row_1", x = "hi")
  )

  tbl <- .build_table_df(
    cfg, current_rows, settings = list(),
    effective_keys = "row_1",
    effective_labels = ""
  )

  col_defs <- .build_col_defs(
    cfg,
    ns = shiny::NS("test"),
    current_rows = current_rows,
    settings = list(),
    tbl = tbl,
    effective_keys = "row_1",
    effective_labels = ""
  )

  expect_false(".delete" %in% names(col_defs))
})


test_that(".build_col_defs omits .delete for non-appendable config", {
  cfg <- table_config(
    row_keys = "r1",
    row_labels = "R1",
    columns = list(widget_col("x", "text", "X"))
  )

  current_rows <- list(
    r1 = list(.row_key = "r1", x = "hi")
  )

  tbl <- .build_table_df(cfg, current_rows, settings = list())

  col_defs <- .build_col_defs(
    cfg,
    ns = shiny::NS("test"),
    current_rows = current_rows,
    settings = list(),
    tbl = tbl
  )

  expect_false(".delete" %in% names(col_defs))
})


# ── .build_col_defs: delete cell renders HTML ────────────────────────────────

test_that(".delete cell function renders a button with correct input ID", {
  cfg <- table_config(
    appendable = TRUE,
    allow_delete = TRUE,
    columns = list(widget_col("x", "text", "X"))
  )

  current_rows <- list(
    row_1 = list(.row_key = "row_1", x = "hi")
  )

  tbl <- .build_table_df(
    cfg, current_rows, settings = list(),
    effective_keys = "row_1",
    effective_labels = ""
  )

  ns <- shiny::NS("mymod")
  col_defs <- .build_col_defs(
    cfg,
    ns = ns,
    current_rows = current_rows,
    settings = list(),
    tbl = tbl,
    effective_keys = "row_1",
    effective_labels = ""
  )

  # Call the cell function to get the rendered HTML
  delete_cell_fn <- col_defs$.delete$cell
  html <- delete_cell_fn("", 1L)

  # Should contain the namespaced input ID for the delete action
  expect_match(html, "mymod-.delete_row")
  # Should contain the row key
  expect_match(html, "row_1")
  # Should be a button
  expect_match(html, "<button")
})


# ── .build_empty_row_state: works with appendable row keys ──────────────────

test_that(".build_empty_row_state works with auto-generated row keys", {
  cfg <- table_config(
    appendable = TRUE,
    columns = list(
      widget_col("fruit", "dropdown", "Fruit",
        options = list(choices = c("Apple", "Banana"))
      ),
      widget_col("qty", "numeric", "Qty",
        options = list(min = 1)
      )
    )
  )

  row <- .build_empty_row_state(cfg, "row_42")

  expect_equal(row$.row_key, "row_42")
  expect_true(is.na(row$fruit))
  expect_equal(row$qty, 1)
})
