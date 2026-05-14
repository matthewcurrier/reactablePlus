# tests/testthat/test-config-phase2.R
#
# Phase 2 tests — gate system, selection, and reset in config_table.

# ── .is_gate_open ────────────────────────────────────────────────────────────

describe(".is_gate_open", {
  gate_open <- reactablePlus:::.is_gate_open

  it("returns TRUE when gate is NULL", {
    expect_true(gate_open(NULL, list()))
  })

  it("evaluates value condition against row state", {
    gate <- list(list(type = "value", col_id = "status", values = c("active")))

    expect_true(gate_open(gate, list(status = "active")))
    expect_false(gate_open(gate, list(status = "inactive")))
    expect_false(gate_open(gate, list(status = NA)))
    expect_false(gate_open(gate, list()))
  })

  it("accepts multiple unlocking values", {
    gate <- list(
      list(type = "value", col_id = "status", values = c("active", "pending"))
    )
    expect_true(gate_open(gate, list(status = "active")))
    expect_true(gate_open(gate, list(status = "pending")))
    expect_false(gate_open(gate, list(status = "closed")))
  })

  it("requires ALL conditions (AND logic)", {
    gate <- list(
      list(type = "value", col_id = "status", values = c("active")),
      list(type = "value", col_id = "tier", values = c("premium"))
    )
    expect_true(gate_open(gate, list(status = "active", tier = "premium")))
    expect_false(gate_open(gate, list(status = "active", tier = "basic")))
    expect_false(gate_open(gate, list(status = "inactive", tier = "premium")))
  })

  it("evaluates selected condition", {
    gate <- list(list(type = "selected"))

    expect_true(gate_open(gate, list(.selected = TRUE)))
    expect_false(gate_open(gate, list(.selected = FALSE)))
    expect_false(gate_open(gate, list()))
  })

  it("combines value and selected conditions", {
    gate <- list(
      list(type = "selected"),
      list(type = "value", col_id = "status", values = c("active"))
    )
    expect_true(gate_open(gate, list(.selected = TRUE, status = "active")))
    expect_false(gate_open(gate, list(.selected = TRUE, status = "inactive")))
    expect_false(gate_open(gate, list(.selected = FALSE, status = "active")))
  })
})


# ── .locked_attrs ────────────────────────────────────────────────────────────

describe(".locked_attrs", {
  locked_attrs <- reactablePlus:::.locked_attrs

  it("returns disabled + locked style when locked", {
    result <- locked_attrs(TRUE, "width: 100%;")
    expect_equal(result$disabled, "disabled")
    expect_match(result$style, "opacity: 0.4")
    expect_match(result$style, "pointer-events: none")
    expect_match(result$style, "width: 100%;")
  })

  it("returns NULL disabled and base style when unlocked", {
    result <- locked_attrs(FALSE, "width: 100%;")
    expect_null(result$disabled)
    expect_equal(result$style, "width: 100%;")
  })
})


# ── widget_col gate validation ───────────────────────────────────────────────

test_that("widget_col accepts valid gate conditions", {
  wc <- widget_col("score", "numeric", "Score",
    options = list(min = 0),
    gate = list(
      list(type = "value", col_id = "status", values = c("active"))
    )
  )
  expect_length(wc$gate, 1L)
  expect_equal(wc$gate[[1]]$type, "value")
})

test_that("widget_col rejects gate with invalid type", {
  expect_error(
    widget_col("x", "text", "X",
      gate = list(list(type = "magic"))
    ),
    "must be 'value' or 'selected'"
  )
})

test_that("widget_col rejects value gate without col_id", {
  expect_error(
    widget_col("x", "text", "X",
      gate = list(list(type = "value", values = c("a")))
    ),
    "col_id"
  )
})

test_that("widget_col rejects value gate without values", {
  expect_error(
    widget_col("x", "text", "X",
      gate = list(list(type = "value", col_id = "status"))
    ),
    "values"
  )
})


# ── table_config gate wiring ─────────────────────────────────────────────────

test_that("table_config auto-sets triggers_rerender on gate controllers", {
  cfg <- table_config(
    row_keys = c("a", "b"),
    row_labels = c("A", "B"),
    columns = list(
      widget_col("status", "dropdown", "Status",
        options = list(choices = c("active", "inactive"))
      ),
      widget_col("score", "numeric", "Score",
        options = list(min = 0),
        gate = list(
          list(type = "value", col_id = "status", values = c("active"))
        )
      )
    ),
    badge_col = NULL,
    year_col = NULL
  )
  # "status" column should be auto-marked as triggers_rerender
  status_col <- cfg$columns[[1]]
  expect_true(status_col$triggers_rerender)
  # "score" should not be auto-marked (it's the dependent, not controller)
  score_col <- cfg$columns[[2]]
  expect_false(score_col$triggers_rerender)
})

test_that("table_config rejects gate referencing non-existent col_id", {
  expect_error(
    table_config(
      row_keys = "a", row_labels = "A",
      columns = list(
        widget_col("score", "numeric", "Score",
          options = list(min = 0),
          gate = list(list(type = "value", col_id = "missing", values = "x"))
        )
      ),
      badge_col = NULL, year_col = NULL
    ),
    "does not exist"
  )
})

test_that("table_config rejects selected gate without selectable", {
  expect_error(
    table_config(
      row_keys = "a", row_labels = "A",
      selectable = FALSE,
      columns = list(
        widget_col("score", "numeric", "Score",
          options = list(min = 0),
          gate = list(list(type = "selected"))
        )
      ),
      badge_col = NULL, year_col = NULL
    ),
    "selectable is not TRUE"
  )
})

test_that("table_config accepts selected gate with selectable = TRUE", {
  cfg <- table_config(
    row_keys = "a", row_labels = "A",
    selectable = TRUE,
    columns = list(
      widget_col("score", "numeric", "Score",
        options = list(min = 0),
        gate = list(list(type = "selected"))
      )
    ),
    badge_col = NULL, year_col = NULL
  )
  expect_true(cfg$selectable)
})


# ── table_config selectable and show_reset ───────────────────────────────────

test_that("table_config stores selectable flag", {
  cfg <- table_config(
    row_keys = "a", row_labels = "A",
    selectable = TRUE,
    columns = list(widget_col("x", "text", "X")),
    badge_col = NULL, year_col = NULL
  )
  expect_true(cfg$selectable)
})

test_that("table_config stores show_reset flag", {
  cfg <- table_config(
    row_keys = "a", row_labels = "A",
    show_reset = TRUE,
    columns = list(widget_col("x", "text", "X")),
    badge_col = NULL, year_col = NULL
  )
  expect_true(cfg$show_reset)
})


# ── .build_empty_rows includes .selected ─────────────────────────────────────

test_that(".build_empty_rows includes .selected when selectable", {
  build <- reactablePlus:::.build_empty_rows
  cfg <- table_config(
    row_keys = c("r1", "r2"), row_labels = c("R1", "R2"),
    selectable = TRUE,
    columns = list(widget_col("x", "text", "X")),
    badge_col = NULL, year_col = NULL
  )
  rows <- build(cfg)
  expect_false(rows$r1$.selected)
  expect_false(rows$r2$.selected)
})

test_that(".build_empty_rows omits .selected when not selectable", {
  build <- reactablePlus:::.build_empty_rows
  cfg <- table_config(
    row_keys = "r1", row_labels = "R1",
    columns = list(widget_col("x", "text", "X")),
    badge_col = NULL, year_col = NULL
  )
  rows <- build(cfg)
  expect_null(rows$r1$.selected)
})
