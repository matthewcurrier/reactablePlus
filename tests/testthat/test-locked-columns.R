# tests/testthat/test-locked-columns.R
#
# widget_col(locked = ): a whole column locked for every row, e.g. by a plan
# or permission check the app evaluates at render time.

render_cell <- function(cs, row) {
  def <- reactablePlus:::.build_widget_coldef(
    cs,
    ns = shiny::NS("t"),
    current_rows = list(r1 = row),
    settings = list(),
    tbl = data.frame(.row_key = "r1"),
    config = list()
  )
  as.character(def$cell(NA, 1L))
}

# ── widget_col: validation ───────────────────────────────────────────────────

test_that("widget_col accepts NULL, a flag or a function for locked", {
  expect_null(widget_col("n", "text", "N")$locked)
  expect_true(widget_col("n", "text", "N", locked = TRUE)$locked)
  expect_true(is.function(
    widget_col("n", "text", "N", locked = function() TRUE)$locked
  ))
})

test_that("widget_col rejects any other locked value", {
  expect_error(widget_col("n", "text", "N", locked = "yes"), "locked must be")
  expect_error(
    widget_col("n", "text", "N", locked = c(TRUE, FALSE)),
    "locked must be"
  )
})

# ── .column_locked / .cell_locked ────────────────────────────────────────────

describe(".column_locked", {
  column_locked <- reactablePlus:::.column_locked

  it("is FALSE when nothing locks the column", {
    expect_false(column_locked(widget_col("n", "text", "N")))
  })

  it("evaluates a function each time it is asked", {
    flag <- FALSE
    cs <- widget_col("n", "text", "N", locked = function() flag)
    expect_false(column_locked(cs))
    flag <- TRUE
    expect_true(column_locked(cs))
  })

  it("gives a one-argument function the row, to lock row by row", {
    cs <- widget_col(
      "n", "text", "N",
      locked = function(row) identical(row$kind, "private")
    )
    expect_true(column_locked(cs, list(kind = "private")))
    expect_false(column_locked(cs, list(kind = "public")))
    expect_false(column_locked(cs))
  })

  it("fails closed when the function errors", {
    cs <- widget_col("n", "text", "N", locked = function() stop("boom"))
    expect_true(column_locked(cs))
  })
})

test_that(".cell_locked combines the column lock with the row gate", {
  cell_locked <- reactablePlus:::.cell_locked
  gated <- widget_col(
    "n", "text", "N",
    gate = list(list(type = "selected"))
  )
  expect_true(cell_locked(gated, list(.selected = FALSE)))
  expect_false(cell_locked(gated, list(.selected = TRUE)))

  locked <- widget_col("n", "text", "N", locked = TRUE)
  expect_true(cell_locked(locked, list(.selected = TRUE)))
})

# ── Rendering ────────────────────────────────────────────────────────────────

test_that("a locked text cell is disabled and leaves its value out", {
  cs <- widget_col(
    "notes", "text", "Notes",
    locked = TRUE,
    options = list(placeholder = "Notes…", locked_placeholder = "Not collected")
  )
  html <- render_cell(cs, list(notes = "a private note"))
  expect_false(grepl("a private note", html, fixed = TRUE))
  expect_match(html, "disabled")
  expect_match(html, "Not collected", fixed = TRUE)
})

test_that("an unlocked text cell still shows its value", {
  cs <- widget_col("notes", "text", "Notes", locked = FALSE)
  html <- render_cell(cs, list(notes = "a visible note"))
  expect_match(html, "a visible note", fixed = TRUE)
  expect_false(grepl("disabled", html, fixed = TRUE))
})

test_that("a row-by-row lock hides only the locked row's value", {
  cs <- widget_col(
    "notes", "text", "Notes",
    locked = function(row) identical(row$kind, "private")
  )
  expect_false(grepl(
    "kept back", render_cell(cs, list(kind = "private", notes = "kept back")),
    fixed = TRUE
  ))
  expect_match(
    render_cell(cs, list(kind = "public", notes = "shown")),
    "shown",
    fixed = TRUE
  )
})

test_that("a locked notes_input cell is disabled and leaves its value out", {
  cs <- widget_col(
    "notes", "notes_input", "Notes",
    locked = function() TRUE,
    options = list(locked_placeholder = "Not collected")
  )
  html <- render_cell(cs, list(notes = "a private note"))
  expect_false(grepl("a private note", html, fixed = TRUE))
  expect_match(html, "disabled")
  expect_match(html, "Not collected", fixed = TRUE)
})
