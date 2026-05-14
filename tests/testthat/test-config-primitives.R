# tests/testthat/test-config-primitives.R
#
# Phase 1 tests — primitive input types in config_table system.
# Covers widget_col() construction, .default_validate(), and
# .extract_col_values() for dropdown, numeric, date, checkbox,
# toggle, and text types.

# ── widget_col: type validation ──────────────────────────────────────────────

test_that("widget_col accepts all valid types", {
  valid_types <- c(
    "search_picker",
    "attendance_picker",
    "homeschool_picker",
    "notes_input",
    "custom",
    "dropdown",
    "numeric",
    "date",
    "checkbox",
    "toggle",
    "text"
  )
  purrr::walk(valid_types, function(t) {
    # dropdown and numeric have required options
    opts <- switch(
      t,
      dropdown = list(choices = c("A", "B")),
      numeric = list(min = 0),
      list()
    )
    wc <- widget_col("test_col", t, "Test", options = opts)
    expect_s3_class(wc, "widget_col")
    expect_equal(wc$type, t)
  })
})

test_that("widget_col rejects invalid type", {
  expect_error(
    widget_col("x", "sparkline", "X"),
    "not supported"
  )
})


# ── widget_col: dropdown ─────────────────────────────────────────────────────

test_that("widget_col dropdown requires choices", {
  expect_error(
    widget_col("status", "dropdown", "Status"),
    "choices is required"
  )
})

test_that("widget_col dropdown normalizes unnamed character choices", {
  wc <- widget_col(
    "status",
    "dropdown",
    "Status",
    options = list(choices = c("Active", "Inactive"))
  )
  choices <- wc$options$choices
  expect_length(choices, 2L)
  expect_equal(choices[[1]]$label, "Active")
  expect_equal(choices[[1]]$value, "Active")
  expect_equal(choices[[2]]$label, "Inactive")
  expect_equal(choices[[2]]$value, "Inactive")
})

test_that("widget_col dropdown normalizes named character choices", {
  wc <- widget_col(
    "priority",
    "dropdown",
    "Priority",
    options = list(choices = c("High" = "H", "Medium" = "M", "Low" = "L"))
  )
  choices <- wc$options$choices
  expect_length(choices, 3L)
  expect_equal(choices[[1]]$label, "High")
  expect_equal(choices[[1]]$value, "H")
})

test_that("widget_col dropdown passes through list-of-lists choices", {
  raw <- list(
    list(label = "Good", value = 3),
    list(label = "Bad", value = 1)
  )
  wc <- widget_col(
    "rating",
    "dropdown",
    "Rating",
    options = list(choices = raw)
  )
  expect_identical(wc$options$choices, raw)
})


# ── widget_col: numeric ──────────────────────────────────────────────────────

test_that("widget_col numeric requires min", {
  expect_error(
    widget_col("score", "numeric", "Score"),
    "min is required"
  )
})

test_that("widget_col numeric rejects non-numeric min", {
  expect_error(
    widget_col("score", "numeric", "Score", options = list(min = "zero")),
    "single numeric"
  )
})

test_that("widget_col numeric stores options correctly", {
  wc <- widget_col(
    "score",
    "numeric",
    "Score",
    options = list(min = 0, max = 100, step = 5)
  )
  expect_equal(wc$options$min, 0)
  expect_equal(wc$options$max, 100)
  expect_equal(wc$options$step, 5)
})


# ── widget_col: simple types (no required options) ───────────────────────────

test_that("widget_col date stores min_date and max_date", {
  wc <- widget_col(
    "start",
    "date",
    "Start Date",
    options = list(min_date = "2020-01-01", max_date = "2030-12-31")
  )
  expect_equal(wc$options$min_date, "2020-01-01")
  expect_equal(wc$options$max_date, "2030-12-31")
})

test_that("widget_col checkbox requires no special options", {
  wc <- widget_col("active", "checkbox", "Active")
  expect_equal(wc$type, "checkbox")
  expect_equal(wc$options, list())
})

test_that("widget_col toggle requires no special options", {
  wc <- widget_col("enabled", "toggle", "Enabled")
  expect_equal(wc$type, "toggle")
})

test_that("widget_col text stores max_chars", {
  wc <- widget_col(
    "notes",
    "text",
    "Notes",
    options = list(max_chars = 200, placeholder = "Enter notes...")
  )
  expect_equal(wc$options$max_chars, 200)
  expect_equal(wc$options$placeholder, "Enter notes...")
})


# ── widget_col: empty_value inference ────────────────────────────────────────

test_that("widget_col infers correct empty_value per type", {
  expect_identical(
    widget_col(
      "x",
      "dropdown",
      "X",
      options = list(choices = c("A"))
    )$empty_value,
    NA_character_
  )
  expect_identical(
    widget_col("x", "numeric", "X", options = list(min = 5))$empty_value,
    5
  )
  expect_identical(
    widget_col("x", "numeric", "X", options = list(min = 0))$empty_value,
    0
  )
  expect_identical(
    widget_col("x", "date", "X")$empty_value,
    NA_character_
  )
  expect_identical(
    widget_col("x", "checkbox", "X")$empty_value,
    FALSE
  )
  expect_identical(
    widget_col("x", "toggle", "X")$empty_value,
    FALSE
  )
  expect_identical(
    widget_col("x", "text", "X")$empty_value,
    ""
  )
  expect_identical(
    widget_col("x", "notes_input", "X")$empty_value,
    ""
  )
  expect_null(
    widget_col("x", "search_picker", "X")$empty_value
  )
})

test_that("widget_col respects explicit empty_value over inferred", {
  wc <- widget_col(
    "x",
    "numeric",
    "X",
    options = list(min = 0),
    empty_value = -1
  )
  expect_equal(wc$empty_value, -1)
})


# ── .default_validate ────────────────────────────────────────────────────────

describe(".default_validate for primitive types", {
  validate <- reactablePlus:::.default_validate

  it("coerces dropdown empty string to NA", {
    expect_identical(validate("", "dropdown"), NA_character_)
  })

  it("coerces dropdown NULL to NA", {
    expect_identical(validate(NULL, "dropdown"), NA_character_)
  })

  it("keeps dropdown string value", {
    expect_equal(validate("high", "dropdown"), "high")
  })

  it("parses numeric value", {
    expect_equal(validate(42, "numeric"), 42)
    expect_equal(validate("3.14", "numeric"), 3.14)
  })

  it("returns NA for non-numeric input", {
    expect_true(is.na(validate("abc", "numeric")))
    expect_true(is.na(validate(NULL, "numeric")))
  })

  it("keeps date string value", {
    expect_equal(validate("2024-01-15", "date"), "2024-01-15")
  })

  it("returns NA for empty date", {
    expect_identical(validate("", "date"), NA_character_)
  })

  it("coerces checkbox to logical", {
    expect_true(validate(TRUE, "checkbox"))
    expect_false(validate(FALSE, "checkbox"))
    expect_false(validate(NULL, "checkbox"))
    expect_false(validate("yes", "checkbox"))
  })

  it("coerces toggle to logical", {
    expect_true(validate(TRUE, "toggle"))
    expect_false(validate(NULL, "toggle"))
  })

  it("coerces text to character", {
    expect_equal(validate("hello", "text"), "hello")
    expect_equal(validate(NULL, "text"), "")
    expect_equal(validate(123, "text"), "123")
  })
})


# ── .extract_col_values ──────────────────────────────────────────────────────

describe(".extract_col_values for primitive types", {
  extract <- reactablePlus:::.extract_col_values

  sample_rows <- list(
    r1 = list(
      .row_key = "r1",
      score = 85,
      active = TRUE,
      status = "high",
      notes = "ok"
    ),
    r2 = list(
      .row_key = "r2",
      score = NA,
      active = FALSE,
      status = NA,
      notes = NULL
    ),
    r3 = list(
      .row_key = "r3",
      score = NULL,
      active = NULL,
      status = NULL,
      notes = ""
    )
  )

  it("extracts numeric values with NA for missing", {
    cs <- widget_col("score", "numeric", "Score", options = list(min = 0))
    result <- unname(extract(sample_rows, cs))
    expect_type(result, "double")
    expect_equal(result[1], 85)
    expect_true(is.na(result[2]))
    expect_true(is.na(result[3]))
  })

  it("extracts logical values for checkbox", {
    cs <- widget_col("active", "checkbox", "Active")
    result <- unname(extract(sample_rows, cs))
    expect_type(result, "logical")
    expect_true(result[1])
    expect_false(result[2])
    expect_false(result[3])
  })

  it("extracts logical values for toggle", {
    cs <- widget_col("active", "toggle", "Active")
    result <- unname(extract(sample_rows, cs))
    expect_type(result, "logical")
    expect_true(result[1])
    expect_false(result[2])
  })

  it("extracts character values for dropdown", {
    cs <- widget_col(
      "status",
      "dropdown",
      "Status",
      options = list(choices = c("high", "low"))
    )
    result <- unname(extract(sample_rows, cs))
    expect_type(result, "character")
    expect_equal(result[1], "high")
    expect_equal(result[2], "")
    expect_equal(result[3], "")
  })

  it("extracts character values for text", {
    cs <- widget_col("notes", "text", "Notes")
    result <- unname(extract(sample_rows, cs))
    expect_type(result, "character")
    expect_equal(result[1], "ok")
    expect_equal(result[2], "")
    expect_equal(result[3], "")
  })

  it("returns empty strings for non-list rows", {
    bad_rows <- list(r1 = NULL, r2 = "not a list")
    cs <- widget_col("notes", "text", "Notes")
    result <- unname(extract(bad_rows, cs))
    expect_equal(result, c("", ""))
  })
})
