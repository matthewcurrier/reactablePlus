# tests/testthat/test-constructors.R

# ── display_col ──────────────────────────────────────────────────────────────

test_that("display_col creates a valid object", {
  dc <- display_col("name", "Student Name")
  expect_s3_class(dc, "display_col")
  expect_equal(dc$col_name, "name")
  expect_equal(dc$label, "Student Name")
})

test_that("display_col rejects missing col_name", {
  expect_error(display_col(label = "Name"), "col_name")
})

test_that("display_col rejects empty col_name", {
  expect_error(display_col("", "Name"), "must not be empty")
})

test_that("display_col rejects missing label", {
  expect_error(display_col("name"), "label")
})

test_that("display_col rejects empty label", {
  expect_error(display_col("name", ""), "must not be empty")
})

test_that("display_col prints cleanly", {
  dc <- display_col("name", "Name")
  expect_output(print(dc), "display_col")
  expect_output(print(dc), "name")
})


# ── row_def ──────────────────────────────────────────────────────────────────

test_that("row_def creates a valid object", {
  rd <- row_def("id", list(display_col("name", "Name")))
  expect_s3_class(rd, "row_def")
  expect_equal(rd$id_col, "id")
  expect_length(rd$display_cols, 1)
  expect_null(rd$selectable)
})

test_that("row_def accepts selectable flag", {
  rd <- row_def("id", list(display_col("name", "Name")), selectable = TRUE)
  expect_true(rd$selectable)
})

test_that("row_def rejects empty id_col", {
  expect_error(row_def("", list(display_col("x", "X"))), "must not be empty")
})

test_that("row_def rejects empty display_cols", {
  expect_error(row_def("id", list()), "at least one entry")
})

test_that("row_def rejects display_cols missing required fields", {
  expect_error(
    row_def("id", list(list(col_name = "x"))),
    "must have.*col_name.*label"
  )
})

test_that("row_def works with plain lists in display_cols", {
  rd <- row_def("id", list(list(col_name = "x", label = "X")))
  expect_s3_class(rd, "row_def")
})

test_that("row_def prints cleanly", {
  rd <- row_def("id", list(display_col("name", "Name")))
  expect_output(print(rd), "row_def")
  expect_output(print(rd), "1 column")
})


# ── input_col ────────────────────────────────────────────────────────────────

test_that("input_col creates a dropdown with vector choices", {
  ic <- input_col("role", "Role", "dropdown", choices = c("A", "B", "C"))
  expect_s3_class(ic, "input_col")
  expect_equal(ic$type, "dropdown")
  # Choices should be normalized to list-of-lists
  expect_equal(ic$choices[[1]], list(label = "A", value = "A"))
})

test_that("input_col creates a dropdown with named vector choices", {
  ic <- input_col("status", "Status", "dropdown",
                  choices = c("Active" = "active", "Inactive" = "off"))
  expect_equal(ic$choices[[1]], list(label = "Active", value = "active"))
  expect_equal(ic$choices[[2]], list(label = "Inactive", value = "off"))
})

test_that("input_col creates a dropdown with list-of-lists choices", {
  ic <- input_col("score", "Score", "dropdown",
                  choices = list(
                    list(label = "Good", value = 3),
                    list(label = "Bad", value = 1)
                  ))
  expect_equal(ic$choices[[1]]$value, 3)
})

test_that("input_col creates a numeric column", {
  ic <- input_col("score", "Score", "numeric", min = 0, max = 100, step = 5)
  expect_equal(ic$type, "numeric")
  expect_equal(ic$min, 0)
  expect_equal(ic$max, 100)
  expect_equal(ic$step, 5)
})

test_that("input_col creates a checkbox column", {
  ic <- input_col("active", "Active", "checkbox")
  expect_equal(ic$type, "checkbox")
  expect_null(ic$choices)
})

test_that("input_col creates a text column", {
  ic <- input_col("notes", "Notes", "text", max_chars = 500)
  expect_equal(ic$max_chars, 500)
})

test_that("input_col creates a date column", {
  ic <- input_col("dob", "Date of Birth", "date",
                  min_date = "1900-01-01", max_date = "2025-12-31")
  expect_equal(ic$min_date, "1900-01-01")
})

test_that("input_col rejects missing col_name", {
  expect_error(input_col(label = "X", type = "text"), "col_name")
})

test_that("input_col rejects unsupported type", {
  expect_error(input_col("x", "X", "fancy"), "must be one of")
})

test_that("input_col rejects dropdown without choices", {
  expect_error(input_col("x", "X", "dropdown"), "choices is required")
})

test_that("input_col rejects numeric without min", {
  expect_error(input_col("x", "X", "numeric"), "min is required")
})

test_that("input_col omits NULL optional fields", {
  ic <- input_col("x", "X", "checkbox")
  expect_false("choices" %in% names(ic))
  expect_false("min" %in% names(ic))
  expect_false("gate" %in% names(ic))
})

test_that("input_col prints cleanly", {
  ic <- input_col("role", "Role", "dropdown", choices = c("A", "B"))
  expect_output(print(ic), "input_col")
  expect_output(print(ic), "dropdown")
  expect_output(print(ic), "A, B")
})


# ── Integration with editable_table_server validators ────────────────────────

test_that("validate_row_spec accepts a row_def object", {
  rd <- row_def("id", list(display_col("name", "Name")))
  expect_invisible(validate_row_spec(rd))
})

test_that("validate_col_spec accepts input_col objects", {
  cols <- list(
    input_col("role", "Role", "dropdown", choices = c("A", "B")),
    input_col("score", "Score", "numeric", min = 0)
  )
  result <- validate_col_spec(cols)
  expect_length(result, 2)
})

test_that("editable_table_server accepts constructors (smoke test)", {
  # Just verify the constructors produce objects that pass validation
  rd <- row_def("id", list(display_col("name", "Name")))
  cols <- list(
    input_col("status", "Status", "dropdown", choices = c("A", "B")),
    input_col("count", "Count", "numeric", min = 0)
  )

  validate_row_spec(rd)
  result <- validate_col_spec(cols, rd)
  expect_length(result, 2)
})
