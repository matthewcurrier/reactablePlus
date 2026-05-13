# tests/testthat/test-normalize_choices.R

# ── Unnamed character vector ─────────────────────────────────────────────────

test_that("unnamed vector creates label == value entries", {
  result <- normalize_choices(c("High", "Medium", "Low"))

  expect_length(result, 3)
  expect_equal(result[[1]], list(label = "High", value = "High"))
  expect_equal(result[[2]], list(label = "Medium", value = "Medium"))
  expect_equal(result[[3]], list(label = "Low", value = "Low"))
})

test_that("single unnamed value works", {
  result <- normalize_choices("Only")
  expect_length(result, 1)
  expect_equal(result[[1]], list(label = "Only", value = "Only"))
})


# ── Named character vector ───────────────────────────────────────────────────

test_that("named vector uses names as labels and values as values", {
  result <- normalize_choices(c("High" = "high", "Medium" = "med"))

  expect_length(result, 2)
  expect_equal(result[[1]], list(label = "High", value = "high"))
  expect_equal(result[[2]], list(label = "Medium", value = "med"))
})

test_that("named vector result has no outer names", {
  result <- normalize_choices(c("A" = "a", "B" = "b"))
  expect_null(names(result))
})


# ── List-of-lists (pass-through) ─────────────────────────────────────────────

test_that("list-of-lists is returned as-is", {
  original <- list(
    list(label = "Good", value = 3),
    list(label = "Bad", value = 1)
  )
  result <- normalize_choices(original)
  expect_identical(result, original)
})


# ── Edge cases and errors ────────────────────────────────────────────────────

test_that("empty character vector is rejected", {
  expect_error(
    normalize_choices(character(0)),
    "at least one entry"
  )
})

test_that("NA values are rejected", {
  expect_error(
    normalize_choices(c("High", NA, "Low")),
    "must not contain NA"
  )
})

test_that("empty strings are rejected", {
  expect_error(
    normalize_choices(c("High", "", "Low")),
    "must not contain empty strings"
  )
})

test_that("named vector with empty name is rejected", {
  # Build the vector indirectly to avoid parse-time zero-length name error
  bad_choices <- c(high = "high", none = "none")
  names(bad_choices)[2] <- ""
  expect_error(
    normalize_choices(bad_choices),
    "must not have empty names"
  )
})

test_that("numeric input is rejected", {
  expect_error(
    normalize_choices(1:3),
    "must be a character vector"
  )
})

test_that("logical input is rejected", {
  expect_error(
    normalize_choices(TRUE),
    "must be a character vector"
  )
})


# ── Integration with validate_col_spec ───────────────────────────────────────

test_that("validate_col_spec accepts unnamed vector choices", {
  spec <- list(
    list(
      col_name = "role",
      label = "Role",
      type = "dropdown",
      choices = c("Analyst", "Engineer", "Manager")
    )
  )

  result <- validate_col_spec(spec)
  expect_equal(
    result[[1]]$choices[[1]],
    list(label = "Analyst", value = "Analyst")
  )
})

test_that("validate_col_spec accepts named vector choices", {
  spec <- list(
    list(
      col_name = "status",
      label = "Status",
      type = "dropdown",
      choices = c("Active" = "active", "Inactive" = "inactive")
    )
  )

  result <- validate_col_spec(spec)
  expect_equal(
    result[[1]]$choices[[1]],
    list(label = "Active", value = "active")
  )
})

test_that("validate_col_spec still accepts list-of-lists choices", {
  spec <- list(
    list(
      col_name = "score",
      label = "Score",
      type = "dropdown",
      choices = list(
        list(label = "Good", value = 3),
        list(label = "Bad", value = 1)
      )
    )
  )

  result <- validate_col_spec(spec)
  expect_equal(result[[1]]$choices[[1]]$value, 3)
})

test_that("validate_col_spec returns normalized col_spec", {
  spec <- list(
    list(col_name = "x", label = "X", type = "dropdown", choices = c("A", "B")),
    list(col_name = "y", label = "Y", type = "numeric", min = 0)
  )

  result <- validate_col_spec(spec)

  # Dropdown choices should be normalized

  expect_true(is.list(result[[1]]$choices[[1]]))
  expect_equal(result[[1]]$choices[[1]]$label, "A")

  # Non-dropdown entry should be unchanged
  expect_equal(result[[2]]$type, "numeric")
})
