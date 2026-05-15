# tests/testthat/test-cascading-choices.R
#
# Phase 4 tests — cascading (dynamic) dropdown choices.
# Covers: widget_col() validation with choices_fn / choices_depends_on,
# table_config() auto-wiring of triggers_rerender, and backward
# compatibility with static-only choices.

# ── widget_col: choices_fn validation ────────────────────────────────────────

test_that("widget_col dropdown accepts choices_fn with static choices", {
  wc <- widget_col(
    "city",
    "dropdown",
    "City",
    options = list(
      choices = c("Default" = "default"),
      choices_fn = function(row_state) c("A" = "a", "B" = "b")
    )
  )

  expect_s3_class(wc, "widget_col")
  expect_true(is.function(wc$options$choices_fn))
  # Static choices should still be normalized
  expect_equal(wc$options$choices[[1]]$label, "Default")
})


test_that("widget_col dropdown accepts choices_fn without static choices", {
  wc <- widget_col(
    "city",
    "dropdown",
    "City",
    options = list(
      choices_fn = function(row_state) c("X", "Y", "Z")
    )
  )

  expect_s3_class(wc, "widget_col")
  expect_true(is.function(wc$options$choices_fn))
  # Use [[ to avoid $ partial-matching "choices" to "choices_fn"
  expect_null(wc$options[["choices"]])
})


test_that("widget_col dropdown rejects missing both choices and choices_fn", {
  expect_error(
    widget_col("city", "dropdown", "City", options = list()),
    "choices is required"
  )
})


test_that("widget_col dropdown rejects non-function choices_fn", {
  expect_error(
    widget_col(
      "city",
      "dropdown",
      "City",
      options = list(choices_fn = "not a function")
    ),
    "choices_fn must be a function"
  )
})


test_that("widget_col dropdown accepts choices_depends_on", {
  wc <- widget_col(
    "city",
    "dropdown",
    "City",
    options = list(
      choices_fn = function(row_state) c("A", "B"),
      choices_depends_on = "state"
    )
  )

  expect_equal(wc$options$choices_depends_on, "state")
})


test_that("widget_col dropdown accepts multiple choices_depends_on", {
  wc <- widget_col(
    "city",
    "dropdown",
    "City",
    options = list(
      choices_fn = function(row_state) c("A", "B"),
      choices_depends_on = c("state", "country")
    )
  )

  expect_equal(wc$options$choices_depends_on, c("state", "country"))
})


test_that("widget_col dropdown rejects non-character choices_depends_on", {
  expect_error(
    widget_col(
      "city",
      "dropdown",
      "City",
      options = list(
        choices_fn = function(row_state) c("A"),
        choices_depends_on = 42
      )
    ),
    "non-empty character vector"
  )
})


test_that("widget_col dropdown rejects empty choices_depends_on", {
  expect_error(
    widget_col(
      "city",
      "dropdown",
      "City",
      options = list(
        choices_fn = function(row_state) c("A"),
        choices_depends_on = character(0)
      )
    ),
    "non-empty character vector"
  )
})


# ── table_config: auto-wiring triggers_rerender ──────────────────────────────

test_that("table_config auto-marks choices_depends_on columns as triggers_rerender", {
  cfg <- table_config(
    row_keys = c("r1", "r2"),
    row_labels = c("R1", "R2"),
    columns = list(
      widget_col(
        "state",
        "dropdown",
        "State",
        options = list(choices = c("CA", "NY", "TX"))
      ),
      widget_col(
        "city",
        "dropdown",
        "City",
        options = list(
          choices_fn = function(row_state) {
            switch(
              row_state$state %||% "",
              "CA" = c("LA", "SF"),
              "NY" = c("NYC", "Buffalo"),
              c("-- pick state --")
            )
          },
          choices_depends_on = "state"
        )
      )
    )
  )

  # "state" should be auto-marked as triggers_rerender
  state_col <- cfg$columns[[1]]
  expect_true(state_col$triggers_rerender)

  # "city" should not be auto-marked (it's the dependent, not controller)
  city_col <- cfg$columns[[2]]
  expect_false(city_col$triggers_rerender)
})


test_that("table_config rejects choices_depends_on referencing non-existent column", {
  expect_error(
    table_config(
      row_keys = "r1",
      row_labels = "R1",
      columns = list(
        widget_col(
          "city",
          "dropdown",
          "City",
          options = list(
            choices_fn = function(row_state) c("A"),
            choices_depends_on = "nonexistent"
          )
        )
      )
    ),
    "do not exist"
  )
})


test_that("table_config combines gate and choices_depends_on wiring", {
  cfg <- table_config(
    row_keys = c("r1"),
    row_labels = c("R1"),
    columns = list(
      widget_col(
        "category",
        "dropdown",
        "Category",
        options = list(choices = c("Food", "Drink"))
      ),
      widget_col(
        "item",
        "dropdown",
        "Item",
        options = list(
          choices_fn = function(row_state) {
            if (identical(row_state$category, "Food")) {
              c("Pizza", "Burger")
            } else {
              c("Water", "Juice")
            }
          },
          choices_depends_on = "category"
        )
      ),
      widget_col(
        "rating",
        "numeric",
        "Rating",
        options = list(min = 1, max = 5),
        gate = list(
          list(type = "value", col_id = "category", values = "Food")
        )
      )
    )
  )

  # "category" should be triggers_rerender from BOTH gate and choices_depends_on
  cat_col <- cfg$columns[[1]]
  expect_true(cat_col$triggers_rerender)
})


# ── Backward compatibility ───────────────────────────────────────────────────

test_that("static-only dropdown still works unchanged", {
  wc <- widget_col(
    "color",
    "dropdown",
    "Color",
    options = list(choices = c("Red", "Blue", "Green"))
  )

  expect_s3_class(wc, "widget_col")
  expect_null(wc$options$choices_fn)
  expect_null(wc$options$choices_depends_on)
  expect_length(wc$options$choices, 3L)
})


test_that("existing test suite pattern: named vector choices normalize correctly", {
  wc <- widget_col(
    "priority",
    "dropdown",
    "Priority",
    options = list(choices = c("High" = "H", "Medium" = "M"))
  )

  expect_equal(wc$options$choices[[1]]$label, "High")
  expect_equal(wc$options$choices[[1]]$value, "H")
})
