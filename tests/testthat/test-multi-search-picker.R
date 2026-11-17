# tests/testthat/test-multi-search-picker.R
#
# One table, two search pickers (a district and a school). Each fills only
# its own column, and the search function learns which picker asked and on
# which row, so it can answer both and narrow one by the other.

two_picker_config <- function() {
  table_config(
    row_keys = c("A", "B", "C"),
    row_labels = c("A", "B", "C"),
    columns = list(
      widget_col("district", "search_picker", "District"),
      widget_col("school", "search_picker", "School")
    ),
    interactions = list(
      fill_down = list(
        list(column = "district", input_name = "district_fill_down"),
        list(column = "school", input_name = "school_fill_down")
      )
    ),
    search_fn_col = c("district", "school"),
    to_output_fn = function(row_state, row_key) {
      data.frame(
        key = row_key,
        district = row_state$district$id %||% NA_character_,
        school = row_state$school$id %||% NA_character_
      )
    }
  )
}

test_that(".fill_down_specs() takes one spec or a list of them", {
  one <- table_config(
    row_keys = "A", row_labels = "A",
    columns = list(widget_col("school", "search_picker", "School")),
    interactions = list(fill_down = list(column = "school"))
  )
  specs <- reactablePlus:::.fill_down_specs(one)
  expect_length(specs, 1L)
  expect_identical(specs[[1]]$input_name, "school_fill_down")

  two <- reactablePlus:::.fill_down_specs(two_picker_config())
  expect_identical(
    vapply(two, `[[`, "", "input_name"),
    c("district_fill_down", "school_fill_down")
  )
  expect_null(reactablePlus:::.fill_down_spec_for(one, "district"))
})

test_that("each picker column fills only itself", {
  shiny::testServer(
    config_table_server,
    args = list(config = two_picker_config()),
    {
      session$flushReact()
      district <- list(id = "0299901", name = "Wonderland")
      session$setInputs(
        district_fill_down = list(from_grade = "A", school = district)
      )
      out <- get_data()
      # A is the source and is empty itself; fill reaches the rows after it.
      expect_identical(out$district, c(NA, "0299901", "0299901"))
      expect_true(all(is.na(out$school)))

      school <- list(id = "17", name = "Rabbit Hole Elementary")
      session$setInputs(
        school_fill_down = list(from_grade = "B", school = school)
      )
      out <- get_data()
      expect_identical(out$school, c(NA, NA, "17"))
      expect_identical(out$district, c(NA, "0299901", "0299901"))
    }
  )
})

test_that("search_fn with a context argument learns the picker and row", {
  seen <- NULL
  search <- function(query, limit, context) {
    seen <<- context
    data.frame(id = "1", name = "x")
  }
  shiny::testServer(
    config_table_server,
    args = list(config = two_picker_config(), search_fn = search),
    {
      session$flushReact() # the search observer ignores its first value
      session$setInputs(school_search = list(
        query = "", request_id = "r1", source_id = "school_B",
        column = "school", row_key = "B", browse = TRUE
      ))
      expect_identical(seen$column, "school")
      expect_identical(seen$row_key, "B")
      expect_true(seen$browse)
    }
  )
})

test_that("an empty query is ignored unless it is a browse", {
  calls <- 0L
  search <- function(query, limit) {
    calls <<- calls + 1L
    data.frame()
  }
  shiny::testServer(
    config_table_server,
    args = list(config = two_picker_config(), search_fn = search),
    {
      session$flushReact() # the search observer ignores its first value
      session$setInputs(school_search = list(
        query = "", request_id = "r1", source_id = "school_A"
      ))
      expect_identical(calls, 0L)
      session$setInputs(school_search = list(
        query = "", request_id = "r2", source_id = "school_A", browse = TRUE
      ))
      expect_identical(calls, 1L)
    }
  )
})

test_that("searchPickerInput writes the column, fill input and display flags", {
  html <- as.character(searchPickerInput(
    "district_A",
    column = "district",
    fill_input = "district_fill_down",
    show_type_pill = FALSE,
    browse_on_open = TRUE
  ))
  expect_match(html, 'data-column="district"', fixed = TRUE)
  expect_match(html, 'data-fill-input="district_fill_down"', fixed = TRUE)
  expect_match(html, 'data-show-type-pill="false"', fixed = TRUE)
  expect_match(html, 'data-browse-on-open="true"', fixed = TRUE)

  default <- as.character(searchPickerInput("school_A"))
  expect_match(default, 'data-fill-input="school_fill_down"', fixed = TRUE)
  expect_false(grepl("data-column", default, fixed = TRUE))
})
