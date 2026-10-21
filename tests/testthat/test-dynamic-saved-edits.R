# tests/testthat/test-dynamic-saved-edits.R
#
# A dynamic table (source_data) seeded from saved data (data_r) must keep the
# user's edits on saved rows when source_data changes. It used to re-apply
# data_r() to every saved row on each source_data change, so adding a row
# reverted every unsaved edit on every saved row.

saved_edits_config <- function() {
  table_config(
    row_id_col = "id",
    row_label_col = "label",
    columns = list(
      widget_col(
        "when", "custom", "When",
        render_cell_fn = function(ns, row_key, row, cs, settings) ""
      )
    ),
    from_saved_fn = function(db_row, col_specs) list(when = db_row$when),
    to_output_fn = function(row_state, row_key) {
      data.frame(id = row_key, when = row_state$when %||% NA_character_)
    }
  )
}

source_rows <- function(ids) data.frame(id = ids, label = ids)

test_that("adding a row keeps an edit made to a saved row", {
  ids <- shiny::reactiveVal(c("saved1", "new1"))
  saved <- data.frame(id = "saved1", when = "2024-03-07")

  shiny::testServer(
    config_table_server,
    args = list(
      config = saved_edits_config(),
      source_data = shiny::reactive(source_rows(ids())),
      data_r = shiny::reactive(saved)
    ),
    {
      session$flushReact() # first render: rows seeded, cell observers wired
      out <- function() stats::setNames(get_data()$when, get_data()$id)
      expect_identical(out()[["saved1"]], "2024-03-07")

      session$setInputs(when_saved1 = "2022-11-15", when_new1 = "2023-05-09")
      expect_identical(out()[["saved1"]], "2022-11-15")

      ids(c("saved1", "new1", "new2")) # "Add a row"
      session$flushReact()

      expect_identical(out()[["saved1"]], "2022-11-15")
      expect_identical(out()[["new1"]], "2023-05-09")
      expect_true(is.na(out()[["new2"]]))
    }
  )
})

test_that("a saved row appearing for the first time is still seeded", {
  ids <- shiny::reactiveVal("new1")
  saved <- data.frame(id = "saved1", when = "2024-03-07")

  shiny::testServer(
    config_table_server,
    args = list(
      config = saved_edits_config(),
      source_data = shiny::reactive(source_rows(ids())),
      data_r = shiny::reactive(saved)
    ),
    {
      ids(c("saved1", "new1"))
      session$flushReact()
      out <- stats::setNames(get_data()$when, get_data()$id)
      expect_identical(out[["saved1"]], "2024-03-07")
    }
  )
})

test_that("new saved data still replaces the rows (opening another record)", {
  ids <- shiny::reactiveVal("saved1")
  saved <- shiny::reactiveVal(data.frame(id = "saved1", when = "2024-03-07"))

  shiny::testServer(
    config_table_server,
    args = list(
      config = saved_edits_config(),
      source_data = shiny::reactive(source_rows(ids())),
      data_r = saved
    ),
    {
      session$flushReact()
      session$setInputs(when_saved1 = "2022-11-15")
      saved(data.frame(id = "saved1", when = "2019-01-01"))
      session$flushReact()
      out <- stats::setNames(get_data()$when, get_data()$id)
      expect_identical(out[["saved1"]], "2019-01-01")
    }
  )
})
