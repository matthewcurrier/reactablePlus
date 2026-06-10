# search_picker fill-down through the *_extra store. The store port routes
# picker fill-down via update_extra -> the JS picker write handler (setValue),
# replacing the pre-port sendInputMessage path. This pins that cascade.

test_that("picker fill-down cascades through get_data and the picker DOM", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("jsonlite")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome), "No Chrome/Chromium available")

  app <- shinytest2::AppDriver$new(
    test_path("apps", "picker-filldown"),
    name = "picker-filldown", timeout = 20000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_js(
    "typeof $ !== 'undefined' && $('#t-school_r1').data('sh-popover') != null",
    timeout = 10000
  )

  rows <- function() jsonlite::fromJSON(app$get_value(output = "o"))
  filled_pickers <- function() {
    app$get_js("document.querySelectorAll('.sh-school-picker .pv-summary').length")
  }

  # Baseline: nothing filled.
  expect_true(all(is.na(rows()$school)))
  expect_equal(filled_pickers(), 0)

  # Pick a school on r1.
  school <- "{id:'S1', name:'Alpha Academy', low_grade:'r1', high_grade:'r4'}"
  app$run_js(paste0(
    "var el=document.getElementById('t-school_r1');",
    "$(el).data('sh-popover').setValue(", school, ");",
    "$(el).trigger('change');"
  ))
  app$wait_for_idle()
  expect_equal(rows()$school[rows()$row == "r1"], "Alpha Academy")
  expect_equal(filled_pickers(), 1)

  # Fire the fill-down input the picker's down-arrow sends.
  app$run_js(paste0(
    "Shiny.setInputValue('t-school_fill_down',",
    "{from_grade:'r1', school:", school, "}, {priority:'event'});"
  ))
  app$wait_for_idle()

  # Every row now holds the school — in get_data AND in the rendered pickers
  # (the store write handler drove each popover's setValue).
  expect_true(all(rows()$school == "Alpha Academy"))
  expect_equal(filled_pickers(), 4)
})
