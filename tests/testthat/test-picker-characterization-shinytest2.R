# Characterization of popover-picker behavior in the config module. The
# get_data assertions are the invariant the store port must preserve. Driving
# uses the popover's setValue + a 'change' event, which both the current
# binding and the ported (store-backed) version respond to.

test_that("picker: pick and clear flow through to get_data", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("jsonlite")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome), "No Chrome/Chromium available")

  app <- shinytest2::AppDriver$new(
    test_path("apps", "picker-characterization"),
    name = "picker-characterization", timeout = 20000
  )
  on.exit(app$stop(), add = TRUE)

  # Wait until the picker popover instances are initialized.
  app$wait_for_js(
    "typeof $ !== 'undefined' && $('#t-hs_r1').data('sh-popover') != null",
    timeout = 10000
  )

  rows <- function() jsonlite::fromJSON(app$get_value(output = "o"))
  r1 <- function() rows()[rows()$row == "r1", ]

  # Baseline: empty.
  expect_true(is.na(r1()$by))

  # Pick a homeschool value on r1.
  app$run_js(paste0(
    "var el=document.getElementById('t-hs_r1');",
    "$(el).data('sh-popover').setValue({by:'Mother', curriculum:'Sonlight'});",
    "$(el).trigger('change');"
  ))
  app$wait_for_idle()
  expect_equal(r1()$by, "Mother")
  expect_equal(r1()$curriculum, "Sonlight")

  # Other row untouched.
  expect_true(is.na(rows()$by[rows()$row == "r2"]))

  # Clear r1.
  app$run_js(paste0(
    "var el=document.getElementById('t-hs_r1');",
    "$(el).data('sh-popover').setValue(null);",
    "$(el).trigger('change');"
  ))
  app$wait_for_idle()
  expect_true(is.na(r1()$by))

  # ── Attendance picker: the value-semantics edge (empty == {} internally,
  # normalized to null by getValue). Setting a section value must flow to
  # get_data, and clearing must read back as empty again.
  expect_true(is.na(r1()$school))

  app$run_js(paste0(
    "var el=document.getElementById('t-att_r1');",
    "$(el).data('sh-popover').setValue({school:'Excellent'});",
    "$(el).trigger('change');"
  ))
  app$wait_for_idle()
  expect_equal(r1()$school, "Excellent")
  expect_true(is.na(rows()$school[rows()$row == "r2"]))

  # Clearing attendance back to the empty object normalizes to null.
  app$run_js(paste0(
    "var el=document.getElementById('t-att_r1');",
    "$(el).data('sh-popover').setValue({});",
    "$(el).trigger('change');"
  ))
  app$wait_for_idle()
  expect_true(is.na(r1()$school))
})
