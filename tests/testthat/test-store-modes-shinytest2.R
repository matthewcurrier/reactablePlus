# Browser tests for the store re-plumb under appendable + dynamic modes, where
# the row set changes at runtime. These exercise the single per-column store
# observer (wired once) covering rows that did not exist at wiring time, plus
# .sync_store_cells keeping the store authoritative across re-renders.

`%||%` <- function(a, b) if (is.null(a)) b else a

# Find a store cell by column input id + row, set its value, fire its event.
.set_store_cell <- function(app, input_id, row, val = NULL, evt = "input",
                            check = NULL) {
  js <- if (!is.null(check)) {
    sprintf(paste0(
      "(function(){var n=document.querySelectorAll('.rp-extra');",
      "for(var i=0;i<n.length;i++){",
      "if(n[i].getAttribute('data-input-id')==='%s' &&",
      "n[i].getAttribute('data-row')==='%s'){",
      "n[i].checked=%s;n[i].dispatchEvent(new Event('%s',{bubbles:true}));",
      "return 'ok';}}return 'no';})()"
    ), input_id, row, tolower(check), evt)
  } else {
    sprintf(paste0(
      "(function(){var n=document.querySelectorAll('.rp-extra');",
      "for(var i=0;i<n.length;i++){",
      "if(n[i].getAttribute('data-input-id')==='%s' &&",
      "n[i].getAttribute('data-row')==='%s'){",
      "n[i].value='%s';n[i].dispatchEvent(new Event('%s',{bubbles:true}));",
      "return 'ok';}}return 'no';})()"
    ), input_id, row, val, evt)
  }
  app$run_js(js)
  app$wait_for_idle()
}


test_that("appendable: runtime-added rows edit through the store; reset clears", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("jsonlite")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome), "No Chrome/Chromium available")

  app <- shinytest2::AppDriver$new(
    test_path("apps", "store-appendable"),
    name = "store-appendable", timeout = 20000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_js(
    "document.querySelectorAll('.rp-extra[data-input-id=\"t-qty\"]').length === 1",
    timeout = 10000
  )
  rows <- function() jsonlite::fromJSON(app$get_value(output = "o"))
  expect_equal(nrow(rows()), 1L)

  # Add a row, then discover the new row's key from its qty cell.
  app$run_js("document.getElementById('t-add_row').click();")
  app$wait_for_idle()
  app$wait_for_js(
    "document.querySelectorAll('.rp-extra[data-input-id=\"t-qty\"]').length === 2",
    timeout = 5000
  )
  new_key <- app$get_js(paste0(
    "(function(){var n=document.querySelectorAll(",
    "'.rp-extra[data-input-id=\"t-qty\"]');",
    "return n[1].getAttribute('data-row');})()"
  ))
  expect_true(nzchar(new_key))

  # Edit the runtime-added row's store cells (covered by the once-wired
  # per-column observer).
  .set_store_cell(app, "t-qty", new_key, "7", "input")
  .set_store_cell(app, "t-fruit", new_key, "b", "change")
  after <- rows()
  expect_equal(nrow(after), 2L)
  expect_equal(after$qty[2], 7)
  expect_equal(after$fruit[2], "b")

  # Reset clears back to a single blank row (store synced).
  app$run_js("document.getElementById('t-reset').click();")
  app$wait_for_idle()
  reset_rows <- rows()
  expect_equal(nrow(reset_rows), 1L)
  expect_true(is.na(reset_rows$fruit[1]))
})


test_that("dynamic: source_data change preserves edited rows and seeds new", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("jsonlite")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome), "No Chrome/Chromium available")

  app <- shinytest2::AppDriver$new(
    test_path("apps", "store-dynamic"),
    name = "store-dynamic", timeout = 20000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_js(
    "document.querySelectorAll('.rp-extra[data-input-id=\"t-status\"]').length === 2",
    timeout = 10000
  )
  rows <- function() jsonlite::fromJSON(app$get_value(output = "o"))
  expect_equal(nrow(rows()), 2L)

  # Edit p1 (keyed by row id).
  .set_store_cell(app, "t-status", "p1", "a", "change")
  .set_store_cell(app, "t-score", "p1", "88", "input")
  r0 <- rows()
  expect_equal(r0$status[r0$id == "p1"], "a")
  expect_equal(r0$score[r0$id == "p1"], 88)

  # Change source_data (add Carol). p1's edits must survive the re-derive,
  # and p3 must appear seeded.
  app$run_js("document.getElementById('more').click();")
  app$wait_for_idle()
  app$wait_for_js(
    "document.querySelectorAll('.rp-extra[data-input-id=\"t-status\"]').length === 3",
    timeout = 5000
  )
  r1 <- rows()
  expect_equal(nrow(r1), 3L)
  expect_equal(r1$status[r1$id == "p1"], "a")  # preserved
  expect_equal(r1$score[r1$id == "p1"], 88)    # preserved
  expect_true(is.na(r1$status[r1$id == "p3"])) # new row seeded empty

  # The preserved value is also visible in the DOM after the re-render.
  dom_p1 <- app$get_js(paste0(
    "(function(){var n=document.querySelectorAll('.rp-extra');",
    "for(var i=0;i<n.length;i++){",
    "if(n[i].getAttribute('data-input-id')==='t-status' &&",
    "n[i].getAttribute('data-row')==='p1') return n[i].value;}return null;})()"
  ))
  expect_equal(dom_p1, "a")
})
