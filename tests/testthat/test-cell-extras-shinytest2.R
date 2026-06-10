# Browser-level regression test for the text_extra value-return contract
# and the reconcile-on-repaint behavior. Requires shinytest2 + a headless
# Chrome; skipped automatically where those are unavailable (e.g. CRAN).

test_that("text_extra reports a keyed list and survives reconcile on sort", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("reactable")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome), "No Chrome/Chromium available")

  # The app lives in its own directory so the shinytest2 child process loads
  # the package explicitly via library(reactablePlus) (see apps/text-extra/).
  app <- shinytest2::AppDriver$new(
    test_path("apps", "text-extra"),
    name = "text-extra-reconcile",
    timeout = 20000
  )
  on.exit(app$stop(), add = TRUE)

  # Wait for reactable to paint all three editable cells (store seeds here).
  app$wait_for_js(
    "document.querySelectorAll('.rp-extra').length === 3",
    timeout = 10000
  )

  # (1) Seeding: input$comment is a named list keyed by row index, equal to
  # the original column before any edit.
  seeded <- app$get_value(input = "comment")
  expect_length(seeded, 3L)
  expect_equal(seeded[["1"]], "alpha")
  expect_equal(seeded[["2"]], "beta")
  expect_equal(seeded[["3"]], "gamma")

  # (2) Edit row 2 directly in the DOM. The store binding uses
  # Shiny.setInputValue (not a Shiny input binding), so set_inputs() cannot
  # drive it — we set the element value and dispatch the input event.
  app$run_js(paste0(
    "var el = document.querySelector('.rp-extra[data-row=\"2\"]');",
    "el.value = 'EDITED';",
    "el.dispatchEvent(new Event('input', {bubbles:true}));"
  ))
  app$wait_for_idle()

  edited <- app$get_value(input = "comment")
  expect_equal(edited[["2"]], "EDITED")
  expect_equal(edited[["1"]], "alpha") # untouched rows unchanged

  # (3) Reconcile: sort by the non-editable Product column. reactable
  # repaints every cell from its own (original) data — the store must
  # re-apply 'EDITED' to the freshly painted row-2 input.
  app$run_js(paste0(
    "var ths = document.querySelectorAll('.rt-th');",
    "for (var i = 0; i < ths.length; i++) {",
    "  if (ths[i].textContent.indexOf('Product') > -1) { ths[i].click(); break; }",
    "}"
  ))
  app$wait_for_idle()

  # The repainted cell (selected by stable data-row, not DOM position) must
  # still show the edit, not the original 'beta'.
  app$wait_for_js(
    paste0(
      "var e = document.querySelector('.rp-extra[data-row=\"2\"]');",
      "e && e.value === 'EDITED'"
    ),
    timeout = 5000
  )
  reconciled <- app$get_js(
    "document.querySelector('.rp-extra[data-row=\"2\"]').value"
  )
  expect_equal(reconciled, "EDITED")

  # And the reported input still carries the edit after the repaint.
  expect_equal(app$get_value(input = "comment")[["2"]], "EDITED")
})


test_that("typed *_extra factories coerce values to the right R types", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("reactable")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome), "No Chrome/Chromium available")

  app <- shinytest2::AppDriver$new(
    test_path("apps", "extra-types"),
    name = "extra-types-coercion",
    timeout = 20000
  )
  on.exit(app$stop(), add = TRUE)

  # 2 rows x 4 editable columns = 8 cells.
  app$wait_for_js(
    "document.querySelectorAll('.rp-extra').length === 8",
    timeout = 10000
  )

  # (1) Seeded values arrive as the correct R types, not strings.
  qty <- app$get_value(input = "qty")
  # JS has no int/float distinction, so whole numbers arrive as integer and
  # fractional ones as double — both are numeric, which is the contract.
  expect_true(is.numeric(qty[["1"]]))
  expect_equal(qty[["1"]], 5)
  expect_equal(qty[["2"]], 10)

  active <- app$get_value(input = "active")
  expect_type(active[["1"]], "logical")
  expect_true(active[["1"]])
  expect_false(active[["2"]])

  status <- app$get_value(input = "status")
  expect_type(status[["1"]], "character")
  expect_equal(status[["1"]], "a")

  enabled <- app$get_value(input = "enabled")
  expect_type(enabled[["1"]], "logical")
  expect_false(enabled[["1"]])
  expect_true(enabled[["2"]])

  # (2) Editing a numeric cell reports a number, not a string.
  app$run_js(paste0(
    "var el = document.querySelector('.rp-extra[data-rp-type=\"numeric\"]",
    "[data-row=\"1\"]');",
    "el.value = '42';",
    "el.dispatchEvent(new Event('input', {bubbles:true}));"
  ))
  app$wait_for_idle()
  qty2 <- app$get_value(input = "qty")
  expect_true(is.numeric(qty2[["1"]]))
  expect_equal(qty2[["1"]], 42)

  # (3) Toggling a checkbox reports a logical.
  app$run_js(paste0(
    "document.querySelector('.rp-extra[data-rp-type=\"checkbox\"]",
    "[data-row=\"2\"]').click();"
  ))
  app$wait_for_idle()
  active2 <- app$get_value(input = "active")
  expect_type(active2[["2"]], "logical")
  expect_true(active2[["2"]])

  # (4) Clicking a toggle flips and reports a logical.
  app$run_js(paste0(
    "(function(){var n=document.querySelectorAll('.rp-extra');",
    "for(var i=0;i<n.length;i++){",
    "if(n[i].getAttribute('data-input-id')==='enabled' &&",
    "n[i].getAttribute('data-row')==='1'){ n[i].click(); break; }}})()"
  ))
  app$wait_for_idle()
  enabled2 <- app$get_value(input = "enabled")
  expect_type(enabled2[["1"]], "logical")
  expect_true(enabled2[["1"]])
})


test_that("update_extra pushes server values into the store, input and DOM", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("reactable")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome), "No Chrome/Chromium available")

  app <- shinytest2::AppDriver$new(
    test_path("apps", "extras-set"),
    name = "extras-set",
    timeout = 20000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_js(
    "document.querySelectorAll('.rp-extra').length === 6",
    timeout = 10000
  )

  # Baseline seeded values.
  expect_equal(app$get_value(input = "comment")[["1"]], "a")
  expect_equal(app$get_value(input = "comment")[["2"]], "b")

  # Fire the server-side update_extra() calls.
  app$run_js("document.getElementById('set').click();")
  app$wait_for_idle()

  # (1) The reported inputs reflect the server-set values; untouched rows stay.
  comment <- app$get_value(input = "comment")
  expect_equal(comment[["2"]], "server-set")
  expect_equal(comment[["1"]], "a")
  expect_equal(app$get_value(input = "qty")[["1"]], 99)
  expect_true(app$get_value(input = "active")[["1"]])

  # (2) The live DOM was updated too (store-push touches the element).
  dom_comment <- app$get_js(paste0(
    "(function(){var n=document.querySelectorAll('.rp-extra');",
    "for(var i=0;i<n.length;i++){",
    "if(n[i].getAttribute('data-input-id')==='comment' &&",
    "n[i].getAttribute('data-row')==='2') return n[i].value;}",
    "return null;})()"
  ))
  expect_equal(dom_comment, "server-set")

  dom_active <- app$get_js(paste0(
    "(function(){var n=document.querySelectorAll('.rp-extra');",
    "for(var i=0;i<n.length;i++){",
    "if(n[i].getAttribute('data-input-id')==='active' &&",
    "n[i].getAttribute('data-row')==='1') return n[i].checked;}",
    "return null;})()"
  ))
  expect_true(isTRUE(dom_active))
})
