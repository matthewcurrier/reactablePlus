# Characterization tests for the config-driven table module. The get_data /
# selected_ids assertions capture behavior the store re-plumb must preserve
# (value collection, gating, selection, Reset clearing). The DRIVING targets
# the store-backed cells (data-input-id / data-row); the assertions are the
# mechanism-agnostic invariant.

test_that("config module: value collection, gating, selection, reset", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("reactable")
  skip_if_not_installed("jsonlite")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome), "No Chrome/Chromium available")

  app <- shinytest2::AppDriver$new(
    test_path("apps", "config-characterization"),
    name = "config-characterization",
    timeout = 20000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_js(
    "document.querySelector('.rp-extra[data-input-id=\"tbl-mode\"]') !== null",
    timeout = 10000
  )

  # Helpers ------------------------------------------------------------------
  # Store cells: find by data-input-id + data-row, set value/checked, fire the
  # event the store binding listens for.
  app$run_js(paste0(
    "window.__setStore = function(inputId, row, val, evt, check){",
    "var n = document.querySelectorAll('.rp-extra'); var el=null;",
    "for(var i=0;i<n.length;i++){",
    "if(n[i].getAttribute('data-input-id')===inputId &&",
    "n[i].getAttribute('data-row')===row){el=n[i];break;}}",
    "if(!el) return 'NO '+inputId;",
    "if(check!==undefined && check!==null){ el.checked=check; }",
    "else if(val!==undefined && val!==null){ el.value=val; }",
    "el.dispatchEvent(new Event(evt, {bubbles:true}));",
    "return 'ok'; };"
  ))

  set_store <- function(col, row, val = NULL, evt = "input", check = NULL) {
    js <- if (!is.null(check)) {
      sprintf("__setStore('tbl-%s','%s',null,'%s',%s)", col, row, evt,
              tolower(check))
    } else {
      sprintf("__setStore('tbl-%s','%s','%s','%s')", col, row, val, evt)
    }
    app$run_js(js)
    app$wait_for_idle()
  }

  store_val <- function(col, row, prop = "value") {
    app$get_js(sprintf(paste0(
      "(function(){var n=document.querySelectorAll('.rp-extra');",
      "for(var i=0;i<n.length;i++){",
      "if(n[i].getAttribute('data-input-id')==='tbl-%s' &&",
      "n[i].getAttribute('data-row')==='%s') return n[i].%s;}return null;})()"
    ), col, row, prop))
  }

  rows <- function() jsonlite::fromJSON(app$get_value(output = "data_out"))
  r1 <- function() rows()[rows()$row == "r1", ]

  # ── Baseline ──────────────────────────────────────────────────────────────
  base <- rows()
  expect_equal(nrow(base), 2L)
  expect_true(is.na(base$mode[base$row == "r1"]))
  expect_equal(base$score[base$row == "r1"], 0) # gate closed -> empty_value
  expect_equal(base$detail[base$row == "r1"], "")

  # ── Value collection across primitive types ───────────────────────────────
  set_store("note", "r1", "hello", "input")
  set_store("flag", "r1", evt = "change", check = TRUE)
  set_store("detail", "r1", "keepme", "input")
  expect_equal(r1()$note, "hello")
  expect_true(r1()$flag)
  expect_equal(r1()$detail, "keepme")

  # ── Gating: score collected only while the gate (mode in a/b) is open ─────
  set_store("mode", "r1", "a", "change")
  expect_equal(r1()$mode, "a")
  set_store("score", "r1", "50", "input")
  expect_equal(r1()$score, 50) # gate open -> value flows through
  set_store("mode", "r1", "", "change") # back to placeholder -> gate closes
  expect_true(is.na(r1()$mode))
  expect_equal(r1()$score, 0) # gate closed -> forced to empty_value

  # ── Selection (rendered separately, not a store cell) ─────────────────────
  app$run_js(paste0(
    "var s=document.getElementById('tbl-.selected_r1');",
    "s.checked=true; s.dispatchEvent(new Event('change',{bubbles:true}));"
  ))
  app$wait_for_idle()
  sel <- jsonlite::fromJSON(app$get_value(output = "sel_out"))
  expect_true("r1" %in% sel)

  # ── Reset: server clears all cells (the reconcile-fight case) ─────────────
  # get_data returns to defaults AND the displayed store inputs visually clear
  # (the latter only works because Reset is routed through update_extra).
  app$run_js("document.getElementById('tbl-reset').click();")
  app$wait_for_idle()

  after <- r1()
  expect_true(is.na(after$mode))
  expect_equal(after$score, 0)
  expect_equal(after$note, "")
  expect_false(after$flag)
  expect_equal(after$detail, "")
  sel_after <- jsonlite::fromJSON(app$get_value(output = "sel_out"))
  expect_false("r1" %in% sel_after)

  # Displayed store cells cleared, not just the output.
  expect_equal(store_val("note", "r1", "value"), "")
  expect_false(isTRUE(store_val("flag", "r1", "checked")))
})
