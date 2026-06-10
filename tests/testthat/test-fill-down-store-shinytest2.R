# Fill-down onto a store-backed column: picking a value in r1 and clicking its
# fill link cascades it into the empty rows below, in both get_data and the DOM.

test_that("fill-down cascades a store-column value into rows below", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("reactable")
  skip_if_not_installed("jsonlite")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome), "No Chrome/Chromium available")

  app <- shinytest2::AppDriver$new(
    test_path("apps", "fill-down-store"),
    name = "fill-down-store",
    timeout = 20000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_js(
    "document.querySelectorAll('.rp-extra[data-input-id=\"t-status\"]').length === 3",
    timeout = 10000
  )

  rows <- function() jsonlite::fromJSON(app$get_value(output = "o"))

  # Baseline: nothing set.
  expect_true(all(is.na(rows()$status)))

  # Set r1 = Active via the store.
  app$run_js(paste0(
    "(function(){var n=document.querySelectorAll('.rp-extra');",
    "for(var i=0;i<n.length;i++){",
    "if(n[i].getAttribute('data-input-id')==='t-status' &&",
    "n[i].getAttribute('data-row')==='r1'){",
    "n[i].value='a';n[i].dispatchEvent(new Event('change',{bubbles:true}));break;}}})()"
  ))
  app$wait_for_idle()
  expect_equal(rows()$status[rows()$row == "r1"], "a")
  expect_true(is.na(rows()$status[rows()$row == "r2"]))

  # Click r1's fill-down link.
  clicked <- app$get_js(paste0(
    "(function(){var w=document.querySelectorAll('.rp-fill-wrap');",
    "for(var i=0;i<w.length;i++){",
    "var sel=w[i].querySelector('.rp-extra[data-row=\"r1\"]');",
    "if(sel){w[i].querySelector('.rp-fill-down').click();return 'ok';}}",
    "return 'no-link';})()"
  ))
  expect_equal(clicked, "ok")
  app$wait_for_idle()

  # get_data: the value cascaded into r2 and r3.
  after <- rows()
  expect_equal(after$status[after$row == "r1"], "a")
  expect_equal(after$status[after$row == "r2"], "a")
  expect_equal(after$status[after$row == "r3"], "a")

  # DOM: r2's select now shows the cascaded value (store-push updated it).
  dom_r2 <- app$get_js(paste0(
    "(function(){var n=document.querySelectorAll('.rp-extra');",
    "for(var i=0;i<n.length;i++){",
    "if(n[i].getAttribute('data-input-id')==='t-status' &&",
    "n[i].getAttribute('data-row')==='r2') return n[i].value;}return null;})()"
  ))
  expect_equal(dom_r2, "a")
})
