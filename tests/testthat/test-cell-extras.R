# ── text_extra ─────────────────────────────────────────────────────────────

test_that("text_extra returns a cell render function", {
  cell <- text_extra("comment")
  expect_type(cell, "closure")
})

test_that("text_extra emits an input carrying the contract data attributes", {
  cell <- text_extra("comment")
  html <- cell("hello", 2L)
  expect_match(html, "<input")
  expect_match(html, 'class="rp-extra"')
  expect_match(html, 'data-input-id="comment"')
  expect_match(html, 'data-row="2"')
  expect_match(html, 'data-rp-type="text"')
  expect_match(html, 'value="hello"')
})

test_that("text_extra prefixes the input id with the module namespace", {
  cell <- text_extra("comment", ns = "mod-")
  html <- cell("x", 1L)
  expect_match(html, 'data-input-id="mod-comment"')
})

test_that("text_extra renders NA and NULL cell values as empty", {
  cell <- text_extra("c")
  expect_match(cell(NA, 1L), 'value=""')
  expect_match(cell(NULL, 1L), 'value=""')
})

test_that("text_extra applies placeholder and extra class", {
  cell <- text_extra("c", placeholder = "type here", class = "wide")
  html <- cell("", 1L)
  expect_match(html, 'placeholder="type here"')
  expect_match(html, 'class="rp-extra wide"')
})

test_that("text_extra coerces non-character cell values to text", {
  cell <- text_extra("qty")
  expect_match(cell(42L, 1L), 'value="42"')
})

test_that("text_extra rejects an invalid col_id", {
  expect_error(text_extra(""), "col_id")
  expect_error(text_extra(c("a", "b")), "col_id")
})

# ── numeric_extra ────────────────────────────────────────────────────────────

test_that("numeric_extra emits a number input with the numeric contract type", {
  cell <- numeric_extra("qty")
  html <- cell(5, 2L)
  expect_match(html, 'type="number"')
  expect_match(html, 'data-rp-type="numeric"')
  expect_match(html, 'data-input-id="qty"')
  expect_match(html, 'data-row="2"')
  expect_match(html, 'value="5"')
})

test_that("numeric_extra applies min, max and step attributes", {
  cell <- numeric_extra("qty", min = 0, max = 100, step = 0.5)
  html <- cell(1, 1L)
  expect_match(html, 'min="0"')
  expect_match(html, 'max="100"')
  expect_match(html, 'step="0.5"')
})

test_that("numeric_extra omits unset bounds and renders NA as empty", {
  cell <- numeric_extra("qty")
  html <- cell(NA, 1L)
  expect_no_match(html, "min=")
  expect_no_match(html, "step=")
  expect_match(html, 'value=""')
})

# ── date_extra ───────────────────────────────────────────────────────────────

test_that("date_extra emits a date input carrying the date contract type", {
  cell <- date_extra("registered")
  html <- cell("2024-01-10", 3L)
  expect_match(html, 'type="date"')
  expect_match(html, 'data-rp-type="date"')
  expect_match(html, 'value="2024-01-10"')
  expect_match(html, 'data-row="3"')
})

test_that("date_extra coerces a Date value and bounds to ISO strings", {
  cell <- date_extra("d", min_date = as.Date("2020-01-01"))
  html <- cell(as.Date("2024-06-09"), 1L)
  expect_match(html, 'value="2024-06-09"')
  expect_match(html, 'min="2020-01-01"')
})

# ── checkbox_extra ───────────────────────────────────────────────────────────

test_that("checkbox_extra marks the box checked only for TRUE values", {
  cell <- checkbox_extra("active")
  on  <- cell(TRUE, 1L)
  off <- cell(FALSE, 2L)
  expect_match(on, 'type="checkbox"')
  expect_match(on, 'data-rp-type="checkbox"')
  expect_match(on, "checked")
  expect_no_match(off, "checked")
})

test_that("checkbox_extra treats NA and NULL as unchecked", {
  cell <- checkbox_extra("active")
  expect_no_match(cell(NA, 1L), "checked")
  expect_no_match(cell(NULL, 1L), "checked")
})

# ── dropdown_extra ───────────────────────────────────────────────────────────

test_that("dropdown_extra renders a select with the dropdown contract type", {
  cell <- dropdown_extra("status", choices = c("Active", "Inactive"))
  html <- cell("Active", 1L)
  expect_match(html, "<select")
  expect_match(html, 'data-rp-type="dropdown"')
  expect_match(html, 'data-input-id="status"')
  expect_match(html, "<option")
  expect_match(html, "Inactive")
})

test_that("dropdown_extra marks the matching option selected", {
  cell <- dropdown_extra("status", choices = c("Active" = "a", "Inactive" = "i"))
  html <- cell("i", 1L)
  # The selected attribute should sit on the option whose value is "i".
  expect_match(html, '<option value="i" selected')
})

test_that("dropdown_extra prepends a placeholder option when supplied", {
  cell <- dropdown_extra("status", choices = c("Active"),
                         placeholder = "-- pick --")
  html <- cell(NA, 1L)
  expect_match(html, "-- pick --")
  expect_match(html, 'disabled')
})

# ── bindExtrasOnRender ───────────────────────────────────────────────────────

test_that("bindExtrasOnRender attaches a render hook invoking rpExtrasOnReady", {
  tbl <- reactable::reactable(data.frame(comment = "a"))
  bound <- bindExtrasOnRender(tbl)
  hooks <- bound$jsHooks$render
  expect_true(length(hooks) >= 1L)
  expect_match(hooks[[1]]$code, "rpExtrasOnReady")
})

test_that("bindExtrasOnRender threads the fallback timeout into the hook", {
  tbl <- reactable::reactable(data.frame(comment = "a"))
  bound <- bindExtrasOnRender(tbl, fallback_ms = 250L)
  expect_match(bound$jsHooks$render[[1]]$code, "250")
})

# ── toggle_extra ─────────────────────────────────────────────────────────────

test_that("toggle_extra renders a button carrying the toggle contract", {
  cell <- toggle_extra("enabled")
  on  <- cell(TRUE, 1L)
  off <- cell(FALSE, 2L)
  expect_match(on, "<button")
  expect_match(on, 'type="button"')
  expect_match(on, 'data-rp-type="toggle"')
  expect_match(on, 'data-input-id="enabled"')
  expect_match(on, 'data-rp-value="true"')
  expect_match(on, 'aria-pressed="true"')
  expect_match(on, ">On<")
  expect_match(off, 'data-rp-value="false"')
  expect_match(off, ">Off<")
})

test_that("toggle_extra honors custom labels", {
  cell <- toggle_extra("x", on_label = "Yes", off_label = "No")
  expect_match(cell(TRUE, 1L), ">Yes<")
  expect_match(cell(TRUE, 1L), 'data-on-label="Yes"')
  expect_match(cell(FALSE, 1L), 'data-off-label="No"')
})

test_that("toggle_extra treats NA and NULL as off", {
  cell <- toggle_extra("x")
  expect_match(cell(NA, 1L), 'data-rp-value="false"')
  expect_match(cell(NULL, 1L), 'data-rp-value="false"')
})

# ── notes_extra ──────────────────────────────────────────────────────────────

test_that("notes_extra renders a text input with the cell-input class", {
  cell <- notes_extra("comment")
  html <- cell("hi", 2L)
  expect_match(html, 'type="text"')
  expect_match(html, 'data-rp-type="text"')
  expect_match(html, "rp-extra cell-input")
  expect_match(html, 'data-row="2"')
  expect_match(html, 'value="hi"')
})

# ── update_extra ─────────────────────────────────────────────────────────────

test_that("update_extra sends a well-formed rp_extras_set message", {
  sent <- NULL
  fake_session <- list(
    sendCustomMessage = function(type, message) {
      sent <<- list(type = type, message = message)
    }
  )
  update_extra(fake_session, "comment", list(`2` = "x", `3` = "y"), ns = "mod-")
  expect_equal(sent$type, "rp_extras_set")
  expect_equal(sent$message$inputId, "mod-comment")
  expect_equal(sent$message$values, list(`2` = "x", `3` = "y"))
})

test_that("update_extra defaults to an unprefixed input id", {
  sent <- NULL
  fake_session <- list(
    sendCustomMessage = function(type, message) sent <<- message
  )
  update_extra(fake_session, "qty", list(`1` = 5))
  expect_equal(sent$inputId, "qty")
})

test_that("update_extra errors without a session", {
  expect_error(update_extra(NULL, "c", list()), "Shiny session")
})

test_that("update_extra rejects an invalid col_id", {
  fake <- list(sendCustomMessage = function(...) NULL)
  expect_error(update_extra(fake, "", list()), "col_id")
})

# ── dependency wiring ────────────────────────────────────────────────────────

test_that("useReactablePlus includes the reactable-extras dependency", {
  deps <- useReactablePlus()
  dep_names <- vapply(deps, function(d) d$name, character(1))
  expect_true("reactable-extras" %in% dep_names)
})
