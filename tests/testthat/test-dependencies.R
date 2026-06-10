# ── Dependency objects ────────────────────────────────────────────────────

test_that("popoverDep returns an htmlDependency", {
  dep <- popoverDep()
  expect_s3_class(dep, "html_dependency")
  expect_equal(dep$name, "popover-core")
})

test_that("popoverDep points to existing files", {
  dep <- popoverDep()
  src_dir <- dep$src$file
  expect_true(dir.exists(src_dir))

  js_file <- file.path(src_dir, dep$script)
  expect_true(file.exists(js_file))

  css_file <- file.path(src_dir, dep$stylesheet)
  expect_true(file.exists(css_file))
})


# ── useReactablePlus ──────────────────────────────────────────────────────

test_that("useReactablePlus returns a tagList of dependencies", {
  result <- useReactablePlus()
  expect_s3_class(result, "shiny.tag.list")

  deps <- Filter(function(x) inherits(x, "html_dependency"), result)
  expect_true(length(deps) >= 5)

  dep_names <- vapply(deps, `[[`, character(1), "name")
  expect_true("popover-core" %in% dep_names)
  expect_true("attendance-picker" %in% dep_names)
  expect_true("school-picker" %in% dep_names)
  expect_true("homeschool-picker" %in% dep_names)
  expect_true("notes-input" %in% dep_names)
  expect_true("gear-popover" %in% dep_names)
})

test_that("all dependency JS files exist on disk", {
  result <- useReactablePlus()
  all_deps <- Filter(function(x) inherits(x, "html_dependency"), result)

  for (dep in all_deps) {
    src_dir <- dep$src$file
    if (!is.null(dep$script)) {
      js_path <- file.path(src_dir, dep$script)
      expect_true(
        file.exists(js_path),
        info = paste("Missing JS:", dep$name, dep$script)
      )
    }
    if (!is.null(dep$stylesheet)) {
      css_path <- file.path(src_dir, dep$stylesheet)
      expect_true(
        file.exists(css_path),
        info = paste("Missing CSS:", dep$name, dep$stylesheet)
      )
    }
  }
})


# ── useSchoolHistory (deprecated alias) ──────────────────────────────────

test_that("useSchoolHistory emits a deprecation warning", {
  expect_warning(
    useSchoolHistory(),
    regexp = "deprecated"
  )
})

test_that("useSchoolHistory still returns valid dependencies", {
  result <- suppressWarnings(useSchoolHistory())
  expect_s3_class(result, "shiny.tag.list")

  deps <- Filter(function(x) inherits(x, "html_dependency"), result)
  dep_names <- vapply(deps, `[[`, character(1), "name")
  expect_true("popover-core" %in% dep_names)
})


# ── bindPickersOnRender ──────────────────────────────────────────────────

test_that("bindPickersOnRender requires htmlwidgets", {
  # Should not error — htmlwidgets is a dependency
  expect_true(requireNamespace("htmlwidgets", quietly = TRUE))
})

test_that("bindPickersOnRender emits shBindPickersOnReady call", {
  # Build a minimal reactable widget so onRender has something to attach to
  tbl <- reactable::reactable(data.frame(x = 1L))
  result <- bindPickersOnRender(tbl)

  # The onRender JS string should reference the MutationObserver entry point,
  # not the old raw setTimeout path
  js <- result$jsHooks$render[[1]]$code
  expect_match(js, "shBindPickersOnReady", fixed = TRUE)
  expect_no_match(js, "setTimeout")
})

test_that("bindPickersOnRender passes fallback_ms to JS", {
  tbl <- reactable::reactable(data.frame(x = 1L))

  result_default <- bindPickersOnRender(tbl)
  js_default <- result_default$jsHooks$render[[1]]$code
  expect_match(js_default, "600", fixed = TRUE)

  result_custom <- bindPickersOnRender(tbl, fallback_ms = 1200L)
  js_custom <- result_custom$jsHooks$render[[1]]$code
  expect_match(js_custom, "1200", fixed = TRUE)
})

test_that("bindPickersOnRender rejects non-integer fallback_ms gracefully", {
  tbl <- reactable::reactable(data.frame(x = 1L))
  # as.integer() coerces — 500.9 becomes 500; no error expected
  expect_no_error(bindPickersOnRender(tbl, fallback_ms = 500.9))
})
