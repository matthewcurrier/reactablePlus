test_that("gradeChoices returns all 14 grades", {
  gc <- gradeChoices()
  expect_length(gc, 14)
  expect_named(gc)
  expect_equal(unname(gc[1]), "PK")
  expect_equal(unname(gc[2]), "K")
  expect_equal(unname(gc[14]), "12")
})

test_that("gradeChoices values are two-char keys", {
  gc <- gradeChoices()
  # All values should be character

  expect_type(gc, "character")
  # PK, K are special; 01-12 are zero-padded
  expect_true(all(nchar(unname(gc)) <= 2))
})

test_that("gradeChoices names are display labels", {
  gc <- gradeChoices()
  expect_equal(names(gc)[1], "PreK")
  expect_equal(names(gc)[2], "K")
  expect_equal(names(gc)[3], "1st")
  expect_equal(names(gc)[14], "12th")
})

test_that("gradeLabelMap returns key-to-label mapping", {
  glm <- gradeLabelMap()
  expect_type(glm, "list")
  expect_equal(glm[["PK"]], "PreK")
  expect_equal(glm[["K"]], "K")
  expect_equal(glm[["01"]], "1st")
  expect_equal(glm[["12"]], "12th")
})

test_that("gradeLabelMap covers all 14 grades", {
  glm <- gradeLabelMap()
  expect_length(glm, 14)
  expect_true(all(unname(gradeChoices()) %in% names(glm)))
})

test_that("gradeIndex assigns correct ordinal positions", {
  expect_equal(gradeIndex("PK"), 0L)
  expect_equal(gradeIndex("K"),  1L)
  expect_equal(gradeIndex("01"), 2L)
  expect_equal(gradeIndex("05"), 6L)
  expect_equal(gradeIndex("12"), 13L)
})

test_that("gradeIndex is vectorized", {
  result <- gradeIndex(c("PK", "K", "05", "12"))
  expect_equal(result, c(0L, 1L, 6L, 13L))
})

test_that("gradeIndex ordering is monotonically increasing", {
  all_keys <- unname(gradeChoices())
  indices <- gradeIndex(all_keys)
  expect_true(all(diff(indices) > 0))
})

test_that("gradeInRange returns TRUE for grades within range", {
  expect_true(gradeInRange("05", "K", "08"))
  expect_true(gradeInRange("K", "K", "08"))
  expect_true(gradeInRange("08", "K", "08"))
  expect_true(gradeInRange("PK", "PK", "12"))
  expect_true(gradeInRange("12", "PK", "12"))
})

test_that("gradeInRange returns FALSE for grades outside range", {
  expect_false(gradeInRange("09", "K", "08"))
  expect_false(gradeInRange("PK", "K", "08"))
  expect_false(gradeInRange("PK", "01", "05"))
})

test_that("gradeInRange is vectorized", {
  result <- gradeInRange(c("PK", "05", "09"), "K", "08")
  expect_equal(result, c(FALSE, TRUE, FALSE))
})

test_that("gradeInRange handles single-grade range", {
  expect_true(gradeInRange("05", "05", "05"))
  expect_false(gradeInRange("04", "05", "05"))
  expect_false(gradeInRange("06", "05", "05"))
})
