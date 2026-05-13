# test-merge_existing_data.R

describe("merge_existing_data()", {

  # ── NULL existing_data ────────────────────────────────────────────────────────

  describe("given existing_data = NULL", {

    it("returns NA for every col_spec column", {
      result <- merge_existing_data(
        data_df       = valid_grades_df(),
        existing_data = NULL,
        row_spec      = valid_row_spec(),
        col_spec      = valid_col_spec()
      )
      col_names <- purrr::map_chr(valid_col_spec(), \(s) s$col_name)
      na_check  <- purrr::map_lgl(col_names, \(col) all(is.na(result[[col]])))
      expect_true(all(na_check))
    })

    it("preserves all rows from data_df", {
      result <- merge_existing_data(
        data_df       = valid_grades_df(),
        existing_data = NULL,
        row_spec      = valid_row_spec(),
        col_spec      = valid_col_spec()
      )
      expect_equal(nrow(result), nrow(valid_grades_df()))
    })

    it("preserves display column values from data_df", {
      result <- merge_existing_data(
        data_df       = valid_grades_df(),
        existing_data = NULL,
        row_spec      = valid_row_spec(),
        col_spec      = valid_col_spec()
      )
      expect_equal(result$grade_label, valid_grades_df()$grade_label)
    })

    it("preserves the id_col values from data_df", {
      result <- merge_existing_data(
        data_df       = valid_grades_df(),
        existing_data = NULL,
        row_spec      = valid_row_spec(),
        col_spec      = valid_col_spec()
      )
      expect_equal(result$grade_id, valid_grades_df()$grade_id)
    })
  })

  # ── Empty existing_data data frame ────────────────────────────────────────────

  describe("given an empty existing_data data frame", {
    it("returns NA for all col_spec columns, same as NULL", {
      empty_existing <- tibble::tibble(
        grade_id    = integer(),
        attendance  = character(),
        suspensions = integer()
      )
      result <- merge_existing_data(
        data_df       = valid_grades_df(),
        existing_data = empty_existing,
        row_spec      = valid_row_spec(),
        col_spec      = valid_col_spec()
      )
      expect_true(all(is.na(result$attendance)))
      expect_true(all(is.na(result$suspensions)))
    })
  })

  # ── Partial existing_data ─────────────────────────────────────────────────────

  describe("given existing_data that covers a subset of rows", {

    it("fills in attendance from existing_data where grade_id matches", {
      result <- merge_existing_data(
        data_df       = valid_grades_df(),
        existing_data = valid_existing_data(),
        row_spec      = valid_row_spec(),
        col_spec      = valid_col_spec()
      )
      expect_equal(result$attendance[result$grade_id == 6L], "good")
      expect_equal(result$attendance[result$grade_id == 7L], "satisfactory")
    })

    it("fills in suspensions from existing_data where grade_id matches", {
      result <- merge_existing_data(
        data_df       = valid_grades_df(),
        existing_data = valid_existing_data(),
        row_spec      = valid_row_spec(),
        col_spec      = valid_col_spec()
      )
      expect_equal(result$suspensions[result$grade_id == 6L], 0L)
      expect_equal(result$suspensions[result$grade_id == 7L], 2L)
    })

    it("leaves NA for rows in data_df absent from existing_data", {
      result <- merge_existing_data(
        data_df       = valid_grades_df(),
        existing_data = valid_existing_data(),
        row_spec      = valid_row_spec(),
        col_spec      = valid_col_spec()
      )
      expect_true(is.na(result$attendance[result$grade_id == 8L]))
      expect_true(is.na(result$suspensions[result$grade_id == 8L]))
    })
  })

  # ── Extra rows in existing_data ───────────────────────────────────────────────

  describe("given existing_data with rows whose id is not in data_df", {
    it("does not add extra rows to the result", {
      extra_existing <- tibble::add_row(
        valid_existing_data(),
        grade_id    = 99L,
        attendance  = "good",
        suspensions = 0L
      )
      result <- merge_existing_data(
        data_df       = valid_grades_df(),
        existing_data = extra_existing,
        row_spec      = valid_row_spec(),
        col_spec      = valid_col_spec()
      )
      expect_equal(nrow(result), nrow(valid_grades_df()))
    })

    it("does not include the extra id in the result", {
      extra_existing <- tibble::add_row(
        valid_existing_data(),
        grade_id    = 99L,
        attendance  = "good",
        suspensions = 0L
      )
      result <- merge_existing_data(
        data_df       = valid_grades_df(),
        existing_data = extra_existing,
        row_spec      = valid_row_spec(),
        col_spec      = valid_col_spec()
      )
      expect_false(99L %in% result$grade_id)
    })
  })

  # ── Return type ───────────────────────────────────────────────────────────────

  describe("return type", {
    it("always returns a tibble", {
      result <- merge_existing_data(
        data_df       = valid_grades_df(),
        existing_data = NULL,
        row_spec      = valid_row_spec(),
        col_spec      = valid_col_spec()
      )
      expect_s3_class(result, "tbl_df")
    })

    it("contains exactly the id_col, display_cols, and col_spec columns — no extras", {
      result <- merge_existing_data(
        data_df       = valid_grades_df(),
        existing_data = NULL,
        row_spec      = valid_row_spec(),
        col_spec      = valid_col_spec()
      )
      display_col_names <- purrr::map_chr(
        valid_row_spec()$display_cols,
        \(d) d$col_name
      )
      input_col_names <- purrr::map_chr(valid_col_spec(), \(s) s$col_name)
      expected_names  <- c(valid_row_spec()$id_col, display_col_names, input_col_names)

      expect_setequal(names(result), expected_names)
    })
  })
})
