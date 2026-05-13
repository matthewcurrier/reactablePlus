# test-validate_row_inputs.R
# validate_row_inputs(df, row_spec, col_spec) returns a list:
#   $valid  — logical
#   $errors — list of lists, each with $row_id, $col_name, $message

describe("validate_row_inputs()", {

  # ── Fully valid inputs ────────────────────────────────────────────────────────

  describe("given a fully valid input data frame", {
    it("returns valid == TRUE", {
      result <- validate_row_inputs(valid_inputs_df(), valid_row_spec(), valid_col_spec())
      expect_true(result$valid)
    })

    it("returns an empty errors list", {
      result <- validate_row_inputs(valid_inputs_df(), valid_row_spec(), valid_col_spec())
      expect_length(result$errors, 0L)
    })
  })

  # ── Numeric: NA ───────────────────────────────────────────────────────────────

  describe("given a numeric input that is NA", {
    it("returns valid == FALSE", {
      df                    <- valid_inputs_df()
      df$suspensions[[2L]]  <- NA_integer_
      result <- validate_row_inputs(df, valid_row_spec(), valid_col_spec())
      expect_false(result$valid)
    })

    it("reports the failing row_id in errors", {
      df                    <- valid_inputs_df()
      df$suspensions[[2L]]  <- NA_integer_
      result      <- validate_row_inputs(df, valid_row_spec(), valid_col_spec())
      failing_ids <- purrr::map_int(result$errors, \(e) e$row_id)
      expect_true(7L %in% failing_ids)
    })

    it("reports suspensions as the failing col_name", {
      df                   <- valid_inputs_df()
      df$suspensions[[2L]] <- NA_integer_
      result       <- validate_row_inputs(df, valid_row_spec(), valid_col_spec())
      failing_cols <- purrr::map_chr(result$errors, \(e) e$col_name)
      expect_true("suspensions" %in% failing_cols)
    })
  })

  # ── Numeric: below min ────────────────────────────────────────────────────────

  describe("given a numeric input below the column's min", {
    it("returns valid == FALSE", {
      df                   <- valid_inputs_df()
      df$suspensions[[1L]] <- -1L
      result <- validate_row_inputs(df, valid_row_spec(), valid_col_spec())
      expect_false(result$valid)
    })

    it("reports the failing row_id in errors", {
      df                   <- valid_inputs_df()
      df$suspensions[[1L]] <- -1L
      result      <- validate_row_inputs(df, valid_row_spec(), valid_col_spec())
      failing_ids <- purrr::map_int(result$errors, \(e) e$row_id)
      expect_true(6L %in% failing_ids)
    })
  })

  # ── Numeric: non-integer value ────────────────────────────────────────────────

  describe("given a numeric input that is not a whole number", {
    it("returns valid == FALSE for a decimal value", {
      df                   <- valid_inputs_df()
      df$suspensions[[1L]] <- 1.5
      result <- validate_row_inputs(df, valid_row_spec(), valid_col_spec())
      expect_false(result$valid)
    })
  })

  # ── Dropdown: NA ──────────────────────────────────────────────────────────────

  describe("given a dropdown input that is NA", {
    it("returns valid == FALSE", {
      df                  <- valid_inputs_df()
      df$attendance[[3L]] <- NA_character_
      result <- validate_row_inputs(df, valid_row_spec(), valid_col_spec())
      expect_false(result$valid)
    })

    it("reports the failing row_id in errors", {
      df                  <- valid_inputs_df()
      df$attendance[[3L]] <- NA_character_
      result      <- validate_row_inputs(df, valid_row_spec(), valid_col_spec())
      failing_ids <- purrr::map_int(result$errors, \(e) e$row_id)
      expect_true(8L %in% failing_ids)
    })

    it("reports attendance as the failing col_name", {
      df                  <- valid_inputs_df()
      df$attendance[[3L]] <- NA_character_
      result       <- validate_row_inputs(df, valid_row_spec(), valid_col_spec())
      failing_cols <- purrr::map_chr(result$errors, \(e) e$col_name)
      expect_true("attendance" %in% failing_cols)
    })
  })

  # ── Dropdown: value not in choices ────────────────────────────────────────────

  describe("given a dropdown input whose value is not in the col_spec choices", {
    it("returns valid == FALSE", {
      df                  <- valid_inputs_df()
      df$attendance[[1L]] <- "excellent"   # not a valid choice value
      result <- validate_row_inputs(df, valid_row_spec(), valid_col_spec())
      expect_false(result$valid)
    })

    it("reports the failing row_id in errors", {
      df                  <- valid_inputs_df()
      df$attendance[[1L]] <- "excellent"
      result      <- validate_row_inputs(df, valid_row_spec(), valid_col_spec())
      failing_ids <- purrr::map_int(result$errors, \(e) e$row_id)
      expect_true(6L %in% failing_ids)
    })
  })

  # ── Multiple failing rows ─────────────────────────────────────────────────────

  describe("given multiple invalid rows", {
    it("returns valid == FALSE", {
      df                   <- valid_inputs_df()
      df$suspensions[[1L]] <- -1L
      df$attendance[[2L]]  <- NA_character_
      result <- validate_row_inputs(df, valid_row_spec(), valid_col_spec())
      expect_false(result$valid)
    })

    it("reports an error entry for each failing row-column pair", {
      df                   <- valid_inputs_df()
      df$suspensions[[1L]] <- -1L
      df$attendance[[2L]]  <- NA_character_
      result <- validate_row_inputs(df, valid_row_spec(), valid_col_spec())
      expect_gte(length(result$errors), 2L)
    })

    it("reports the correct row_ids for all failing rows", {
      df                   <- valid_inputs_df()
      df$suspensions[[1L]] <- -1L
      df$attendance[[2L]]  <- NA_character_
      result      <- validate_row_inputs(df, valid_row_spec(), valid_col_spec())
      failing_ids <- purrr::map_int(result$errors, \(e) e$row_id)
      expect_true(6L %in% failing_ids)
      expect_true(7L %in% failing_ids)
    })
  })

  # ── Error structure ───────────────────────────────────────────────────────────

  describe("each error entry", {
    it("contains row_id, col_name, and message fields", {
      df                   <- valid_inputs_df()
      df$suspensions[[1L]] <- -1L
      result <- validate_row_inputs(df, valid_row_spec(), valid_col_spec())
      error  <- result$errors[[1L]]
      expect_true(all(c("row_id", "col_name", "message") %in% names(error)))
    })

    it("includes a non-empty message string", {
      df                   <- valid_inputs_df()
      df$suspensions[[1L]] <- -1L
      result  <- validate_row_inputs(df, valid_row_spec(), valid_col_spec())
      message <- result$errors[[1L]]$message
      expect_type(message, "character")
      expect_gt(nchar(message), 0L)
    })
  })

  # ── Date column ───────────────────────────────────────────────────────────────

  describe("given a date column", {

    # Helper: a single-column col_spec with just the date entry
    date_col_spec <- function() list(valid_date_col_spec_entry())

    # Helper: a matching 3-row input df
    date_inputs_df <- function(dates = as.Date(c("2023-09-01", "2023-09-01", "2023-09-01"))) {
      tibble::tibble(grade_id = c(6L, 7L, 8L), enrollment_date = dates)
    }

    it("returns valid == TRUE for a fully populated date column", {
      result <- validate_row_inputs(date_inputs_df(), valid_row_spec(), date_col_spec())
      expect_true(result$valid)
    })

    it("returns valid == FALSE when a date value is NA", {
      df                        <- date_inputs_df()
      df$enrollment_date[[2L]]  <- NA
      result <- validate_row_inputs(df, valid_row_spec(), date_col_spec())
      expect_false(result$valid)
    })

    it("reports the correct row_id when a date is NA", {
      df                       <- date_inputs_df()
      df$enrollment_date[[2L]] <- NA
      result      <- validate_row_inputs(df, valid_row_spec(), date_col_spec())
      failing_ids <- purrr::map_int(result$errors, \(e) e$row_id)
      expect_true(7L %in% failing_ids)
    })

    it("returns valid == FALSE when a date is below min_date", {
      spec_with_bounds <- list(valid_date_col_spec_entry_with_bounds())
      df               <- date_inputs_df(as.Date(c("2019-01-01", "2023-09-01", "2023-09-01")))
      result <- validate_row_inputs(df, valid_row_spec(), spec_with_bounds)
      expect_false(result$valid)
    })

    it("returns valid == FALSE when a date is above max_date", {
      spec_with_bounds <- list(valid_date_col_spec_entry_with_bounds())
      df               <- date_inputs_df(as.Date(c("2023-09-01", "2031-01-01", "2023-09-01")))
      result <- validate_row_inputs(df, valid_row_spec(), spec_with_bounds)
      expect_false(result$valid)
    })
  })

  # ── Checkbox column ───────────────────────────────────────────────────────────

  describe("given a checkbox column", {

    checkbox_col_spec <- function() list(valid_checkbox_col_spec_entry())

    checkbox_inputs_df <- function(vals = c(TRUE, FALSE, TRUE)) {
      tibble::tibble(grade_id = c(6L, 7L, 8L), has_iep = vals)
    }

    it("returns valid == TRUE for TRUE and FALSE values", {
      result <- validate_row_inputs(checkbox_inputs_df(), valid_row_spec(), checkbox_col_spec())
      expect_true(result$valid)
    })

    it("returns valid == FALSE when a checkbox value is NA", {
      df              <- checkbox_inputs_df()
      df$has_iep[[1L]] <- NA
      result <- validate_row_inputs(df, valid_row_spec(), checkbox_col_spec())
      expect_false(result$valid)
    })

    it("returns valid == FALSE when a checkbox value is not logical", {
      df               <- checkbox_inputs_df()
      df$has_iep[[1L]] <- "true"
      result <- validate_row_inputs(df, valid_row_spec(), checkbox_col_spec())
      expect_false(result$valid)
    })
  })

  # ── Toggle column ─────────────────────────────────────────────────────────────

  describe("given a toggle column", {

    toggle_col_spec <- function() list(valid_toggle_col_spec_entry())

    toggle_inputs_df <- function(vals = c(TRUE, TRUE, FALSE)) {
      tibble::tibble(grade_id = c(6L, 7L, 8L), active = vals)
    }

    it("returns valid == TRUE for TRUE and FALSE values", {
      result <- validate_row_inputs(toggle_inputs_df(), valid_row_spec(), toggle_col_spec())
      expect_true(result$valid)
    })

    it("returns valid == FALSE when a toggle value is NA", {
      df              <- toggle_inputs_df()
      df$active[[2L]] <- NA
      result <- validate_row_inputs(df, valid_row_spec(), toggle_col_spec())
      expect_false(result$valid)
    })

    it("returns valid == FALSE when a toggle value is not logical", {
      df              <- toggle_inputs_df()
      df$active[[2L]] <- 1L
      result <- validate_row_inputs(df, valid_row_spec(), toggle_col_spec())
      expect_false(result$valid)
    })
  })

  # ── Text column ───────────────────────────────────────────────────────────────

  describe("given a text column", {

    text_col_spec       <- function() list(valid_text_col_spec_entry())
    text_col_spec_max   <- function() list(valid_text_col_spec_entry_with_max())

    text_inputs_df <- function(vals = c("Good student", "", "Needs support")) {
      tibble::tibble(grade_id = c(6L, 7L, 8L), notes = vals)
    }

    it("returns valid == TRUE for non-NA character values including empty string", {
      result <- validate_row_inputs(text_inputs_df(), valid_row_spec(), text_col_spec())
      expect_true(result$valid)
    })

    it("returns valid == FALSE when a text value is NA", {
      df            <- text_inputs_df()
      df$notes[[1L]] <- NA_character_
      result <- validate_row_inputs(df, valid_row_spec(), text_col_spec())
      expect_false(result$valid)
    })

    it("returns valid == TRUE when text length is within max_chars", {
      df <- text_inputs_df(c("short", "also short", "fine"))
      result <- validate_row_inputs(df, valid_row_spec(), text_col_spec_max())
      expect_true(result$valid)
    })

    it("returns valid == FALSE when text exceeds max_chars", {
      long_string    <- strrep("a", 201L)
      df             <- text_inputs_df(c(long_string, "fine", "fine"))
      result <- validate_row_inputs(df, valid_row_spec(), text_col_spec_max())
      expect_false(result$valid)
    })

    it("reports the correct row_id when text exceeds max_chars", {
      long_string    <- strrep("a", 201L)
      df             <- text_inputs_df(c("fine", long_string, "fine"))
      result      <- validate_row_inputs(df, valid_row_spec(), text_col_spec_max())
      failing_ids <- purrr::map_int(result$errors, \(e) e$row_id)
      expect_true(7L %in% failing_ids)
    })
  })
})
