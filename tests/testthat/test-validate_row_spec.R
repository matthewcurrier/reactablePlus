# test-validate_row_spec.R

describe("validate_row_spec()", {
  describe("given a valid row_spec", {
    it("returns invisibly without error", {
      expect_no_error(validate_row_spec(valid_row_spec()))
    })
  })

  # ── id_col ──────────────────────────────────────────────────────────────────

  describe("given a row_spec missing id_col", {
    it("throws an error mentioning id_col", {
      spec <- valid_row_spec()
      spec$id_col <- NULL
      expect_error(validate_row_spec(spec), regexp = "id_col")
    })
  })

  describe("given an id_col that is not a single character string", {
    it("throws an error when id_col is numeric", {
      spec <- valid_row_spec()
      spec$id_col <- 1L
      expect_error(validate_row_spec(spec), regexp = "id_col")
    })

    it("throws an error when id_col is a character vector of length > 1", {
      spec <- valid_row_spec()
      spec$id_col <- c("grade_id", "other_id")
      expect_error(validate_row_spec(spec), regexp = "id_col")
    })

    it("throws an error when id_col is an empty string", {
      spec <- valid_row_spec()
      spec$id_col <- ""
      expect_error(validate_row_spec(spec), regexp = "id_col")
    })
  })

  # ── display_cols ─────────────────────────────────────────────────────────────

  describe("given a row_spec missing display_cols", {
    it("throws an error mentioning display_cols", {
      spec <- valid_row_spec()
      spec$display_cols <- NULL
      expect_error(validate_row_spec(spec), regexp = "display_cols")
    })
  })

  describe("given display_cols that is not a list", {
    it("throws an error when display_cols is a character vector", {
      spec <- valid_row_spec()
      spec$display_cols <- "grade_label"
      expect_error(validate_row_spec(spec), regexp = "display_cols")
    })
  })

  describe("given display_cols that is an empty list", {
    it("throws an error", {
      spec <- valid_row_spec()
      spec$display_cols <- list()
      expect_error(validate_row_spec(spec), regexp = "display_cols")
    })
  })

  describe("given a display_col entry missing col_name", {
    it("throws an error mentioning col_name", {
      spec <- valid_row_spec()
      spec$display_cols[[1]]$col_name <- NULL
      expect_error(validate_row_spec(spec), regexp = "col_name")
    })
  })

  describe("given a display_col entry missing label", {
    it("throws an error mentioning label", {
      spec <- valid_row_spec()
      spec$display_cols[[1]]$label <- NULL
      expect_error(validate_row_spec(spec), regexp = "label")
    })
  })

  describe("given a display_col entry where col_name is not a string", {
    it("throws an error", {
      spec <- valid_row_spec()
      spec$display_cols[[1]]$col_name <- 99L
      expect_error(validate_row_spec(spec), regexp = "col_name")
    })
  })

  describe("given a display_col entry where label is not a string", {
    it("throws an error", {
      spec <- valid_row_spec()
      spec$display_cols[[1]]$label <- TRUE
      expect_error(validate_row_spec(spec), regexp = "label")
    })
  })

  describe("given a row_spec with multiple display_cols, all valid", {
    it("returns invisibly without error", {
      spec <- valid_row_spec()
      spec$display_cols <- list(
        list(col_name = "grade_label", label = "Grade"),
        list(col_name = "school_name", label = "School")
      )
      expect_no_error(validate_row_spec(spec))
    })
  })

  # ── selectable ────────────────────────────────────────────────────────────────

  describe("given a row_spec with selectable = TRUE", {
    it("returns invisibly without error", {
      expect_no_error(validate_row_spec(selectable_row_spec()))
    })
  })

  describe("given a row_spec with selectable = FALSE", {
    it("returns invisibly without error", {
      spec <- valid_row_spec()
      spec$selectable <- FALSE
      expect_no_error(validate_row_spec(spec))
    })
  })

  describe("given a row_spec with selectable omitted", {
    it("returns invisibly without error — selectable is optional", {
      expect_no_error(validate_row_spec(valid_row_spec()))
    })
  })

  describe("given a row_spec with selectable that is not logical", {
    it("throws an error when selectable is a string", {
      spec <- valid_row_spec()
      spec$selectable <- "yes"
      expect_error(validate_row_spec(spec), regexp = "selectable")
    })

    it("throws an error when selectable is numeric", {
      spec <- valid_row_spec()
      spec$selectable <- 1L
      expect_error(validate_row_spec(spec), regexp = "selectable")
    })

    it("throws an error when selectable is a logical vector of length > 1", {
      spec <- valid_row_spec()
      spec$selectable <- c(TRUE, FALSE)
      expect_error(validate_row_spec(spec), regexp = "selectable")
    })
  })
})
