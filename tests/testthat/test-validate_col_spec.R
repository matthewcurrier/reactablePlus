# test-validate_col_spec.R

describe("validate_col_spec()", {
  describe("given a valid col_spec with dropdown and numeric columns", {
    it("returns invisibly without error", {
      expect_no_error(validate_col_spec(valid_col_spec()))
    })
  })

  # ── Top-level structure ───────────────────────────────────────────────────────

  describe("given a col_spec that is not a list", {
    it("throws an error when passed a character string", {
      expect_error(validate_col_spec("attendance"), regexp = "list")
    })

    it("throws an error when passed NULL", {
      expect_error(validate_col_spec(NULL), regexp = "list")
    })
  })

  describe("given an empty col_spec list", {
    it("throws an error", {
      expect_error(validate_col_spec(list()), regexp = "empty")
    })
  })

  # ── Shared required fields ────────────────────────────────────────────────────

  describe("given a col_spec entry missing col_name", {
    it("throws an error mentioning col_name", {
      spec <- valid_col_spec()
      spec[[1]]$col_name <- NULL
      expect_error(validate_col_spec(spec), regexp = "col_name")
    })
  })

  describe("given a col_spec entry missing label", {
    it("throws an error mentioning label", {
      spec <- valid_col_spec()
      spec[[1]]$label <- NULL
      expect_error(validate_col_spec(spec), regexp = "label")
    })
  })

  describe("given a col_spec entry missing type", {
    it("throws an error mentioning type", {
      spec <- valid_col_spec()
      spec[[1]]$type <- NULL
      expect_error(validate_col_spec(spec), regexp = "type")
    })
  })

  describe("given a col_spec entry with an unsupported type", {
    it("throws an error mentioning type", {
      spec <- valid_col_spec()
      spec[[1]]$type <- "checkbox"
      expect_error(validate_col_spec(spec), regexp = "type")
    })
  })

  # ── Dropdown-specific rules ───────────────────────────────────────────────────

  describe("given a dropdown col_spec entry", {
    it("throws an error when choices is missing entirely", {
      spec <- valid_col_spec()
      spec[[1]]$choices <- NULL
      expect_error(validate_col_spec(spec), regexp = "choices")
    })

    it("normalizes a plain character vector into list-of-lists", {
      spec <- valid_col_spec()
      spec[[1]]$choices <- c("Good", "Satisfactory", "Bad")
      result <- validate_col_spec(spec)
      choices <- result[[1]]$choices
      expect_true(is.list(choices))
      expect_equal(choices[[1]]$label, "Good")
      expect_equal(choices[[1]]$value, "Good")
    })

    it("throws an error when choices is an empty list", {
      spec <- valid_col_spec()
      spec[[1]]$choices <- list()
      expect_error(validate_col_spec(spec), regexp = "choices")
    })

    it("throws an error when a choice entry is missing label", {
      spec <- valid_col_spec()
      spec[[1]]$choices[[1]]$label <- NULL
      expect_error(validate_col_spec(spec), regexp = "label")
    })

    it("throws an error when a choice entry is missing value", {
      spec <- valid_col_spec()
      spec[[1]]$choices[[1]]$value <- NULL
      expect_error(validate_col_spec(spec), regexp = "value")
    })

    it("throws an error when a choice label is not a string", {
      spec <- valid_col_spec()
      spec[[1]]$choices[[1]]$label <- 1L
      expect_error(validate_col_spec(spec), regexp = "label")
    })

    it("accepts integer values in choices", {
      spec <- valid_col_spec()
      spec[[1]]$choices <- list(
        list(label = "Doctor", value = 1L),
        list(label = "Nurse", value = 2L)
      )
      expect_no_error(validate_col_spec(spec))
    })

    it("accepts character values in choices", {
      expect_no_error(validate_col_spec(valid_col_spec()))
    })

    it("throws an error when choice values are not all the same type", {
      spec <- valid_col_spec()
      spec[[1]]$choices <- list(
        list(label = "Good", value = "good"),
        list(label = "Bad", value = 2L) # mixing character and integer
      )
      expect_error(validate_col_spec(spec), regexp = "value")
    })
  })

  # ── Numeric-specific rules ────────────────────────────────────────────────────

  describe("given a numeric col_spec entry", {
    it("throws an error when min is missing", {
      spec <- valid_col_spec()
      spec[[2]]$min <- NULL
      expect_error(validate_col_spec(spec), regexp = "min")
    })

    it("throws an error when min is not numeric", {
      spec <- valid_col_spec()
      spec[[2]]$min <- "zero"
      expect_error(validate_col_spec(spec), regexp = "min")
    })

    it("accepts a numeric min of 0", {
      expect_no_error(validate_col_spec(valid_col_spec()))
    })

    it("accepts a negative numeric min", {
      spec <- valid_col_spec()
      spec[[2]]$min <- -10L
      expect_no_error(validate_col_spec(spec))
    })
  })

  # ── Multiple entries ──────────────────────────────────────────────────────────

  describe("given a col_spec where the second entry is invalid", {
    it("throws an error identifying the second entry", {
      spec <- valid_col_spec()
      spec[[2]]$type <- "radio" # unsupported type on second entry
      expect_error(validate_col_spec(spec), regexp = "type")
    })
  })

  describe("given duplicate col_name values across entries", {
    it("throws an error mentioning duplicate col_name", {
      spec <- valid_col_spec()
      spec[[2]]$col_name <- "attendance" # same as spec[[1]]
      expect_error(validate_col_spec(spec), regexp = "col_name")
    })
  })

  # ── Date-specific rules ───────────────────────────────────────────────────────

  describe("given a date col_spec entry", {
    it("accepts a minimal date entry with no bounds", {
      expect_no_error(validate_col_spec(list(valid_date_col_spec_entry())))
    })

    it("accepts a date entry with valid ISO-format min_date and max_date", {
      expect_no_error(validate_col_spec(list(valid_date_col_spec_entry_with_bounds())))
    })

    it("throws an error when min_date is not a string", {
      entry <- valid_date_col_spec_entry_with_bounds()
      entry$min_date <- 20200101L
      expect_error(validate_col_spec(list(entry)), regexp = "min_date")
    })

    it("throws an error when max_date is not a string", {
      entry <- valid_date_col_spec_entry_with_bounds()
      entry$max_date <- TRUE
      expect_error(validate_col_spec(list(entry)), regexp = "max_date")
    })

    it("throws an error when min_date is not in ISO format", {
      entry <- valid_date_col_spec_entry_with_bounds()
      entry$min_date <- "01/01/2020"
      expect_error(validate_col_spec(list(entry)), regexp = "min_date")
    })

    it("throws an error when max_date is earlier than min_date", {
      entry <- valid_date_col_spec_entry_with_bounds()
      entry$min_date <- "2030-01-01"
      entry$max_date <- "2020-01-01"
      expect_error(validate_col_spec(list(entry)), regexp = "max_date")
    })
  })

  # ── Checkbox-specific rules ───────────────────────────────────────────────────

  describe("given a checkbox col_spec entry", {
    it("accepts a valid checkbox entry", {
      expect_no_error(validate_col_spec(list(valid_checkbox_col_spec_entry())))
    })

    it("throws an error when an unexpected field is present that suggests misconfiguration", {
      entry <- valid_checkbox_col_spec_entry()
      entry$choices <- list(list(label = "Yes", value = TRUE))
      expect_error(validate_col_spec(list(entry)), regexp = "choices")
    })
  })

  # ── Toggle-specific rules ─────────────────────────────────────────────────────

  describe("given a toggle col_spec entry", {
    it("accepts a valid toggle entry", {
      expect_no_error(validate_col_spec(list(valid_toggle_col_spec_entry())))
    })

    it("throws an error when an unexpected field is present that suggests misconfiguration", {
      entry <- valid_toggle_col_spec_entry()
      entry$choices <- list(list(label = "On", value = TRUE))
      expect_error(validate_col_spec(list(entry)), regexp = "choices")
    })
  })

  # ── Text-specific rules ───────────────────────────────────────────────────────

  describe("given a text col_spec entry", {
    it("accepts a minimal text entry with no max_chars", {
      expect_no_error(validate_col_spec(list(valid_text_col_spec_entry())))
    })

    it("accepts a text entry with a valid max_chars", {
      expect_no_error(validate_col_spec(list(valid_text_col_spec_entry_with_max())))
    })

    it("throws an error when max_chars is not a whole number", {
      entry <- valid_text_col_spec_entry_with_max()
      entry$max_chars <- 1.5
      expect_error(validate_col_spec(list(entry)), regexp = "max_chars")
    })

    it("throws an error when max_chars is not positive", {
      entry <- valid_text_col_spec_entry_with_max()
      entry$max_chars <- 0L
      expect_error(validate_col_spec(list(entry)), regexp = "max_chars")
    })

    it("throws an error when max_chars is a string", {
      entry <- valid_text_col_spec_entry_with_max()
      entry$max_chars <- "200"
      expect_error(validate_col_spec(list(entry)), regexp = "max_chars")
    })
  })

  # ── All six types together ────────────────────────────────────────────────────

  describe("given a col_spec containing all six supported types", {
    it("returns invisibly without error", {
      expect_no_error(validate_col_spec(
        valid_full_col_spec(),
        valid_row_spec()
      ))
    })
  })

  # ── depends_on rejection ──────────────────────────────────────────────────────

  describe("given a col_spec entry using the old depends_on field", {
    it("throws an error telling the caller to use gate instead", {
      spec <- list(
        list(
          col_name = "progress",
          label = "Progress",
          type = "dropdown",
          choices = list(list(label = "Early", value = "early"))
        ),
        list(
          col_name = "months_offset",
          label = "+/- Months",
          type = "numeric",
          min = -24L,
          depends_on = list(col_name = "progress", values = c("early"))
        )
      )
      expect_error(validate_col_spec(spec, valid_row_spec()), regexp = "gate")
    })
  })

  # ── gate rules ────────────────────────────────────────────────────────────────

  describe("given a valid value-only gate", {
    it("accepts the value_gate_col_spec without error", {
      expect_no_error(validate_col_spec(
        value_gate_col_spec(),
        valid_row_spec()
      ))
    })
  })

  describe("given a valid selection-only gate", {
    it("accepts the selection_gate_col_spec without error", {
      expect_no_error(validate_col_spec(
        selection_gate_col_spec(),
        selectable_row_spec()
      ))
    })
  })

  describe("given a valid chained gate", {
    it("accepts the chained_gate_col_spec without error", {
      expect_no_error(validate_col_spec(
        chained_gate_col_spec(),
        selectable_row_spec()
      ))
    })
  })

  describe("given a gate that is not a list", {
    it("throws an error", {
      spec <- value_gate_col_spec()
      spec[[2L]]$gate <- "progress"
      expect_error(validate_col_spec(spec, valid_row_spec()), regexp = "gate")
    })
  })

  describe("given a gate that is an empty list", {
    it("throws an error", {
      spec <- value_gate_col_spec()
      spec[[2L]]$gate <- list()
      expect_error(validate_col_spec(spec, valid_row_spec()), regexp = "gate")
    })
  })

  describe("given a gate condition missing type", {
    it("throws an error mentioning type", {
      spec <- value_gate_col_spec()
      spec[[2L]]$gate[[1L]]$type <- NULL
      expect_error(validate_col_spec(spec, valid_row_spec()), regexp = "type")
    })
  })

  describe("given a gate condition with an unsupported type", {
    it("throws an error mentioning type", {
      spec <- value_gate_col_spec()
      spec[[2L]]$gate[[1L]]$type <- "dropdown"
      expect_error(validate_col_spec(spec, valid_row_spec()), regexp = "type")
    })
  })

  describe("given a gate condition of type 'selected' when selectable is not TRUE", {
    it("throws an error at validation time", {
      expect_error(
        validate_col_spec(selection_gate_col_spec(), valid_row_spec()),
        regexp = "selectable"
      )
    })

    it("throws an error when row_spec is NULL", {
      expect_error(
        validate_col_spec(selection_gate_col_spec(), NULL),
        regexp = "selectable"
      )
    })
  })

  describe("given a gate value condition missing col_name", {
    it("throws an error mentioning col_name", {
      spec <- value_gate_col_spec()
      spec[[2L]]$gate[[1L]]$col_name <- NULL
      expect_error(
        validate_col_spec(spec, valid_row_spec()),
        regexp = "col_name"
      )
    })
  })

  describe("given a gate value condition whose col_name does not exist in the spec", {
    it("throws an error naming the missing col_name", {
      spec <- value_gate_col_spec()
      spec[[2L]]$gate[[1L]]$col_name <- "nonexistent_col"
      expect_error(
        validate_col_spec(spec, valid_row_spec()),
        regexp = "nonexistent_col"
      )
    })
  })

  describe("given a gate value condition whose col_name refers to a later column", {
    it("throws an error — controller must be declared before dependent", {
      spec <- list(
        list(
          col_name = "months_offset",
          label = "+/- Months",
          type = "numeric",
          min = -24L,
          gate = list(list(
            type = "value",
            col_name = "progress",
            values = c("early")
          ))
        ),
        list(
          col_name = "progress",
          label = "Progress",
          type = "dropdown",
          choices = list(list(label = "Early", value = "early"))
        )
      )
      expect_error(
        validate_col_spec(spec, valid_row_spec()),
        regexp = "progress"
      )
    })
  })

  describe("given a gate value condition whose controller is not a dropdown", {
    it("throws an error mentioning dropdown", {
      spec <- list(
        list(
          col_name = "suspensions",
          label = "Suspensions",
          type = "numeric",
          min = 0L
        ),
        list(
          col_name = "months_offset",
          label = "+/- Months",
          type = "numeric",
          min = -24L,
          gate = list(list(
            type = "value",
            col_name = "suspensions",
            values = c("1")
          ))
        )
      )
      expect_error(
        validate_col_spec(spec, valid_row_spec()),
        regexp = "dropdown"
      )
    })
  })

  describe("given a gate value condition missing values", {
    it("throws an error mentioning values", {
      spec <- value_gate_col_spec()
      spec[[2L]]$gate[[1L]]$values <- NULL
      expect_error(validate_col_spec(spec, valid_row_spec()), regexp = "values")
    })
  })

  describe("given a gate value condition with values not in controller choices", {
    it("throws an error naming the invalid value", {
      spec <- value_gate_col_spec()
      spec[[2L]]$gate[[1L]]$values <- c("early", "nonexistent_value")
      expect_error(
        validate_col_spec(spec, valid_row_spec()),
        regexp = "nonexistent_value"
      )
    })
  })
})
