# test-editable_table_server.R
# Uses shiny::testServer() to exercise the module without a browser.
# session$setInputs() supplies inline input values by their local (un-namespaced) IDs.
# The pattern for inline input IDs inside the module is: "{row_id}_{col_name}".
#
# NOTE: The reset button also sends a JS message via session$sendCustomMessage
# to clear the browser DOM and Shiny's input store. That JS side-effect does
# not run in testServer. Reset tests therefore validate the server-side state
# change (display_data_r goes blank, current_data reflects blank defaults)
# under the condition that no prior inputs were set via session$setInputs —
# which is the equivalent of the browser state immediately after JS clears the
# inputs. Full round-trip reset behaviour is covered by integration tests.

describe("editable_table_server()", {
  # ── current_data: no existing data, no user interaction ───────────────────

  describe("current_data with no existing data and no user interaction", {
    it("returns a tibble", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          expect_s3_class(current_data(), "tbl_df")
        }
      )
    })

    it("returns one row per row in data_r", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          expect_equal(nrow(current_data()), nrow(valid_grades_df()))
        }
      )
    })

    it("includes the id_col", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          expect_true("grade_id" %in% names(current_data()))
        }
      )
    })

    it("sets numeric columns to spec$min when the user has not interacted", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          expected_min <- valid_col_spec()[[2L]]$min
          expect_true(all(current_data()$suspensions == expected_min))
        }
      )
    })

    it("sets dropdown columns to NA when the user has not interacted", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          expect_true(all(is.na(current_data()$attendance)))
        }
      )
    })

    it("does not include display columns in current_data", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          expect_false("grade_label" %in% names(current_data()))
        }
      )
    })
  })

  # ── current_data: reflects user input changes ─────────────────────────────

  describe("current_data when the user sets input values", {
    it("reflects an updated dropdown value immediately", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          session$setInputs(`6_attendance` = "good")
          expect_equal(
            current_data()$attendance[current_data()$grade_id == 6L],
            "good"
          )
        }
      )
    })

    it("reflects an updated numeric value immediately", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          session$setInputs(`7_suspensions` = 3L)
          expect_equal(
            current_data()$suspensions[current_data()$grade_id == 7L],
            3L
          )
        }
      )
    })

    it("does not affect other rows when one row's input changes", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          session$setInputs(`6_suspensions` = 5L)
          expect_equal(
            current_data()$suspensions[current_data()$grade_id == 7L],
            valid_col_spec()[[2L]]$min
          )
        }
      )
    })
  })

  # ── current_data: pre-fill from existing_data_r ───────────────────────────

  describe("current_data when existing_data_r supplies previously saved values", {
    it("pre-fills dropdown columns for rows present in existing_data", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(valid_existing_data())
        ),
        {
          expect_equal(
            current_data()$attendance[current_data()$grade_id == 6L],
            "good"
          )
        }
      )
    })

    it("pre-fills numeric columns for rows present in existing_data", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(valid_existing_data())
        ),
        {
          expect_equal(
            current_data()$suspensions[current_data()$grade_id == 6L],
            0L
          )
        }
      )
    })

    it("leaves dropdown columns as NA for rows absent from existing_data", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(valid_existing_data())
        ),
        {
          expect_true(
            is.na(current_data()$attendance[current_data()$grade_id == 8L])
          )
        }
      )
    })

    it("uses spec$min for numeric columns absent from existing_data", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(valid_existing_data())
        ),
        {
          expect_equal(
            current_data()$suspensions[current_data()$grade_id == 8L],
            valid_col_spec()[[2L]]$min
          )
        }
      )
    })
  })

  # ── Reset button ──────────────────────────────────────────────────────────

  describe("reset button", {
    it("increments reset_count from 0 to 1", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          expect_equal(reset_count(), 0L)
          session$setInputs(reset = 1L)
          expect_equal(reset_count(), 1L)
        }
      )
    })

    it("sets dropdown columns to NA after reset (no prior inputs set)", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(valid_existing_data())
        ),
        {
          session$setInputs(reset = 1L)
          expect_true(all(is.na(current_data()$attendance)))
        }
      )
    })

    it("sets numeric columns to spec$min after reset (no prior inputs set)", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(valid_existing_data())
        ),
        {
          session$setInputs(reset = 1L)
          expected_min <- valid_col_spec()[[2L]]$min
          expect_true(all(current_data()$suspensions == expected_min))
        }
      )
    })

    it("clears pre-filled existing data from display after reset", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(valid_existing_data())
        ),
        {
          # Grade 6 is pre-filled with "good" from existing_data.
          # After reset, display_data_r ignores existing_data_r entirely.
          session$setInputs(reset = 1L)
          expect_true(is.na(current_data()$attendance[
            current_data()$grade_id == 6L
          ]))
        }
      )
    })

    it("can be clicked multiple times without error", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          session$setInputs(reset = 1L)
          session$setInputs(reset = 2L)
          expect_equal(reset_count(), 2L)
          expect_s3_class(current_data(), "tbl_df")
        }
      )
    })
  })

  # ── New types: default values before user interaction ─────────────────────

  describe("current_data default values for all six types with no user interaction", {
    it("sets date column to NA by default", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_full_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          expect_true(all(is.na(current_data()$enrollment_date)))
        }
      )
    })

    it("sets checkbox column to FALSE by default", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_full_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          expect_true(all(current_data()$has_iep == FALSE))
        }
      )
    })

    it("sets toggle column to FALSE by default", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_full_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          expect_true(all(current_data()$active == FALSE))
        }
      )
    })

    it("sets text column to empty string by default", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_full_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          expect_true(all(current_data()$notes == ""))
        }
      )
    })
  })

  # ── New types: live input reflection ──────────────────────────────────────

  describe("current_data reflects new type inputs when set via session$setInputs", {
    it("reflects a date input", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_full_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          session$setInputs(`6_enrollment_date` = "2023-09-01")
          expect_equal(
            current_data()$enrollment_date[current_data()$grade_id == 6L],
            as.Date("2023-09-01")
          )
        }
      )
    })

    it("reflects a checkbox input set to TRUE", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_full_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          session$setInputs(`7_has_iep` = TRUE)
          expect_true(
            current_data()$has_iep[current_data()$grade_id == 7L]
          )
        }
      )
    })

    it("reflects a toggle input set to TRUE", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_full_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          session$setInputs(`8_active` = TRUE)
          expect_true(
            current_data()$active[current_data()$grade_id == 8L]
          )
        }
      )
    })

    it("reflects a text input", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_full_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          session$setInputs(`6_notes` = "Excellent progress")
          expect_equal(
            current_data()$notes[current_data()$grade_id == 6L],
            "Excellent progress"
          )
        }
      )
    })
  })

  # ── selected_ids: selectable = FALSE (default) ────────────────────────────

  describe("selected_ids when row_spec$selectable is not TRUE", {
    it("returns NULL", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          expect_null(selected_ids())
        }
      )
    })
  })

  # ── selected_ids: selectable = TRUE ──────────────────────────────────────

  describe("selected_ids when row_spec$selectable is TRUE", {
    it("returns an empty vector before any row is selected", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = selectable_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          expect_length(selected_ids(), 0L)
        }
      )
    })

    it("returns the id of a row when its checkbox is checked", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = selectable_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          session$setInputs(`6_selected` = TRUE)
          expect_true(6L %in% selected_ids())
        }
      )
    })

    it("does not include a row whose checkbox is FALSE", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = selectable_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          session$setInputs(`6_selected` = TRUE)
          expect_false(7L %in% selected_ids())
        }
      )
    })

    it("returns multiple ids when multiple rows are checked", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = selectable_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          session$setInputs(`6_selected` = TRUE, `8_selected` = TRUE)
          expect_true(6L %in% selected_ids())
          expect_true(8L %in% selected_ids())
          expect_false(7L %in% selected_ids())
        }
      )
    })

    it("removes a row id from selected_ids when its checkbox is unchecked", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = selectable_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          session$setInputs(`7_selected` = TRUE)
          expect_true(7L %in% selected_ids())
          session$setInputs(`7_selected` = FALSE)
          expect_false(7L %in% selected_ids())
        }
      )
    })

    it("preserves the type of the id_col in selected_ids", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = selectable_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          session$setInputs(`6_selected` = TRUE)
          expect_type(selected_ids(), "integer")
        }
      )
    })

    it("clears selected_ids after reset", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = selectable_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          session$setInputs(`6_selected` = TRUE, `7_selected` = TRUE)
          expect_length(selected_ids(), 2L)
          # Reset sends a JS message to the browser and clears server inputs.
          # In testServer the JS side-effect doesn't run, so we manually
          # clear the inputs to simulate what the browser would do.
          session$setInputs(
            `6_selected` = FALSE,
            `7_selected` = FALSE,
            reset = 1L
          )
          expect_length(selected_ids(), 0L)
        }
      )
    })
  })

  # ── Module return value ───────────────────────────────────────────────────

  describe("module return value", {
    it("is a list", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          expect_type(session$returned, "list")
        }
      )
    })

    it("contains a current_data element", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          expect_true("current_data" %in% names(session$returned))
        }
      )
    })

    it("contains a selected_ids element", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          expect_true("selected_ids" %in% names(session$returned))
        }
      )
    })

    it("current_data element is a reactive that returns a tibble", {
      shiny::testServer(
        editable_table_server,
        args = list(
          data_r = shiny::reactive(valid_grades_df()),
          row_spec = valid_row_spec(),
          col_spec = valid_col_spec(),
          existing_data_r = shiny::reactive(NULL)
        ),
        {
          expect_s3_class(session$returned$current_data(), "tbl_df")
        }
      )
    })
  })

  # ── Value gate (type = "value") behaviour ────────────────────────────────

  describe("value gate — gate with type 'value' condition", {
    describe("when the controller has no value", {
      it("returns NA for the gated field", {
        shiny::testServer(
          editable_table_server,
          args = list(
            data_r = shiny::reactive(valid_grades_df()),
            row_spec = valid_row_spec(),
            col_spec = value_gate_col_spec(),
            existing_data_r = shiny::reactive(NULL)
          ),
          {
            expect_true(all(is.na(current_data()$months_offset)))
          }
        )
      })
    })

    describe("when the controller value is not in gate values", {
      it("returns NA for the gated field", {
        shiny::testServer(
          editable_table_server,
          args = list(
            data_r = shiny::reactive(valid_grades_df()),
            row_spec = valid_row_spec(),
            col_spec = value_gate_col_spec(),
            existing_data_r = shiny::reactive(NULL)
          ),
          {
            session$setInputs(`7_progress` = "on_time", `7_months_offset` = 5L)
            expect_true(
              is.na(current_data()$months_offset[current_data()$grade_id == 7L])
            )
          }
        )
      })

      it("does not affect rows whose gate is open", {
        shiny::testServer(
          editable_table_server,
          args = list(
            data_r = shiny::reactive(valid_grades_df()),
            row_spec = valid_row_spec(),
            col_spec = value_gate_col_spec(),
            existing_data_r = shiny::reactive(NULL)
          ),
          {
            session$setInputs(
              `6_progress` = "early",
              `6_months_offset` = -2L,
              `7_progress` = "on_time",
              `7_months_offset` = 5L
            )
            expect_equal(
              current_data()$months_offset[current_data()$grade_id == 6L],
              -2L
            )
            expect_true(
              is.na(current_data()$months_offset[current_data()$grade_id == 7L])
            )
          }
        )
      })
    })

    describe("when the controller value is in gate values", {
      it("returns the entered value", {
        shiny::testServer(
          editable_table_server,
          args = list(
            data_r = shiny::reactive(valid_grades_df()),
            row_spec = valid_row_spec(),
            col_spec = value_gate_col_spec(),
            existing_data_r = shiny::reactive(NULL)
          ),
          {
            session$setInputs(`6_progress` = "early", `6_months_offset` = -2L)
            expect_equal(
              current_data()$months_offset[current_data()$grade_id == 6L],
              -2L
            )
          }
        )
      })
    })

    describe("when the controller switches to a locking value", {
      it("returns NA for the gated field", {
        shiny::testServer(
          editable_table_server,
          args = list(
            data_r = shiny::reactive(valid_grades_df()),
            row_spec = valid_row_spec(),
            col_spec = value_gate_col_spec(),
            existing_data_r = shiny::reactive(NULL)
          ),
          {
            session$setInputs(`6_progress` = "early", `6_months_offset` = -2L)
            session$setInputs(`6_progress` = "on_time")
            expect_true(
              is.na(current_data()$months_offset[current_data()$grade_id == 6L])
            )
          }
        )
      })
    })

    describe("after reset with a value gate col_spec", {
      it("returns NA for the gated field after reset", {
        shiny::testServer(
          editable_table_server,
          args = list(
            data_r = shiny::reactive(valid_grades_df()),
            row_spec = valid_row_spec(),
            col_spec = value_gate_col_spec(),
            existing_data_r = shiny::reactive(NULL)
          ),
          {
            session$setInputs(`6_progress` = "early", `6_months_offset` = -2L)
            session$setInputs(
              `6_progress` = "",
              `6_months_offset` = -24L,
              reset = 1L
            )
            expect_true(
              is.na(current_data()$months_offset[current_data()$grade_id == 6L])
            )
          }
        )
      })
    })
  })

  # ── Selection gate (type = "selected") behaviour ──────────────────────────

  describe("selection gate — gate with type 'selected' condition", {
    describe("when the row is not selected", {
      it("returns NA for the selection-gated field", {
        shiny::testServer(
          editable_table_server,
          args = list(
            data_r = shiny::reactive(valid_grades_df()),
            row_spec = selectable_row_spec(),
            col_spec = selection_gate_col_spec(),
            existing_data_r = shiny::reactive(NULL)
          ),
          {
            expect_true(all(is.na(current_data()$progress)))
          }
        )
      })

      it("returns NA even if the user sets the input value directly", {
        shiny::testServer(
          editable_table_server,
          args = list(
            data_r = shiny::reactive(valid_grades_df()),
            row_spec = selectable_row_spec(),
            col_spec = selection_gate_col_spec(),
            existing_data_r = shiny::reactive(NULL)
          ),
          {
            session$setInputs(`6_progress` = "early")
            expect_true(
              is.na(current_data()$progress[current_data()$grade_id == 6L])
            )
          }
        )
      })
    })

    describe("when the row is selected", {
      it("returns the entered value for the selection-gated field", {
        shiny::testServer(
          editable_table_server,
          args = list(
            data_r = shiny::reactive(valid_grades_df()),
            row_spec = selectable_row_spec(),
            col_spec = selection_gate_col_spec(),
            existing_data_r = shiny::reactive(NULL)
          ),
          {
            session$setInputs(`6_selected` = TRUE, `6_progress` = "early")
            expect_equal(
              current_data()$progress[current_data()$grade_id == 6L],
              "early"
            )
          }
        )
      })

      it("does not affect unselected rows", {
        shiny::testServer(
          editable_table_server,
          args = list(
            data_r = shiny::reactive(valid_grades_df()),
            row_spec = selectable_row_spec(),
            col_spec = selection_gate_col_spec(),
            existing_data_r = shiny::reactive(NULL)
          ),
          {
            session$setInputs(`6_selected` = TRUE, `6_progress` = "early")
            expect_true(
              is.na(current_data()$progress[current_data()$grade_id == 7L])
            )
          }
        )
      })
    })

    describe("when a selected row is deselected", {
      it("wipes the selection-gated field back to NA", {
        shiny::testServer(
          editable_table_server,
          args = list(
            data_r = shiny::reactive(valid_grades_df()),
            row_spec = selectable_row_spec(),
            col_spec = selection_gate_col_spec(),
            existing_data_r = shiny::reactive(NULL)
          ),
          {
            session$setInputs(`6_selected` = TRUE, `6_progress` = "early")
            expect_equal(
              current_data()$progress[current_data()$grade_id == 6L],
              "early"
            )
            session$setInputs(`6_selected` = FALSE)
            expect_true(
              is.na(current_data()$progress[current_data()$grade_id == 6L])
            )
          }
        )
      })
    })
  })

  # ── Chained gate (selected + value) behaviour ─────────────────────────────

  describe("chained gate — both selected and value conditions required", {
    describe("when the row is not selected", {
      it("returns NA for all gated fields regardless of value inputs", {
        shiny::testServer(
          editable_table_server,
          args = list(
            data_r = shiny::reactive(valid_grades_df()),
            row_spec = selectable_row_spec(),
            col_spec = chained_gate_col_spec(),
            existing_data_r = shiny::reactive(NULL)
          ),
          {
            session$setInputs(`6_progress` = "early", `6_months_offset` = -2L)
            expect_true(
              is.na(current_data()$progress[current_data()$grade_id == 6L])
            )
            expect_true(
              is.na(current_data()$months_offset[current_data()$grade_id == 6L])
            )
          }
        )
      })
    })

    describe("when the row is selected but progress is 'on_time'", {
      it("unlocks progress but keeps months_offset and notes as NA", {
        shiny::testServer(
          editable_table_server,
          args = list(
            data_r = shiny::reactive(valid_grades_df()),
            row_spec = selectable_row_spec(),
            col_spec = chained_gate_col_spec(),
            existing_data_r = shiny::reactive(NULL)
          ),
          {
            session$setInputs(
              `6_selected` = TRUE,
              `6_progress` = "on_time",
              `6_months_offset` = 5L,
              `6_notes` = "some note"
            )
            expect_equal(
              current_data()$progress[current_data()$grade_id == 6L],
              "on_time"
            )
            expect_true(
              is.na(current_data()$months_offset[current_data()$grade_id == 6L])
            )
            expect_true(
              is.na(current_data()$notes[current_data()$grade_id == 6L])
            )
          }
        )
      })
    })

    describe("when the row is selected and progress is 'early'", {
      it("unlocks progress, months_offset, and notes", {
        shiny::testServer(
          editable_table_server,
          args = list(
            data_r = shiny::reactive(valid_grades_df()),
            row_spec = selectable_row_spec(),
            col_spec = chained_gate_col_spec(),
            existing_data_r = shiny::reactive(NULL)
          ),
          {
            session$setInputs(
              `6_selected` = TRUE,
              `6_progress` = "early",
              `6_months_offset` = -2L,
              `6_notes` = "Ahead of schedule"
            )
            expect_equal(
              current_data()$progress[current_data()$grade_id == 6L],
              "early"
            )
            expect_equal(
              current_data()$months_offset[current_data()$grade_id == 6L],
              -2L
            )
            expect_equal(
              current_data()$notes[current_data()$grade_id == 6L],
              "Ahead of schedule"
            )
          }
        )
      })
    })

    describe("when a fully unlocked row is deselected", {
      it("wipes all selected-gated fields back to NA", {
        shiny::testServer(
          editable_table_server,
          args = list(
            data_r = shiny::reactive(valid_grades_df()),
            row_spec = selectable_row_spec(),
            col_spec = chained_gate_col_spec(),
            existing_data_r = shiny::reactive(NULL)
          ),
          {
            session$setInputs(
              `6_selected` = TRUE,
              `6_progress` = "early",
              `6_months_offset` = -2L,
              `6_notes` = "Ahead of schedule"
            )
            session$setInputs(`6_selected` = FALSE)
            expect_true(is.na(current_data()$progress[
              current_data()$grade_id == 6L
            ]))
            expect_true(is.na(current_data()$months_offset[
              current_data()$grade_id == 6L
            ]))
            expect_true(is.na(current_data()$notes[
              current_data()$grade_id == 6L
            ]))
          }
        )
      })
    })
  })
})
