# tests/testthat/test-smoke.R

describe("reactablePlus package exports", {

  # Phase 1 — dependencies
  it("exposes useReactablePlus()", {
    expect_true(is.function(useReactablePlus))
  })

  it("exposes bindPickersOnRender()", {
    expect_true(is.function(bindPickersOnRender))
  })

  it("exposes popoverDep()", {
    expect_true(is.function(popoverDep))
  })

  # Phase 2 — generic editable table
  it("exposes editable_table_ui()", {
    expect_true(is.function(editable_table_ui))
  })

  it("exposes editable_table_server()", {
    expect_true(is.function(editable_table_server))
  })

  it("exposes validate_col_spec()", {
    expect_true(is.function(validate_col_spec))
  })

  it("exposes validate_row_spec()", {
    expect_true(is.function(validate_row_spec))
  })

  it("exposes collect_inputs()", {
    expect_true(is.function(collect_inputs))
  })

  it("exposes merge_existing_data()", {
    expect_true(is.function(merge_existing_data))
  })

  it("exposes make_input_id()", {
    expect_true(is.function(make_input_id))
  })

  # Phase 3 — widget constructors
  it("exposes schoolPickerInput()", {
    expect_true(is.function(schoolPickerInput))
  })

  it("exposes attendancePickerInput()", {
    expect_true(is.function(attendancePickerInput))
  })

  it("exposes homeschoolPickerInput()", {
    expect_true(is.function(homeschoolPickerInput))
  })

  it("exposes notesInput()", {
    expect_true(is.function(notesInput))
  })

  it("exposes gearPopoverInput()", {
    expect_true(is.function(gearPopoverInput))
  })

  it("exposes useSchoolSearch()", {
    expect_true(is.function(useSchoolSearch))
  })
})
