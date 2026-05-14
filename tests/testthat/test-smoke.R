# tests/testthat/test-smoke.R

describe("reactablePlus primary exports", {

  # Dependencies
  it("exposes useReactablePlus()", {
    expect_true(is.function(useReactablePlus))
  })

  it("exposes bindPickersOnRender()", {
    expect_true(is.function(bindPickersOnRender))
  })

  it("exposes popoverDep()", {
    expect_true(is.function(popoverDep))
  })

  # Config-driven table (primary API)
  it("exposes config_table_ui()", {
    expect_true(is.function(config_table_ui))
  })

  it("exposes config_table_server()", {
    expect_true(is.function(config_table_server))
  })

  it("exposes table_config()", {
    expect_true(is.function(table_config))
  })

  it("exposes widget_col()", {
    expect_true(is.function(widget_col))
  })

  it("exposes normalize_choices()", {
    expect_true(is.function(normalize_choices))
  })

  # Widget constructors
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

  # Grade utilities
  it("exposes gradeChoices()", {
    expect_true(is.function(gradeChoices))
  })

  it("exposes gradeIndex()", {
    expect_true(is.function(gradeIndex))
  })

  it("exposes gradeInRange()", {
    expect_true(is.function(gradeInRange))
  })
})

describe("reactablePlus deprecated exports (still available)", {

  it("exposes editable_table_ui() with deprecation", {
    expect_true(is.function(editable_table_ui))
  })

  it("exposes editable_table_server() with deprecation", {
    expect_true(is.function(editable_table_server))
  })

  it("exposes display_col() with deprecation", {
    expect_true(is.function(display_col))
  })

  it("exposes row_def() with deprecation", {
    expect_true(is.function(row_def))
  })

  it("exposes input_col() with deprecation", {
    expect_true(is.function(input_col))
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
})
