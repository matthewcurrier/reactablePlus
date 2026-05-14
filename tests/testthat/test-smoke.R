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


