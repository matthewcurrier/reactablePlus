# Suppress R CMD check notes for:
#   - NSE variables used by dplyr (e.g., .selected in relocate/mutate)
#   - Same-package functions called inside closures that codetools can't trace
#     (moduleServer callbacks, reactive expressions, reactable cell renderers)
utils::globalVariables(c(
  # NSE variable
  ".selected",
  # Internal helpers called inside editable_table_server closures
  "build_gate_map",
  "build_select_col_def",
  "build_display_col_defs",
  "build_input_col_defs",
  "build_selection_gate_js",
  # Exported functions called inside moduleServer / reactive closures
  "validate_row_spec",
  "validate_col_spec",
  "merge_existing_data",
  "collect_inputs",
  # Widget constructors called inside config_table cell renderers
  "attendancePickerInput",
  "homeschoolPickerInput",
  "notesInput",
  "gearPopoverInput",
  # Grade utility called inside config_table_server fill-down
 "gradeInRange"
))
