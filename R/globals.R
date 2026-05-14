# Suppress R CMD check notes for:
#   - NSE variables used by dplyr (e.g., .selected in relocate/mutate)
#   - Same-package functions called inside closures that codetools can't trace
#     (moduleServer callbacks, reactive expressions, reactable cell renderers)
utils::globalVariables(c(
  # NSE variable
  ".selected",
  # Internal helpers called inside editable_table_server closures
  # Widget constructors called inside config_table cell renderers
  "attendancePickerInput",
  "homeschoolPickerInput",
  "notesInput",
  "gearPopoverInput",
  # Grade utility called inside config_table_server fill-down
  "gradeInRange"
))
