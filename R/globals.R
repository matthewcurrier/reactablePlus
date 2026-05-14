# Suppress R CMD check notes for:
#   - NSE variables used by dplyr (e.g., .selected in relocate/mutate)
#   - Same-package functions called inside closures that codetools can't trace
#     (moduleServer callbacks, reactive expressions, reactable cell renderers)
utils::globalVariables(c(
  # NSE variable
  ".selected",
  # Widget constructors called inside config_table cell renderers
  "searchPickerInput",
  "attendancePickerInput",
  "homeschoolPickerInput",
  "notesInput",
  "gearPopoverInput",
  # Search wiring called inside config_table_server
  "useTypeaheadSearch"
))
