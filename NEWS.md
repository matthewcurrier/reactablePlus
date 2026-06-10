# reactablePlus (development version)

# reactablePlus 0.2.0

## New features

* Popover pickers (`search_picker`, `attendance_picker`,
  `homeschool_picker`) now report through the same per-column
  `*_extra` store as the primitive cell types. The config-driven table
  module reads **all** column types uniformly, picker fill-down and
  reset flow through `update_extra()`, and edits survive reactable
  repaints (sort / filter / paginate) for pickers as they already did
  for primitives. No changes are required in existing app code.

## Documentation

* New article **"Editable Cells: the `*_extra` family"** — the
  cell-factory layer (`text_extra()`, `numeric_extra()`, `date_extra()`,
  `checkbox_extra()`, `dropdown_extra()`, `toggle_extra()`,
  `notes_extra()`), the per-column value-return contract, required
  wiring, `update_extra()`, and the sort/filter caveat.
* New article **"Picker Widgets and Rich Interactions"** — the popover
  picker family, server-side typeahead search (`useTypeaheadSearch()` /
  `search_fn_col`), and the table-level interactions (mutual exclusion,
  fill-down with range checks, and gear toggles).

## Internal

* Added a JS type registry to the cell-extras store so non-input
  widgets (popover pickers) can participate via `read` / `write`
  handlers; pickers report through a jQuery-delegated change listener.
* New characterization tests pin the picker store contract
  (`test-picker-characterization-shinytest2.R`, including the
  attendance empty-value edge) and the search-picker fill-down cascade
  (`test-picker-filldown-shinytest2.R`).


# reactablePlus 0.1.0

* Initial release. Extracted from the `schoolhistory` domain package
  into a generic, domain-agnostic library (see `MIGRATION_GUIDE.md`).
* Cell-level picker widgets with custom Shiny bindings:
  `searchPickerInput()`, `attendancePickerInput()`,
  `homeschoolPickerInput()`, `notesInput()`, `gearPopoverInput()`.
* The `*_extra` editable cell factories for use inside any reactable.
* A declarative, config-driven editable table module
  (`table_config()`, `widget_col()`, `config_table_ui()`,
  `config_table_server()`) supporting static, dynamic, and appendable
  row modes, with gating, selection, reset, mutual exclusion,
  fill-down, and gear toggles.
