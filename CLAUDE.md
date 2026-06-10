# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`reactablePlus` is an R package that extends [reactable](https://glin.github.io/reactable/) with inline editing widgets and a declarative configuration layer for building editable tables in Shiny. It was extracted from a domain package (`schoolhistory`) to become a generic, domain-agnostic library — see `MIGRATION_GUIDE.md` for the rename history (e.g. `col_spec()` → `widget_col()`, `editable_table_*` → `config_table_*`).

## Commands

Run from an R session at the repo root (uses `devtools`/`testthat`):

```r
devtools::load_all()        # load package for interactive work
devtools::document()        # regenerate NAMESPACE + man/ from roxygen (run after changing @export/@param)
devtools::test()            # run the full testthat suite
devtools::check()           # R CMD check

# Run a single test file
testthat::test_file("tests/testthat/test-config-primitives.R")

# Launch a bundled demo app (also the fastest way to eyeball behavior)
reactablePlus::runExample()              # lists available examples
reactablePlus::runExample("inventory")   # inventory | roster | evaluations | appendable
```

There is no separate lint step; follow the existing style (the codebase is formatted to ~80 cols with section-banner comments).

## Architecture

### Two layers

1. **Widget library** (`R/*-picker.R`, `R/notes-input.R`, `R/gear-popover.R`) — standalone cell-level inputs usable directly inside any `reactable` cell. Each widget is a triple that must stay in sync:
   - an R constructor returning `htmltools` tags (e.g. `searchPickerInput()`)
   - an `update*()` R function that sends a custom message to the browser
   - a JS Shiny input binding in `inst/assets/js/*-binding.js`
   - registered as an `htmlDependency` in `R/dependencies.R`

   All pickers build on the shared popover lifecycle in `inst/assets/js/popover-core.js` (`R/dependencies.R::popoverDep()`).

2. **Config-driven module** (`R/mod_config_table.R` + `R/config_table_render.R` + `R/table-config.R`) — a generic Shiny module that renders a whole editable table from a declarative spec, with **no domain knowledge**. This is the heart of the package.

### Config-driven module data flow

```
table_config(columns = list(widget_col(...), ...), ...)   # R/table-config.R — the spec
        │
        ├── config_table_ui(id, config)        # R/mod_config_table.R — toolbar + reactableOutput
        └── config_table_server(id, config,     # R/mod_config_table.R — state, observers, marshaling
                source_data=, data_r=, reset_signal=, search_fn=)
                    │ uses pure render helpers in
                    └── R/config_table_render.R  # builds the reactable data frame + colDefs + cell HTML
```

- `table_config()` and `widget_col()` are S3 constructors that **validate and wire** the spec at construction time: they check gate references resolve, auto-mark gate-controller and `choices_depends_on` columns as `triggers_rerender`, normalize dropdown choices, and infer per-type `empty_value` defaults.
- `R/config_table_render.R` holds **pure functions only** (no reactive state): given config + current row state + settings, they produce the reactable data frame, the `colDef` list, and per-cell HTML. Cell rendering branches on `widget_col$type`.
- `config_table_server()` owns all reactivity: per-row input observers, gate evaluation, mutual-exclusion / fill-down interactions, selection, reset, and the `from_saved_fn`/`to_output_fn` data marshaling. It returns `list(get_data = reactive, selected_ids = reactive)`.

### Three row modes (mutually constrained — see validation in `table-config.R`)

- **Static** — rows fixed at config time via `row_keys`/`row_labels`.
- **Dynamic** — `row_id_col` set; rows derived from the reactive `source_data` data frame at runtime, with state preserved across row-set changes. Enables `display_cols`.
- **Appendable** — `appendable = TRUE`; user adds/removes rows via Add Row / Delete buttons, bounded by `min_rows`/`max_rows`. Auto-incrementing `row_N` keys. **Mutually exclusive with dynamic mode.**

### Widget column types

`widget_col(type = ...)` accepts **picker widgets** (`search_picker`, `attendance_picker`, `homeschool_picker`, `notes_input` — popover-based, backed by the JS bindings) and **primitive inputs** (`dropdown`, `numeric`, `date`, `checkbox`, `toggle`, `text` — inline HTML controls). `type = "custom"` requires a `render_cell_fn`.

## Conventions and gotchas

- **`NAMESPACE` and `man/` are generated** — never hand-edit. Change roxygen comments and run `devtools::document()`.
- **JS dependencies don't flow through reactable cells.** reactable's React rendering ignores `htmlDependency` objects returned from cell functions, so page-level JS/CSS must be injected via `useReactablePlus()` (in `R/dependencies.R`). `config_table_ui()` calls it for you; standalone picker usage requires calling it manually in the UI.
- **Picker binding timing.** Shiny's `bindAll()` runs before reactable paints its cells, so picker inputs must be bound *after* render via `bindPickersOnRender()` (a `MutationObserver` + `setTimeout` fallback). Wrap any `reactable()` containing pickers with it.
- **In-place cell updates.** Cross-cell effects (mutual-exclusion display swaps, row-class toggles, gear column visibility) are done via custom-message handlers in `inst/assets/js/reactable-plus-updates.js` and CSS, deliberately avoiding full reactable re-renders for performance. Only `triggers_rerender` columns force a full re-render.
- **Editing R behavior that has a JS counterpart** (a picker, an update function, an in-place effect) usually means editing both the `R/` constructor and the matching `inst/assets/js/*.js` binding.
- `useSchoolHistory()` / `useSchoolSearch()` are deprecated aliases kept for the downstream `schoolhistory` package; prefer `useReactablePlus()` / `useTypeaheadSearch()`.

## Working guidelines

- **Test new functions.** Add `testthat` unit tests where appropriate (mirror the existing `tests/testthat/test-*.R` files). For end-to-end / regression coverage of Shiny behavior, reach for `shinytest2`.
- **Prefer the tidyverse.** Use `purrr` iterators (`map`, `walk`, `imap`, `reduce`, …) over `for` loops, and `dplyr`/`tibble`/`rlang` over base equivalents, consistent with the existing code.
- **Never write `c("" = "")`** or any named vector/list with an empty-string name or value — it produces a zero-length variable that causes hard-to-trace failures. Build choice vectors with real labels and values (see `normalize_choices()` in `R/table-config.R`).
- **roxygen namespaces.** When documenting, declare every external function you use with an `@importFrom pkg fn` tag rather than relying on `pkg::fn` ad hoc — match the pattern already in `R/dependencies.R` and `R/mod_config_table.R`. Run `devtools::document()` afterward.
- **Cite locations.** When proposing a change to existing code, reference the file and line number (exact or approximate is fine, e.g. `R/config_table_render.R:67`).
- **Apply established craft.** Consider the design advice of Martin Fowler (refactoring, naming, small focused functions) and *The Pragmatic Programmer* (DRY, orthogonality, easy-to-change code) when shaping changes.
- **Ask when unsure.** If requirements or intent are ambiguous, ask follow-up questions rather than guessing.
