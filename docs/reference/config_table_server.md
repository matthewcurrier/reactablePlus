# Config-driven Editable Table Server

Server function for the config-driven editable table module. Pairs with
[`config_table_ui()`](https://matthewcurrier.github.io/reactablePlus/reference/config_table_ui.md).
Uses
[`table_config()`](https://matthewcurrier.github.io/reactablePlus/reference/table_config.md)
objects to drive picker widget rendering, state management, and
cross-column interactions.

## Usage

``` r
config_table_server(
  id,
  config,
  source_data = NULL,
  data_r = shiny::reactive(NULL),
  reset_signal = NULL,
  search_fn = NULL
)
```

## Arguments

- id:

  Module namespace ID.

- config:

  A `table_config` object.

- source_data:

  Reactive returning a data frame, or `NULL`. Required in dynamic mode
  (`row_id_col` set in config). When `source_data()` changes, the module
  derives the current row set, preserves user-entered values for
  surviving rows, and assigns defaults to new rows. Ignored in static
  mode. Default `NULL`.

- data_r:

  Reactive returning `NULL` (new/add mode) or a data frame (edit mode
  with saved rows). Seeds initial values via the config's
  `from_saved_fn`. In dynamic mode, applied on top of the
  `source_data`-derived row set.

- reset_signal:

  Reactive returning `NULL` or a named list, or `NULL` to disable. When
  the reactive fires with a non-NULL list, the module replaces its
  internal state and forces a full re-render. Supported list fields:

  `data`

  :   A data frame parsed via `from_saved_fn`, or `NULL` to reset all
      values to `empty_value` defaults.

  `selected`

  :   A character vector of row keys to mark as selected, or `NULL` to
      deselect all. Ignored when `selectable` is `FALSE`.

  This is the correct mechanism for mid-session context switches (e.g.
  opening Report B after Report A) where both column values and
  selection state need to be replaced atomically. The recommended
  calling pattern uses a `reactiveVal(NULL)` that is only set when a
  genuine switch occurs:


          reset <- reactiveVal(NULL)
          observeEvent(input$switch_report, {
            reset(list(data = load_report(input$report_id),
                       selected = NULL))
          })
          config_table_server("tbl", cfg, reset_signal = reset)
        

- search_fn:

  A function `(query, limit)` → data.frame for server-side typeahead
  search. Required when any column uses `search_picker` type. Default
  `NULL`.

## Value

`list(get_data = reactive, selected_ids = reactive)`.
