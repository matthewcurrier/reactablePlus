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
  data_r = shiny::reactive(NULL),
  search_fn = NULL
)
```

## Arguments

- id:

  Module namespace ID.

- config:

  A `table_config` object.

- data_r:

  Reactive returning `NULL` (new/add mode) or a data frame (edit mode
  with saved rows).

- search_fn:

  A function `(query, limit)` → data.frame for server-side typeahead
  search. Required when any column uses `search_picker` type. Default
  `NULL`.

## Value

`list(get_data = reactive)`.
