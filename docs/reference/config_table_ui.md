# Config-driven Editable Table UI

UI function for the config-driven editable table module. Works with
[`table_config()`](https://matthewcurrier.github.io/reactablePlus/reference/table_config.md)
objects and picker widgets, as opposed to the lower-level
[`editable_table_ui()`](https://matthewcurrier.github.io/reactablePlus/reference/editable_table_ui.md)
which uses raw `row_spec`/`col_spec` lists.

## Usage

``` r
config_table_ui(id, config)
```

## Arguments

- id:

  Module namespace ID.

- config:

  A
  [table_config](https://matthewcurrier.github.io/reactablePlus/reference/table_config.md)
  object.

## Value

A tagList with dependencies, toolbar, and reactable output.
