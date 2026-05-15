# Config-driven Editable Table UI

UI function for the config-driven editable table module. Works with
[`table_config()`](https://matthewcurrier.github.io/reactablePlus/reference/table_config.md)
objects and picker widgets. When the config has `appendable = TRUE`, the
toolbar includes an "Add Row" button and each row renders a delete
button (if `allow_delete = TRUE`).

## Usage

``` r
config_table_ui(id, config)
```

## Arguments

- id:

  Module namespace ID.

- config:

  A `table_config` object.

## Value

A tagList with dependencies, toolbar, and reactable output.
