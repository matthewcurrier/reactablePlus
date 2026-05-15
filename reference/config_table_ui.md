# Config-driven Editable Table UI

UI function for the config-driven editable table module. Works with
[`table_config()`](https://matthewcurrier.github.io/reactablePlus/reference/table_config.md)
objects and picker widgets.

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
