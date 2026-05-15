# Run a reactablePlus example app

Launches one of the bundled demo apps that showcase package features.
Call with no arguments to see available examples.

## Usage

``` r
runExample(example = NULL, port = NULL)
```

## Arguments

- example:

  Character. The name of the example to run. Available examples:

  `"inventory"`

  :   Product inventory tracker — all 6 primitive input types,
      value-based gating, reset, badge column.

  `"roster"`

  :   Team roster — row selection, chained selection + value gating,
      reset, selected_ids output.

  `"evaluations"`

  :   Student evaluations — attendance_picker with custom sections,
      notes_input, badge_render_fn, row_class_fn, mixing picker and
      primitive columns.

- port:

  Integer. Port to run the app on. Default `NULL` (Shiny picks an
  available port).

## Value

Called for its side effect (launches a Shiny app).

## Examples

``` r
if (FALSE) { # \dontrun{
# List available examples
runExample()

# Launch a specific example
runExample("inventory")
runExample("roster")
runExample("evaluations")
} # }
```
