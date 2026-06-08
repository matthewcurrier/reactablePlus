# Bind picker inputs after a reactable renders

Wraps a reactable widget with an `onRender` callback that initializes
picker inputs inside table cells. Required because Shiny's `bindAll()`
runs before reactable creates its DOM.

## Usage

``` r
bindPickersOnRender(widget, fallback_ms = 600L)
```

## Arguments

- widget:

  A reactable widget (the return value of
  [`reactable::reactable()`](https://glin.github.io/reactable/reference/reactable.html)).

- fallback_ms:

  Integer. Milliseconds before the safety-net `setTimeout` fires if the
  `MutationObserver` has not already triggered binding. Default `600L`.
  Increase for very large tables on slow connections; you will rarely
  need to change this.

## Value

The widget with the `onRender` callback attached.

## Details

Binding is triggered by a `MutationObserver` that watches the table
container and fires `shBindPickers()` the moment reactable paints its
first picker cell — typically within one animation frame. A plain
`setTimeout` of `fallback_ms` milliseconds is kept as a safety net in
case the observer never fires (empty table, future reactable internals
change, or environments where `MutationObserver` is unavailable).
Whichever fires first wins; the other is cancelled, so there is no
double-binding.

## Examples

``` r
if (FALSE) { # \dontrun{
output$table <- renderReactable({
  tbl <- reactable(data, columns = list(
    school = colDef(html = TRUE, cell = function(value, index) {
      as.character(searchPickerInput("school_1", grade_key = "PK"))
    })
  ))
  bindPickersOnRender(tbl)
})
} # }
```
