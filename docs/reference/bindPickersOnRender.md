# Bind picker inputs after a reactable renders

Wraps a reactable widget with an `onRender` callback that initializes
picker inputs inside table cells. Required because Shiny's `bindAll()`
runs before reactable creates its DOM.

## Usage

``` r
bindPickersOnRender(widget, delay_ms = 300L)
```

## Arguments

- widget:

  A reactable widget.

- delay_ms:

  Milliseconds to wait after render. Default 300.

## Value

The widget with the callback attached.

## Examples

``` r
if (FALSE) { # \dontrun{
output$table <- renderReactable({
  tbl <- reactable(data, columns = list(
    school = colDef(html = TRUE, cell = function(value, index) {
      as.character(schoolPickerInput("school_1", grade_key = "PK"))
    })
  ))
  bindPickersOnRender(tbl)
})
} # }
```
