# Notes text input

A simple single-line text input designed for reactable cells. Returns
the text value to Shiny, or `NULL` if empty.

## Usage

``` r
notesInput(inputId, value = NULL, placeholder = "Optional", width = "100%")
```

## Arguments

- inputId:

  Character. The Shiny input ID.

- value:

  Optional initial text value.

- placeholder:

  Placeholder text. Default `"Optional"`. Use
  `"Curriculum, evaluator\u2026"` for homeschool rows.

- width:

  CSS width. Default `"100%"`.

## Value

A tag with attached dependencies.

## Examples

``` r
if (FALSE) { # \dontrun{
notesInput("notes_PK")
notesInput("notes_09", placeholder = "Curriculum, evaluator\u2026")
} # }
```
