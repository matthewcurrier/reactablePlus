# School picker input

Creates a school picker widget with typeahead search. When empty,
renders a dashed-border trigger button. When filled, renders a card
showing school name, public/private pill, district, city/state, NCES ID,
and action links (change, fill down, clear).

## Usage

``` r
schoolPickerInput(
  inputId,
  value = NULL,
  grade_label = NULL,
  grade_key = NULL,
  show_nces_id = TRUE,
  trigger_label = "+ Pick school",
  popover_title = "Find school",
  search_placeholder = "Search by school name, city, district…",
  empty_hint = "Type 2+ characters to search 100k+ US schools",
  no_match_hint = "No schools match. Check spelling — picking is required.",
  show_fill_down = TRUE,
  ns = "",
  width = "100%"
)
```

## Arguments

- inputId:

  Character. The Shiny input ID. The value is a named list with `id`,
  `name`, `district`, `city`, `state`, `type`, or `NULL`.

- value:

  Optional initial value — a list with the school fields above.

- grade_label:

  Optional display label (e.g. `"3rd grade"`) for accessible naming.

- grade_key:

  The grade's two-char key (e.g. `"PK"`, `"03"`). Used by the fill-down
  action to tell the server which grade triggered it.

- show_nces_id:

  Logical. Whether to show the NCES ID in the filled state and search
  results. Default `TRUE`.

- trigger_label:

  Character. Text on the empty-state trigger button. Default
  `"+ Pick school"`.

- popover_title:

  Character. Popover header text. Default `"Find school"`.

- search_placeholder:

  Character. Placeholder in the search input. Default
  `"Search by school name, city, district\u2026"`.

- empty_hint:

  Character. Hint text shown before the user types. Default
  `"Type 2+ characters to search 100k+ US schools"`.

- no_match_hint:

  Character. Text shown when search returns no results. Default
  `"No schools match. Check spelling \u2014 picking is required."`.

- show_fill_down:

  Logical. Whether to show the "fill down" action link in the filled
  state. Default `TRUE`.

- ns:

  Character. The Shiny module namespace prefix for search communication.
  Pass `session$ns("")` from within a module. For top-level apps, leave
  as `""`.

- width:

  CSS width. Default `"100%"`.

## Value

An
[htmltools::tagList](https://rstudio.github.io/htmltools/reference/tagList.html).

## Details

Search is server-side: the widget sends queries to R via
`input$<ns>school_search`; use
[`useSchoolSearch()`](https://matthewcurrier.github.io/reactablePlus/reference/useSchoolSearch.md)
in your server function to wire up the response.

Trigger and popover labels are configurable so the widget can be reused
for any server-side typeahead scenario (provider search, facility
lookup, etc.).

## Examples

``` r
if (FALSE) { # \dontrun{
# Default (backward compatible)
schoolPickerInput("school_PK", grade_label = "PreK", grade_key = "PK")

# Custom labels for provider search
schoolPickerInput("provider_PK",
  trigger_label = "+ Find provider",
  popover_title = "Search providers",
  search_placeholder = "Type provider name or specialty\u2026",
  empty_hint = "Type 2+ characters to search providers",
  no_match_hint = "No providers found.",
  show_nces_id = FALSE,
  show_fill_down = FALSE,
  grade_label = "PreK",
  grade_key = "PK"
)
} # }
```
