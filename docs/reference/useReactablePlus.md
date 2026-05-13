# Include all reactablePlus widget dependencies

Drop this into your UI (e.g. inside
[`fluidPage()`](https://rdrr.io/pkg/shiny/man/fluidPage.html) or a
module's
[`tagList()`](https://rstudio.github.io/htmltools/reference/tagList.html))
to ensure all JS and CSS for picker widgets is loaded at the page level.
This is **required** when embedding pickers inside `reactable` cells,
because reactable's React-based rendering does not process
[htmltools::htmlDependency](https://rstudio.github.io/htmltools/reference/htmlDependency.html)
objects returned from cell functions.

`useSchoolHistory()` is a deprecated alias for `useReactablePlus()`. It
will be removed in a future release.

## Usage

``` r
useReactablePlus()

useSchoolHistory()
```

## Value

An
[htmltools::tagList](https://rstudio.github.io/htmltools/reference/tagList.html)
of HTML dependencies (invisible in the UI).

## Details

Calling this once covers SchoolPicker, AttendancePicker,
HomeschoolPicker, NotesInput, and GearPopover. Duplicate calls are
harmless (htmltools deduplicates).

## Examples

``` r
if (FALSE) { # \dontrun{
ui <- fluidPage(
  useReactablePlus(),
  reactableOutput("table")
)
} # }
```
