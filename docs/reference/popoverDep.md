# Popover core dependency (shared JS + CSS)

Returns the
[htmltools::htmlDependency](https://rstudio.github.io/htmltools/reference/htmlDependency.html)
for the shared popover infrastructure. All picker widgets depend on
this. It is attached automatically when you call any picker input
function — you normally don't need to call this yourself.

## Usage

``` r
popoverDep()
```

## Value

An
[htmltools::htmlDependency](https://rstudio.github.io/htmltools/reference/htmlDependency.html)
object.
