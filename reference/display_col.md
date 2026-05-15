# Build a read-only display column specification

Defines a non-editable column whose values come from `source_data` in
dynamic mode. Display columns provide context alongside editable
[`widget_col()`](https://matthewcurrier.github.io/reactablePlus/reference/widget_col.md)
columns — e.g. an email address, department name, or enrollment date
that the user can see but not edit.

## Usage

``` r
display_col(id, label, width = NULL, min_width = NULL, render_fn = NULL)
```

## Arguments

- id:

  `character(1)`. The name of the column in `source_data` to display.
  Also used as the column key in the reactable data frame.

- label:

  `character(1)`. The column header text.

- width:

  Integer or `NULL`. Fixed column width in pixels.

- min_width:

  Integer or `NULL`. Minimum column width.

- render_fn:

  Function or `NULL`. An optional custom cell renderer
  `(value, row_key)` -\> HTML string. When `NULL` (the default), values
  are shown as plain escaped text.

## Value

A `display_col` list (S3 class `"display_col"`).

## Examples

``` r
display_col("email", "Email", width = 200)
#> $id
#> [1] "email"
#> 
#> $label
#> [1] "Email"
#> 
#> $width
#> [1] 200
#> 
#> $min_width
#> NULL
#> 
#> $render_fn
#> NULL
#> 
#> attr(,"class")
#> [1] "display_col"
display_col("department", "Dept", min_width = 120)
#> $id
#> [1] "department"
#> 
#> $label
#> [1] "Dept"
#> 
#> $width
#> NULL
#> 
#> $min_width
#> [1] 120
#> 
#> $render_fn
#> NULL
#> 
#> attr(,"class")
#> [1] "display_col"
display_col("status", "Status", render_fn = function(value, row_key) {
  cls <- if (identical(value, "active")) "badge-success" else "badge-muted"
  sprintf('<span class="%s">%s</span>', cls, value)
})
#> $id
#> [1] "status"
#> 
#> $label
#> [1] "Status"
#> 
#> $width
#> NULL
#> 
#> $min_width
#> NULL
#> 
#> $render_fn
#> function (value, row_key) 
#> {
#>     cls <- if (identical(value, "active")) 
#>         "badge-success"
#>     else "badge-muted"
#>     sprintf("<span class=\"%s\">%s</span>", cls, value)
#> }
#> <environment: 0x55986a176ca8>
#> 
#> attr(,"class")
#> [1] "display_col"
```
