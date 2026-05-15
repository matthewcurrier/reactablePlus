# Normalize dropdown choices to list-of-lists format

Converts shorthand choice formats into the canonical list-of-lists
structure expected by
[`widget_col()`](https://matthewcurrier.github.io/reactablePlus/reference/widget_col.md)
and the cell renderers. Called automatically during
[`widget_col()`](https://matthewcurrier.github.io/reactablePlus/reference/widget_col.md)
validation for `type = "dropdown"`, but can also be used directly.

## Usage

``` r
normalize_choices(choices)
```

## Arguments

- choices:

  A character vector, named character vector, or list of lists. Empty
  strings and `NA` values in character vectors are rejected.

## Value

A list of lists, each with `label` (character) and `value`.

## Details

Three formats are accepted:

- Unnamed character vector:

  `c("High", "Medium", "Low")` — label and value are identical.

- Named character vector:

  `c("High" = "high", "Medium" = "med")` — names become labels, values
  become values.

- List of lists:

  `list(list(label = "High", value = "high"), ...)` — canonical format,
  returned as-is.

## Examples

``` r
# Unnamed vector — label == value
normalize_choices(c("High", "Medium", "Low"))
#> [[1]]
#> [[1]]$label
#> [1] "High"
#> 
#> [[1]]$value
#> [1] "High"
#> 
#> 
#> [[2]]
#> [[2]]$label
#> [1] "Medium"
#> 
#> [[2]]$value
#> [1] "Medium"
#> 
#> 
#> [[3]]
#> [[3]]$label
#> [1] "Low"
#> 
#> [[3]]$value
#> [1] "Low"
#> 
#> 

# Named vector — names are labels
normalize_choices(c("High" = "high", "Medium" = "med", "Low" = "low"))
#> [[1]]
#> [[1]]$label
#> [1] "High"
#> 
#> [[1]]$value
#> [1] "high"
#> 
#> 
#> [[2]]
#> [[2]]$label
#> [1] "Medium"
#> 
#> [[2]]$value
#> [1] "med"
#> 
#> 
#> [[3]]
#> [[3]]$label
#> [1] "Low"
#> 
#> [[3]]$value
#> [1] "low"
#> 
#> 

# Already canonical — returned as-is
normalize_choices(list(
  list(label = "Good", value = 3),
  list(label = "Bad",  value = 1)
))
#> [[1]]
#> [[1]]$label
#> [1] "Good"
#> 
#> [[1]]$value
#> [1] 3
#> 
#> 
#> [[2]]
#> [[2]]$label
#> [1] "Bad"
#> 
#> [[2]]$value
#> [1] 1
#> 
#> 
```
