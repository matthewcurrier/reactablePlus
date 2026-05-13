# Check if a grade falls within a school's grade range

Check if a grade falls within a school's grade range

## Usage

``` r
gradeInRange(grade_key, low_grade, high_grade)
```

## Arguments

- grade_key:

  Character. The grade to check (e.g. `"05"`).

- low_grade:

  Character. The school's lowest grade (e.g. `"K"`).

- high_grade:

  Character. The school's highest grade (e.g. `"08"`).

## Value

Logical.

## Examples

``` r
gradeInRange("05", "K", "08")   # TRUE
#> [1] TRUE
gradeInRange("09", "K", "08")   # FALSE
#> [1] FALSE
gradeInRange("PK", "PK", "12")  # TRUE
#> [1] TRUE
```
