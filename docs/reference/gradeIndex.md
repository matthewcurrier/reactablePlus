# Grade ordering index

Converts a grade key to a numeric index for comparison. PK=0, K=1, 01=2,
02=3, ..., 12=13.

## Usage

``` r
gradeIndex(key)
```

## Arguments

- key:

  Character. One or more grade keys.

## Value

Integer vector.

## Examples

``` r
gradeIndex("PK")  # 0
#> [1] 0
gradeIndex("K")   # 1
#> [1] 1
gradeIndex("05")  # 6
#> [1] 6
gradeIndex("12")  # 13
#> [1] 13
```
