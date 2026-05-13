# Update a homeschool picker from the server

Update a homeschool picker from the server

## Usage

``` r
updateHomeschoolPickerInput(
  session = getDefaultReactiveDomain(),
  inputId,
  value = NULL
)
```

## Arguments

- session:

  The Shiny session object.

- inputId:

  Character. The input ID to update.

- value:

  New value — a list with details or `NULL` to turn off.

## Value

Called for its side effect; returns invisibly.
