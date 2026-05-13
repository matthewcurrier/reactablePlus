# Update a school picker from the server

Update a school picker from the server

## Usage

``` r
updateSchoolPickerInput(
  session = getDefaultReactiveDomain(),
  inputId,
  value = NULL,
  show_nces_id = NULL
)
```

## Arguments

- session:

  The Shiny session object.

- inputId:

  Character. The input ID to update.

- value:

  New school value (named list) or `NULL` to clear.

- show_nces_id:

  Logical. Update the NCES ID display toggle.

## Value

Called for its side effect; returns invisibly.
