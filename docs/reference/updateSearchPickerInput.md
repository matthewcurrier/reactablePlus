# Update a search picker from the server

`updateSchoolPickerInput()` is a deprecated alias for
`updateSearchPickerInput()`. It will be removed in a future release.

## Usage

``` r
updateSearchPickerInput(
  session = getDefaultReactiveDomain(),
  inputId,
  value = NULL,
  show_nces_id = NULL
)

updateSchoolPickerInput(...)
```

## Arguments

- session:

  The Shiny session object.

- inputId:

  Character. The input ID to update.

- value:

  New value (named list) or `NULL` to clear.

- show_nces_id:

  Logical. Update the NCES ID display toggle.

- ...:

  Arguments passed to `updateSearchPickerInput()`.

## Value

Called for its side effect; returns invisibly.
