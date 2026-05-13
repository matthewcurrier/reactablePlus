# Update an attendance picker from the server

Update an attendance picker from the server

## Usage

``` r
updateAttendancePickerInput(
  session = getDefaultReactiveDomain(),
  inputId,
  value = NULL
)
```

## Arguments

- session:

  The Shiny session object (usually `session`).

- inputId:

  Character. The input ID to update.

- value:

  New value — a named list keyed by section keys, or `NULL` to clear.

## Value

Called for its side effect; returns invisibly.
