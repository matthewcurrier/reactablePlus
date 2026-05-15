# Update a notes input from the server

Update a notes input from the server

## Usage

``` r
updateNotesInput(
  session = getDefaultReactiveDomain(),
  inputId,
  value = NULL,
  placeholder = NULL
)
```

## Arguments

- session:

  The Shiny session object.

- inputId:

  Character. The input ID to update.

- value:

  New text value, or `NULL` to clear.

- placeholder:

  New placeholder text.

## Value

Called for its side effect; returns invisibly.
