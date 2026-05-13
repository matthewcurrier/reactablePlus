#' Notes text input
#'
#' A simple single-line text input designed for reactable cells.
#' Returns the text value to Shiny, or `NULL` if empty.
#'
#' @param inputId Character. The Shiny input ID.
#' @param value Optional initial text value.
#' @param placeholder Placeholder text. Default `"Optional"`.
#'   Use `"Curriculum, evaluator\u2026"` for homeschool rows.
#' @param width CSS width. Default `"100%"`.
#'
#' @return A tag with attached dependencies.
#'
#' @examples
#' \dontrun{
#' notesInput("notes_PK")
#' notesInput("notes_09", placeholder = "Curriculum, evaluator\u2026")
#' }
#'
#' @export
#' @importFrom htmltools tags
notesInput <- function(inputId,
                       value = NULL,
                       placeholder = "Optional",
                       width = "100%") {

  tag <- tags$div(
    id = inputId,
    class = "sh-notes-input",
    style = if (!is.null(width)) paste0("width:", width),
    `data-initial-value` = value %||% "",
    `data-placeholder` = placeholder
  )

  htmltools::attachDependencies(tag, list(popoverDep(), notes_input_dep()))
}


#' Update a notes input from the server
#'
#' @param session The Shiny session object.
#' @param inputId Character. The input ID to update.
#' @param value New text value, or `NULL` to clear.
#' @param placeholder New placeholder text.
#'
#' @return Called for its side effect; returns invisibly.
#' @export
#' @importFrom shiny getDefaultReactiveDomain
updateNotesInput <- function(session = getDefaultReactiveDomain(),
                             inputId,
                             value = NULL,
                             placeholder = NULL) {
  msg <- list()
  if (!missing(value)) msg$value <- value
  if (!is.null(placeholder)) msg$placeholder <- placeholder
  session$sendInputMessage(inputId, msg)
}
