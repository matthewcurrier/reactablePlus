#' Attendance picker input
#'
#' Creates an attendance picker widget that renders as a dashed-border
#' trigger button when empty and a pill summary when filled. Clicking
#' opens a popover with configurable radio groups and an optional notes
#' textarea.
#'
#' The popover's radio sections are fully configurable via the `sections`
#' parameter, making this widget reusable for any multi-field rating
#' scenario (behavior tracking, therapy progress, etc.).
#'
#' @param inputId Character. The Shiny input ID. The value returned to
#'   the server is a named list keyed by each section's `key`, plus
#'   `notes` if notes are enabled, or `NULL` if nothing is set.
#' @param value Optional initial value — a named list keyed by section
#'   keys with character values.
#' @param grade_label Optional display label for this row's grade
#'   (e.g. `"3rd grade"`). Used in the trigger's `aria-label` for
#'   screen reader context.
#' @param sections A list of section definitions for the popover body.
#'   Each section is a list with:
#'   \describe{
#'     \item{key}{Character. The key in the returned value list
#'       (e.g. `"school"`, `"class_"`).}
#'     \item{label}{Character. The heading above the radio group.}
#'     \item{levels}{Character vector. The radio options.}
#'     \item{pill_prefix}{Character (optional). Text prepended to the
#'       pill display when this section is filled
#'       (e.g. `"Class: "` produces `"Class: Excellent"`).
#'       Default is no prefix — the raw level is shown.}
#'     \item{pill_icon}{Character (optional). Pill icon style:
#'       `"school"` (default for first section), `"pencil"`, or
#'       `NULL` for no icon.}
#'     \item{impact_display}{List (optional). If present, renders a
#'       sub-line instead of a pill for this section. Should have
#'       named entries mapping level values to display strings,
#'       e.g. `list(Yes = "\u26A0 impacts academics",
#'       No = "no academic impact")`.}
#'   }
#'   Defaults to the school history attendance sections (School
#'   Attendance, Class Attendance, Impacts Academics).
#' @param trigger_label Character. Text on the empty-state trigger
#'   button. Default `"+ Mark attendance"`.
#' @param popover_title Character. Popover header text.
#'   Default `"Attendance"`.
#' @param show_notes Logical. Whether to include a notes textarea
#'   in the popover. Default `TRUE`.
#' @param notes_placeholder Character. Placeholder for the notes
#'   textarea. Default `"Absences, tardies, context for this
#'   school year\u2026"`.
#' @param width CSS width. Default `"100%"`.
#'
#' @return An [htmltools::tagList] suitable for use in Shiny UI, including
#'   inside `reactable::colDef(cell = ...)` renderers.
#'
#' @examples
#' \dontrun{
#' # Default school-history attendance (backward compatible)
#' attendancePickerInput("att_pk", grade_label = "PreK")
#'
#' # Pre-filled
#' attendancePickerInput("att_k",
#'   value = list(school = "Good", class_ = "Excellent", impacts = "No"),
#'   grade_label = "Kindergarten"
#' )
#'
#' # Custom sections for behavior tracking
#' attendancePickerInput("beh_pk",
#'   sections = list(
#'     list(key = "behavior", label = "Classroom Behavior",
#'          levels = c("Exemplary", "Meets Expectations",
#'                     "Needs Improvement", "Unsatisfactory")),
#'     list(key = "participation", label = "Participation",
#'          levels = c("Active", "Moderate", "Minimal"))
#'   ),
#'   trigger_label = "+ Rate behavior",
#'   popover_title = "Behavior",
#'   notes_placeholder = "Context, observations\u2026"
#' )
#' }
#'
#'
#' @export
#' @importFrom htmltools tagList tags
#' @importFrom jsonlite toJSON
attendancePickerInput <- function(inputId,
                                  value = NULL,
                                  grade_label = NULL,
                                  sections = NULL,
                                  trigger_label = "+ Mark attendance",
                                  popover_title = "Attendance",
                                  show_notes = TRUE,
                                  notes_placeholder = "Absences, tardies, context for this school year\u2026",
                                  width = "100%") {

  # Default sections: school history attendance

  if (is.null(sections)) {
    sections <- list(
      list(
        key    = "school",
        label  = "School Attendance",
        levels = c("Excellent", "Good", "Satisfactory", "Poor")
      ),
      list(
        key         = "class_",
        label       = "Class Attendance",
        levels      = c("Excellent", "Good", "Satisfactory", "Poor"),
        pill_prefix = "Class: ",
        pill_icon   = "pencil"
      ),
      list(
        key            = "impacts",
        label          = "Does student attendance negatively impact academics?",
        levels         = c("Yes", "No"),
        impact_display = list(
          Yes = "\u26A0 impacts academics",
          No  = "no academic impact"
        )
      )
    )
  }

  # Normalise value to a JSON-safe list
  val_json <- if (!is.null(value)) {
    toJSON(value, auto_unbox = TRUE, null = "null")
  } else {
    "{}"
  }

  # Sections config as JSON for the JS binding
  sections_json <- toJSON(sections, auto_unbox = TRUE, null = "null")

  tag <- tags$div(
    id = inputId,
    class = "sh-attendance-picker pv-container",
    style = if (!is.null(width)) paste0("width:", width),
    `data-initial-value`    = val_json,
    `data-grade-label`      = grade_label,
    `data-sections`         = sections_json,
    `data-trigger-label`    = trigger_label,
    `data-popover-title`    = popover_title,
    `data-show-notes`       = tolower(as.character(show_notes)),
    `data-notes-placeholder` = notes_placeholder
  )

  htmltools::attachDependencies(tag, list(popoverDep(), attendance_picker_dep()))
}


#' Update an attendance picker from the server
#'
#' @param session The Shiny session object (usually `session`).
#' @param inputId Character. The input ID to update.
#' @param value New value — a named list keyed by section keys,
#'   or `NULL` to clear.
#'
#' @return Called for its side effect; returns invisibly.
#' @export
#' @importFrom shiny getDefaultReactiveDomain
updateAttendancePickerInput <- function(session = getDefaultReactiveDomain(),
                                        inputId,
                                        value = NULL) {
  session$sendInputMessage(inputId, list(value = value))
}
