#' Homeschool picker input
#'
#' Creates a homeschool picker widget. When empty, renders a dashed-border
#' trigger button. When filled, renders a pill summary with optional
#' provider and curriculum details.
#'
#' Clicking the empty trigger immediately flags the row as active and
#' opens the details popover. The popover fields (provider, curriculum,
#' notes) are all optional.
#'
#' The provider dropdown options, field labels, and trigger text are all
#' configurable, making this widget reusable for any "flag + details"
#' pattern (e.g. therapy provider tracking, tutoring arrangements).
#'
#' @param inputId Character. The Shiny input ID. The value is a named list
#'   with `by`, `by_other`, `curriculum`, `notes` when active,
#'   or `NULL` when inactive.
#' @param value Optional initial value — a list with any combination of
#'   the fields above. Pass `list()` for active with no details.
#'   Pass `NULL` (default) for inactive.
#' @param grade_label Optional display label for accessible naming.
#' @param grade_key The grade's two-char key (e.g. `"PK"`, `"03"`).
#' @param providers Character vector of provider dropdown options.
#'   If the last element is `"Other"`, an additional free-text input
#'   is shown when `"Other"` is selected.
#'   Default: `c("Mother", "Father", "Grandmother", "Grandfather",
#'   "Guardian", "Tutor", "Co-op", "Online program", "Other")`.
#' @param provider_label Character. Label above the provider dropdown.
#'   Default `"Who provided homeschooling?"`.
#' @param curriculum_label Character. Label above the curriculum text
#'   input. Default `"Curriculum / program"`.
#' @param curriculum_placeholder Character. Placeholder for the
#'   curriculum field. Default `"e.g. Time4Learning, Sonlight, custom"`.
#' @param show_curriculum Logical. Whether to include the curriculum
#'   field. Default `TRUE`.
#' @param show_notes Logical. Whether to include the notes textarea.
#'   Default `TRUE`.
#' @param notes_placeholder Character. Placeholder for the notes
#'   textarea. Default `"Evaluator, co-op participation, any
#'   context\u2026"`.
#' @param trigger_label Character. Text on the empty-state trigger
#'   button. Default `"+ Mark homeschool"`.
#' @param trigger_sub_label Character or `NULL`. Sub-label on the
#'   trigger button. Default `"(details optional)"`.
#' @param popover_title Character. Popover header text.
#'   Default `"Homeschool details"`.
#' @param popover_title_sub Character or `NULL`. Popover header
#'   sub-text. Default `"(optional)"`.
#' @param filled_pill_label Character. Text for the primary pill
#'   when active. Default `"Homeschool"`.
#' @param clear_label Character. Text for the footer clear/remove
#'   link. Default `"remove homeschool"`.
#' @param ns Shiny module namespace prefix. Default `""`.
#' @param width CSS width. Default `"100%"`.
#'
#' @return A tag with attached dependencies.
#'
#' @examples
#' \dontrun{
#' # Default school-history homeschool (backward compatible)
#' homeschoolPickerInput("hs_PK", grade_label = "PreK", grade_key = "PK")
#'
#' # Pre-filled
#' homeschoolPickerInput("hs_K",
#'   value = list(by = "Mother", curriculum = "Time4Learning"),
#'   grade_label = "Kindergarten",
#'   grade_key = "K"
#' )
#'
#' # Custom use: therapy provider tracking
#' homeschoolPickerInput("therapy_PK",
#'   providers = c("Occupational Therapist", "Speech Therapist",
#'                 "Physical Therapist", "Behavioral Therapist", "Other"),
#'   provider_label = "Type of therapy",
#'   curriculum_label = "Provider / agency",
#'   curriculum_placeholder = "e.g. ABC Therapy Group",
#'   trigger_label = "+ Add therapy",
#'   trigger_sub_label = NULL,
#'   popover_title = "Therapy details",
#'   popover_title_sub = NULL,
#'   filled_pill_label = "Therapy",
#'   clear_label = "remove therapy",
#'   grade_label = "PreK",
#'   grade_key = "PK"
#' )
#' }
#'
#' @export
#' @importFrom htmltools tags
#' @importFrom jsonlite toJSON
homeschoolPickerInput <- function(
  inputId,
  value = NULL,
  grade_label = NULL,
  grade_key = NULL,
  providers = c(
    "Mother",
    "Father",
    "Grandmother",
    "Grandfather",
    "Guardian",
    "Tutor",
    "Co-op",
    "Online program",
    "Other"
  ),
  provider_label = "Who provided homeschooling?",
  curriculum_label = "Curriculum / program",
  curriculum_placeholder = "e.g. Time4Learning, Sonlight, custom",
  show_curriculum = TRUE,
  show_notes = TRUE,
  notes_placeholder = "Evaluator, co-op participation, any context\u2026",
  trigger_label = "+ Mark homeschool",
  trigger_sub_label = "(details optional)",
  popover_title = "Homeschool details",
  popover_title_sub = "(optional)",
  filled_pill_label = "Homeschool",
  clear_label = "remove homeschool",
  ns = "",
  width = "100%"
) {
  val_json <- if (!is.null(value)) {
    if (length(value) == 0L) {
      "{}"
    } else {
      toJSON(value, auto_unbox = TRUE, null = "null")
    }
  } else {
    "null"
  }

  # Build config object for JS
  config <- list(
    providers = providers,
    provider_label = provider_label,
    curriculum_label = curriculum_label,
    curriculum_placeholder = curriculum_placeholder,
    show_curriculum = show_curriculum,
    show_notes = show_notes,
    notes_placeholder = notes_placeholder,
    trigger_label = trigger_label,
    trigger_sub_label = trigger_sub_label,
    popover_title = popover_title,
    popover_title_sub = popover_title_sub,
    filled_pill_label = filled_pill_label,
    clear_label = clear_label
  )

  tag <- tags$div(
    id = inputId,
    class = "sh-homeschool-picker pv-container",
    style = if (!is.null(width)) paste0("width:", width),
    `data-initial-value` = val_json,
    `data-grade-label` = grade_label,
    `data-grade-key` = grade_key,
    `data-ns` = ns,
    `data-config` = toJSON(config, auto_unbox = TRUE, null = "null")
  )

  htmltools::attachDependencies(
    tag,
    list(popoverDep(), homeschool_picker_dep())
  )
}


#' Update a homeschool picker from the server
#'
#' @param session The Shiny session object.
#' @param inputId Character. The input ID to update.
#' @param value New value — a list with details or `NULL` to turn off.
#'
#' @return Called for its side effect; returns invisibly.
#' @export
#' @importFrom shiny getDefaultReactiveDomain
updateHomeschoolPickerInput <- function(
  session = getDefaultReactiveDomain(),
  inputId,
  value = NULL
) {
  msg <- list()
  if (!missing(value)) {
    msg$value <- value
  }
  session$sendInputMessage(inputId, msg)
}
