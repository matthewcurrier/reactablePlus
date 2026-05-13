#' Gear popover settings input
#'
#' Creates a gear icon button that opens a settings popover with
#' configurable toggle switches. Returns a named list of boolean
#' values to Shiny.
#'
#' @param inputId Character. The Shiny input ID.
#' @param toggles A named list of toggle definitions. Each element
#'   should be a list with `label` (character), `desc` (character,
#'   optional description), and `value` (logical, default state).
#'   The names become the keys in the returned value.
#'
#' @return A tag with attached dependencies.
#'
#' @examples
#' \dontrun{
#' gearPopoverInput("settings", toggles = list(
#'   show_year = list(
#'     label = "Show School Year column",
#'     desc  = "Display the school-year column in the table.",
#'     value = TRUE
#'   ),
#'   show_nces_id = list(
#'     label = "Show NCES ID",
#'     desc  = "Display the federal school identifier inline.",
#'     value = TRUE
#'   ),
#'   compact_rows = list(
#'     label = "Compact rows",
#'     desc  = "Tighten row height; hide secondary metadata.",
#'     value = FALSE
#'   ),
#'   show_homeschool = list(
#'     label = "Show Homeschool column",
#'     desc  = "Add a column to mark homeschool grades and capture details.",
#'     value = FALSE
#'   )
#' ))
#' }
#'
#' @export
#' @importFrom htmltools tags
#' @importFrom jsonlite toJSON
gearPopoverInput <- function(inputId, toggles = list()) {

  # Convert named list to array of { key, label, desc, value }
  toggle_array <- lapply(names(toggles), function(nm) {
    t <- toggles[[nm]]
    list(
      key   = nm,
      label = t$label %||% nm,
      desc  = t$desc %||% NULL,
      value = isTRUE(t$value)
    )
  })

  tag <- tags$div(
    id = inputId,
    class = "sh-gear-wrap",
    `data-toggles` = toJSON(toggle_array, auto_unbox = TRUE, null = "null")
  )

  htmltools::attachDependencies(tag, list(popoverDep(), gear_popover_dep()))
}


#' Update a gear popover from the server
#'
#' @param session The Shiny session object.
#' @param inputId Character. The input ID to update.
#' @param value Named list of toggle values to update.
#'
#' @return Called for its side effect; returns invisibly.
#' @export
#' @importFrom shiny getDefaultReactiveDomain
updateGearPopoverInput <- function(session = getDefaultReactiveDomain(),
                                    inputId,
                                    value = NULL) {
  msg <- list()
  if (!is.null(value)) msg$value <- value
  session$sendInputMessage(inputId, msg)
}
