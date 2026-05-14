#' @importFrom htmltools htmlDependency tagList
#' @noRd
NULL

#' Popover core dependency (shared JS + CSS)
#'
#' Returns the [htmltools::htmlDependency] for the shared popover
#' infrastructure. All picker widgets depend on this. It is attached
#' automatically when you call any picker input function — you
#' normally don't need to call this yourself.
#'
#' @return An [htmltools::htmlDependency] object.
#'
#' @importFrom htmltools htmlDependency
#' @export
popoverDep <- function() {
  htmlDependency(
    name = "popover-core",
    version = "0.2.0",
    src = system.file("assets", package = "reactablePlus"),
    script = "js/popover-core.js",
    stylesheet = "css/popover-core.css"
  )
}

#' @noRd
attendance_picker_dep <- function() {
  htmlDependency(
    name = "attendance-picker",
    version = "0.2.0",
    src = system.file("assets", package = "reactablePlus"),
    script = "js/attendance-picker-binding.js"
  )
}

#' @noRd
school_picker_dep <- function() {
  htmlDependency(
    name = "school-picker",
    version = "0.2.0",
    src = system.file("assets", package = "reactablePlus"),
    script = "js/school-picker-binding.js",
    stylesheet = "css/school-picker.css"
  )
}

#' @noRd
homeschool_picker_dep <- function() {
  htmlDependency(
    name = "homeschool-picker",
    version = "0.2.0",
    src = system.file("assets", package = "reactablePlus"),
    script = "js/homeschool-picker-binding.js"
  )
}

#' @noRd
notes_input_dep <- function() {
  htmlDependency(
    name = "notes-input",
    version = "0.2.0",
    src = system.file("assets", package = "reactablePlus"),
    script = "js/notes-input-binding.js"
  )
}

#' @noRd
gear_popover_dep <- function() {
  htmlDependency(
    name = "gear-popover",
    version = "0.2.0",
    src = system.file("assets", package = "reactablePlus"),
    script = "js/gear-popover-binding.js",
    stylesheet = "css/gear-popover.css"
  )
}

#' In-place update message handlers for cross-cell effects (mutual
#' exclusion display swap, row class toggle). Avoids full reactable
#' re-renders for value changes.
#' @noRd
reactable_plus_updates_dep <- function() {
  htmlDependency(
    name = "reactable-plus-updates",
    version = "0.2.0",
    src = system.file("assets", package = "reactablePlus"),
    script = "js/reactable-plus-updates.js"
  )
}

#' Include all reactablePlus widget dependencies
#'
#' Drop this into your UI (e.g. inside `fluidPage()` or a module's
#' `tagList()`) to ensure all JS and CSS for picker widgets is loaded
#' at the page level. This is **required** when embedding pickers inside
#' `reactable` cells, because reactable's React-based rendering does
#' not process [htmltools::htmlDependency] objects returned from cell
#' functions.
#'
#' Calling this once covers SchoolPicker, AttendancePicker,
#' HomeschoolPicker, NotesInput, and GearPopover. Duplicate calls are
#' harmless (htmltools deduplicates).
#'
#' @return An [htmltools::tagList] of HTML dependencies (invisible in the UI).
#'
#' @examples
#' \dontrun{
#' ui <- fluidPage(
#'   useReactablePlus(),
#'   reactableOutput("table")
#' )
#' }
#'
#' @importFrom htmltools tagList
#' @export
useReactablePlus <- function() {
  tagList(
    popoverDep(),
    attendance_picker_dep(),
    school_picker_dep(),
    homeschool_picker_dep(),
    notes_input_dep(),
    gear_popover_dep(),
    reactable_plus_updates_dep()
  )
}

#' @rdname useReactablePlus
#' @description `useSchoolHistory()` is a deprecated alias for
#'   `useReactablePlus()`. It will be removed in a future release.
#' @export
useSchoolHistory <- function() {
  .Deprecated("useReactablePlus")
  useReactablePlus()
}

#' Bind picker inputs after a reactable renders
#'
#' Wraps a reactable widget with an `onRender` callback that initializes
#' picker inputs inside table cells. Required because Shiny's `bindAll()`
#' runs before reactable creates its DOM.
#'
#' @param widget A reactable widget.
#' @param delay_ms Milliseconds to wait after render. Default 300.
#' @return The widget with the callback attached.
#'
#' @examples
#' \dontrun{
#' output$table <- renderReactable({
#'   tbl <- reactable(data, columns = list(
#'     school = colDef(html = TRUE, cell = function(value, index) {
#'       as.character(schoolPickerInput("school_1", grade_key = "PK"))
#'     })
#'   ))
#'   bindPickersOnRender(tbl)
#' })
#' }
#'
#' @importFrom htmlwidgets onRender
#' @export
bindPickersOnRender <- function(widget, delay_ms = 300L) {
  htmlwidgets::onRender(
    widget,
    sprintf(
      "function(el){setTimeout(function(){window.shBindPickers(el);},%d);}",
      as.integer(delay_ms)
    )
  )
}
