#' School picker input
#'
#' Creates a school picker widget with typeahead search. When empty, renders a
#' dashed-border trigger button. When filled, renders a card showing
#' school name, public/private pill, district, city/state, NCES ID, and action
#' links (change, fill down, clear).
#'
#' Search is server-side: the widget sends queries to R via
#' `input$<ns>school_search`; use `useSchoolSearch()` in your server function
#' to wire up the response.
#'
#' Trigger and popover labels are configurable so the widget can be reused
#' for any server-side typeahead scenario (provider search, facility
#' lookup, etc.).
#'
#' @param inputId Character. The Shiny input ID. The value is a named list
#'   with `id`, `name`, `district`, `city`, `state`, `type`, or `NULL`.
#' @param value Optional initial value — a list with the school fields above.
#' @param grade_label Optional display label (e.g. `"3rd grade"`) for
#'   accessible naming.
#' @param grade_key The grade's two-char key (e.g. `"PK"`, `"03"`). Used by
#'   the fill-down action to tell the server which grade triggered it.
#' @param show_nces_id Logical. Whether to show the NCES ID in the filled
#'   state and search results. Default `TRUE`.
#' @param trigger_label Character. Text on the empty-state trigger button.
#'   Default `"+ Pick school"`.
#' @param popover_title Character. Popover header text.
#'   Default `"Find school"`.
#' @param search_placeholder Character. Placeholder in the search input.
#'   Default `"Search by school name, city, district\u2026"`.
#' @param empty_hint Character. Hint text shown before the user types.
#'   Default `"Type 2+ characters to search 100k+ US schools"`.
#' @param no_match_hint Character. Text shown when search returns no results.
#'   Default `"No schools match. Check spelling \u2014 picking is required."`.
#' @param show_fill_down Logical. Whether to show the "fill down" action
#'   link in the filled state. Default `TRUE`.
#' @param ns Character. The Shiny module namespace prefix for search
#'   communication. Pass `session$ns("")` from within a module. For
#'   top-level apps, leave as `""`.
#' @param width CSS width. Default `"100%"`.
#'
#' @return An [htmltools::tagList].
#'
#' @examples
#' \dontrun{
#' # Default (backward compatible)
#' schoolPickerInput("school_PK", grade_label = "PreK", grade_key = "PK")
#'
#' # Custom labels for provider search
#' schoolPickerInput("provider_PK",
#'   trigger_label = "+ Find provider",
#'   popover_title = "Search providers",
#'   search_placeholder = "Type provider name or specialty\u2026",
#'   empty_hint = "Type 2+ characters to search providers",
#'   no_match_hint = "No providers found.",
#'   show_nces_id = FALSE,
#'   show_fill_down = FALSE,
#'   grade_label = "PreK",
#'   grade_key = "PK"
#' )
#' }
#'
#' @export
#' @importFrom htmltools tagList tags
#' @importFrom jsonlite toJSON
schoolPickerInput <- function(
  inputId,
  value = NULL,
  grade_label = NULL,
  grade_key = NULL,
  show_nces_id = TRUE,
  trigger_label = "+ Pick school",
  popover_title = "Find school",
  search_placeholder = "Search by school name, city, district\u2026",
  empty_hint = "Type 2+ characters to search 100k+ US schools",
  no_match_hint = "No schools match. Check spelling \u2014 picking is required.",
  show_fill_down = TRUE,
  ns = "",
  width = "100%"
) {
  val_json <- if (!is.null(value)) {
    toJSON(value, auto_unbox = TRUE, null = "null")
  } else {
    "null"
  }

  tag <- tags$div(
    id = inputId,
    class = "sh-school-picker pv-container",
    style = if (!is.null(width)) paste0("width:", width),
    `data-initial-value` = val_json,
    `data-grade-label` = grade_label,
    `data-grade-key` = grade_key,
    `data-show-nces-id` = tolower(as.character(show_nces_id)),
    `data-trigger-label` = trigger_label,
    `data-popover-title` = popover_title,
    `data-search-placeholder` = search_placeholder,
    `data-empty-hint` = empty_hint,
    `data-no-match-hint` = no_match_hint,
    `data-show-fill-down` = tolower(as.character(show_fill_down)),
    `data-ns` = ns
  )

  htmltools::attachDependencies(tag, list(popoverDep(), school_picker_dep()))
}


#' Update a school picker from the server
#'
#' @param session The Shiny session object.
#' @param inputId Character. The input ID to update.
#' @param value New school value (named list) or `NULL` to clear.
#' @param show_nces_id Logical. Update the NCES ID display toggle.
#'
#' @return Called for its side effect; returns invisibly.
#' @export
#' @importFrom shiny getDefaultReactiveDomain
updateSchoolPickerInput <- function(
  session = getDefaultReactiveDomain(),
  inputId,
  value = NULL,
  show_nces_id = NULL
) {
  msg <- list()
  if (!missing(value)) {
    msg$value <- value
  }
  if (!is.null(show_nces_id)) {
    msg$showNcesId <- show_nces_id
  }
  session$sendInputMessage(inputId, msg)
}


#' Wire up server-side school search
#'
#' Call this once in your Shiny server function (or module server). It
#' observes the shared `school_search` input sent by all school pickers
#' and routes results back to the correct widget.
#'
#' @param input The Shiny `input` object.
#' @param session The Shiny `session` object.
#' @param search_fn A function that takes `(query, limit)` and returns a
#'   data frame of matching schools.
#' @param limit Maximum number of results to return. Default 25.
#'
#' @return Invisible `NULL`. Called for its side effect.
#' @export
useSchoolSearch <- function(input, session, search_fn, limit = 25L) {
  shiny::observeEvent(
    input$school_search,
    {
      req <- input$school_search
      query <- req$query
      request_id <- req$request_id
      source_id <- req$source_id

      if (is.null(query) || nchar(query) < 2) {
        return()
      }

      results <- tryCatch(
        search_fn(query, limit),
        error = function(e) {
          message("[reactablePlus] search error: ", conditionMessage(e))
          data.frame()
        }
      )

      results_list <- if (nrow(results) > 0) {
        lapply(seq_len(nrow(results)), function(i) {
          as.list(results[i, , drop = FALSE])
        })
      } else {
        list()
      }

      session$sendCustomMessage(
        "sh-search-results",
        list(
          results = results_list,
          request_id = request_id,
          source_id = source_id
        )
      )
    },
    ignoreInit = TRUE
  )

  invisible(NULL)
}
