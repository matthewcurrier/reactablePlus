# =============================================================================
# table-config.R
#
# Declarative configuration for config_table_ui / config_table_server.
# Build a table_config with table_config(), adding columns with widget_col().
#
# The config tells the config-driven module:
#   - what rows exist (keys + labels)
#   - what columns to render and which widget each one uses
#   - what gear toggles to offer
#   - how cross-column interactions work (mutual exclusion, fill-down)
#   - how to marshal data in/out (from saved DB rows -> state, state -> output)
# =============================================================================


#' Build a table configuration
#'
#' Creates a declarative configuration object that drives
#' `config_table_ui()` / `config_table_server()`. The config describes
#' which rows and columns appear, which picker widget each column uses,
#' and how cross-column interactions (mutual exclusion, fill-down) work.
#'
#' @param row_keys Character vector of row identifiers (e.g. grade keys).
#' @param row_labels Character vector of display labels (same length as
#'   `row_keys`).
#' @param columns List of column specs built with `widget_col()`.
#' @param gear_toggles Named list of toggle definitions for
#'   `gearPopoverInput()`. Pass `NULL` for no gear icon.
#' @param interactions List with optional `mutual_exclusion` and
#'   `fill_down` entries. See Details.
#' @param from_saved_fn Function `(db_row, col_specs)` -> named list.
#'   Converts one row of saved data into a row-state list keyed by
#'   column ID. Called once per row when `data_r()` provides a data frame.
#' @param to_output_fn Function `(row_state, row_key)` -> single-row
#'   data.frame. Converts one row of state back to the output schema.
#' @param toolbar_stats_fn Function `(rows, row_keys)` -> [shiny::HTML]
#'   string for the toolbar. Pass `NULL` for no toolbar.
#' @param search_fn_col Character. Column ID that requires server-side
#'   search (i.e. uses `useSchoolSearch()`). Pass `NULL` if no column
#'   needs search. Default `NULL`.
#' @param badge_col Character. Column ID rendered as a static badge
#'   from the row key/label. Default `"grade"`. Pass `NULL` to omit.
#' @param year_col Character. Column ID for the editable year spinner.
#'   Default `"school_year"`. Pass `NULL` to omit.
#' @param year_range Integer vector of length 2, e.g. `c(1990, 2050)`.
#'
#' @details
#' ## Interactions
#'
#' `interactions$mutual_exclusion`: a list of rules, each a list with
#' `when_on` (column ID), `clears` (column ID), and `display` (HTML
#' string to show in the cleared column's cell).
#'
#' `interactions$fill_down`: a list with `column` (column ID) and
#' `range_check` (logical — if TRUE, uses `gradeInRange()` to limit).
#'
#' @return A `table_config` list (S3 class `"table_config"`).
#'
#' @examples
#' \dontrun{
#' cfg <- table_config(
#'   row_keys   = c("PK", "K", "01"),
#'   row_labels = c("PreK", "K", "1st"),
#'   columns    = list(
#'     widget_col("school", "school_picker", "School", min_width = 300),
#'     widget_col("attendance", "attendance_picker", "Attendance", width = 200)
#'   )
#' )
#' }
#'
#' @export
table_config <- function(row_keys,
                         row_labels,
                         columns,
                         gear_toggles    = NULL,
                         interactions    = list(),
                         from_saved_fn   = NULL,
                         to_output_fn    = NULL,
                         toolbar_stats_fn = NULL,
                         search_fn_col   = NULL,
                         badge_col       = "grade",
                         year_col        = "school_year",
                         year_range      = c(1990L, 2050L)) {

  stopifnot(
    is.character(row_keys),
    is.character(row_labels),
    length(row_keys) == length(row_labels),
    is.list(columns)
  )

  label_map <- stats::setNames(as.list(row_labels), row_keys)

  structure(
    list(
      row_keys         = row_keys,
      row_labels       = row_labels,
      label_map        = label_map,
      columns          = columns,
      gear_toggles     = gear_toggles,
      interactions     = interactions,
      from_saved_fn    = from_saved_fn,
      to_output_fn     = to_output_fn,
      toolbar_stats_fn = toolbar_stats_fn,
      search_fn_col    = search_fn_col,
      badge_col        = badge_col,
      year_col         = year_col,
      year_range       = year_range
    ),
    class = "table_config"
  )
}


#' Build a widget column specification
#'
#' Defines a single column in a `table_config()`. Each widget column
#' maps to a picker widget type (`school_picker`, `attendance_picker`,
#' `homeschool_picker`, `notes_input`, or `custom`) and carries
#' widget-specific options.
#'
#' @param id Character. Column ID — becomes the key in row state and
#'   the prefix for Shiny input IDs (e.g. `"school"` -> `input$school_PK`).
#' @param type Character. Widget type: `"school_picker"`,
#'   `"attendance_picker"`, `"homeschool_picker"`, `"notes_input"`,
#'   or `"custom"`.
#' @param label Character. Column header text.
#' @param width Integer or NULL. Fixed column width in pixels.
#' @param min_width Integer or NULL. Minimum column width.
#' @param options Named list of widget-specific options passed to the
#'   widget constructor (e.g. `list(show_nces_id = TRUE)` for school
#'   picker, `list(sections = ...)` for attendance picker).
#' @param gear_toggle Character or NULL. The gear toggle key that
#'   controls this column's visibility. When the toggle is off, the
#'   column is hidden (set to `show = FALSE`).
#' @param triggers_rerender Logical. If TRUE, changes to this column
#'   bump the render key (forcing a full table re-render). Needed for
#'   columns involved in mutual exclusion. Default `FALSE`.
#' @param empty_value The default value for an empty row. Default `NULL`.
#' @param render_cell_fn Function `(ns, row_key, row_state, col_spec,
#'   settings)` -> HTML string. Only needed for `type = "custom"`.
#' @param validate_fn Function `(value)` -> validated value. Controls
#'   how raw input is cleaned before storage. Default `NULL` uses
#'   type-appropriate validation.
#'
#' @return A `widget_col` list (S3 class `"widget_col"`).
#' @export
widget_col <- function(id,
                       type,
                       label,
                       width        = NULL,
                       min_width    = NULL,
                       options      = list(),
                       gear_toggle  = NULL,
                       triggers_rerender = FALSE,
                       empty_value  = NULL,
                       render_cell_fn = NULL,
                       validate_fn    = NULL) {

  stopifnot(is.character(id), length(id) == 1L)
  stopifnot(is.character(type), length(type) == 1L)

  structure(
    list(
      id                = id,
      type              = type,
      label             = label,
      width             = width,
      min_width         = min_width,
      options           = options,
      gear_toggle       = gear_toggle,
      triggers_rerender = triggers_rerender,
      empty_value       = empty_value,
      render_cell_fn    = render_cell_fn,
      validate_fn       = validate_fn
    ),
    class = "widget_col"
  )
}
