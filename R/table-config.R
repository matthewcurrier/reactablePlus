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
#' and how cross-column interactions (mutual exclusion, fill-down,
#' gating) work.
#'
#' @param row_keys Character vector of row identifiers (e.g. grade keys).
#' @param row_labels Character vector of display labels (same length as
#'   `row_keys`).
#' @param columns List of column specs built with `widget_col()`.
#' @param selectable Logical. If `TRUE`, a checkbox column is prepended
#'   and row selection is tracked. Required when any column has a
#'   `gate` condition with `type = "selected"`. Default `FALSE`.
#' @param show_reset Logical. If `TRUE`, a Reset button is shown in
#'   the toolbar that clears all inputs and deselects all rows.
#'   Default `FALSE`.
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
#' @param badge_col Character or NULL. When non-NULL, a static label
#'   column is shown using `row_labels`. Default `NULL` (no badge).
#' @param badge_label Character. Column header for the badge column.
#'   Default `"Label"`. Ignored when `badge_col` is `NULL`.
#' @param badge_render_fn Function `(row_key, row_label)` -> HTML
#'   string for the badge cell. Default `NULL` renders
#'   `<span>row_label</span>`. Supply a custom function for
#'   domain-specific styling (e.g. grade badges with CSS classes).
#' @param row_class_fn Function `(row_key, row_state)` -> CSS class
#'   string or `NULL`. Applied per row via reactable's `rowClass`.
#'   Default `NULL` (no row classes).
#' @param year_col Character or NULL. When non-NULL, an editable year
#'   spinner column is shown. Default `NULL` (no year column).
#' @param year_range Integer vector of length 2, e.g. `c(1990, 2050)`.
#'
#' @details
#' ## Interactions
#'
#' `interactions$mutual_exclusion`: a list of rules, each a list with
#' `when_on` (column ID), `clears` (column ID), and `display` (HTML
#' string to show in the cleared column's cell).
#'
#' `interactions$fill_down`: a list with `column` (column ID),
#' `range_check` (logical), and optionally `input_name` (character —
#' the Shiny input name the widget JS sends; defaults to
#' `paste0(column, "_fill_down")`).
#'
#' ## Gating
#'
#' Columns with `gate` conditions (set via `widget_col(gate = ...)`)
#' are automatically disabled until all conditions are met. Controller
#' columns referenced in `gate` conditions are automatically marked
#' `triggers_rerender = TRUE` so the table updates when they change.
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
#'   ),
#'   badge_col = "grade",
#'   badge_label = "Grade",
#'   badge_render_fn = function(row_key, row_label) {
#'     css_class <- paste0("grade-badge g-", row_key)
#'     sprintf('<span class="%s">%s</span>', css_class, row_label)
#'   }
#' )
#' }
#'
#' @export
table_config <- function(row_keys,
                         row_labels,
                         columns,
                         selectable      = FALSE,
                         show_reset      = FALSE,
                         gear_toggles    = NULL,
                         interactions    = list(),
                         from_saved_fn   = NULL,
                         to_output_fn    = NULL,
                         toolbar_stats_fn = NULL,
                         search_fn_col   = NULL,
                         badge_col       = NULL,
                         badge_label     = "Label",
                         badge_render_fn = NULL,
                         row_class_fn    = NULL,
                         year_col        = NULL,
                         year_range      = c(1990L, 2050L)) {

  stopifnot(
    is.character(row_keys),
    is.character(row_labels),
    length(row_keys) == length(row_labels),
    is.list(columns)
  )

  label_map <- stats::setNames(as.list(row_labels), row_keys)

  # ── Gate wiring ──────────────────────────────────────────────────────────
  # Collect all col_ids referenced as gate controllers and auto-set
  # triggers_rerender on them. Also validate references and check that
  # "selected" gates have selectable = TRUE.

  all_col_ids <- purrr::map_chr(columns, "id")

  controller_ids <- character(0)
  purrr::walk(columns, function(cs) {
    if (is.null(cs$gate)) return()
    purrr::walk(cs$gate, function(cond) {
      if (cond$type == "value") {
        if (!cond$col_id %in% all_col_ids) {
          stop(
            "widget_col '", cs$id, "' gate references col_id '",
            cond$col_id, "' which does not exist in columns.",
            call. = FALSE
          )
        }
        controller_ids <<- unique(c(controller_ids, cond$col_id))
      }
      if (cond$type == "selected" && !isTRUE(selectable)) {
        stop(
          "widget_col '", cs$id, "' gate uses type 'selected' but ",
          "selectable is not TRUE in table_config().",
          call. = FALSE
        )
      }
    })
  })

  # Auto-mark controller columns as triggers_rerender
  columns <- purrr::map(columns, function(cs) {
    if (cs$id %in% controller_ids && !isTRUE(cs$triggers_rerender)) {
      cs$triggers_rerender <- TRUE
    }
    cs
  })

  structure(
    list(
      row_keys         = row_keys,
      row_labels       = row_labels,
      label_map        = label_map,
      columns          = columns,
      selectable       = selectable,
      show_reset       = show_reset,
      gear_toggles     = gear_toggles,
      interactions     = interactions,
      from_saved_fn    = from_saved_fn,
      to_output_fn     = to_output_fn,
      toolbar_stats_fn = toolbar_stats_fn,
      search_fn_col    = search_fn_col,
      badge_col        = badge_col,
      badge_label      = badge_label,
      badge_render_fn  = badge_render_fn,
      row_class_fn     = row_class_fn,
      year_col         = year_col,
      year_range       = year_range
    ),
    class = "table_config"
  )
}


#' Build a widget column specification
#'
#' Defines a single column in a `table_config()`. Each column maps to
#' either a picker widget or a primitive inline input and carries
#' type-specific options.
#'
#' @param id Character. Column ID — becomes the key in row state and
#'   the prefix for Shiny input IDs (e.g. `"school"` -> `input$school_PK`).
#' @param type Character. One of:
#'
#'   **Picker widgets** (complex popover-based inputs):
#'   `"school_picker"`, `"attendance_picker"`, `"homeschool_picker"`,
#'   `"notes_input"`.
#'
#'   **Primitive inputs** (inline HTML form controls):
#'   `"dropdown"`, `"numeric"`, `"date"`, `"checkbox"`, `"toggle"`,
#'   `"text"`.
#'
#'   **Extensible**: `"custom"` (requires `render_cell_fn`).
#'
#' @param label Character. Column header text.
#' @param width Integer or NULL. Fixed column width in pixels.
#' @param min_width Integer or NULL. Minimum column width.
#' @param options Named list of type-specific options:
#'
#'   **Picker widgets**: passed to the widget constructor (e.g.
#'   `list(show_nces_id = TRUE)` for school picker,
#'   `list(sections = ...)` for attendance picker).
#'
#'   **dropdown**: `choices` (required — character vector, named
#'   character vector, or list of `list(label, value)`; see
#'   [normalize_choices()]), `placeholder` (default `"-- Select --"`).
#'
#'   **numeric**: `min` (required), `max`, `step`.
#'
#'   **date**: `min_date`, `max_date` (YYYY-MM-DD strings).
#'
#'   **text**: `max_chars`, `placeholder`.
#'
#'   **checkbox** / **toggle**: no extra options.
#'
#' @param gate A list of conditions that must ALL pass for this column's
#'   input to be enabled. When any condition fails, the input renders
#'   locked (disabled + dimmed) and its value is forced to `empty_value`
#'   in `get_data()`. Two condition types are supported:
#'
#'   `list(type = "value", col_id = "status", values = c("active"))` —
#'   the column whose `id` matches `col_id` must have a value in
#'   `values` for this row.
#'
#'   `list(type = "selected")` — the row must be selected (requires
#'   `selectable = TRUE` in `table_config()`).
#'
#'   Default `NULL` (no gating — always enabled).
#'
#' @param gear_toggle Character or NULL. The gear toggle key that
#'   controls this column's visibility. When the toggle is off, the
#'   column is hidden (set to `show = FALSE`).
#' @param triggers_rerender Logical. If TRUE, changes to this column
#'   bump the render key (forcing a full table re-render). Needed for
#'   columns involved in mutual exclusion. Automatically set to TRUE
#'   by `table_config()` for columns that act as gate controllers.
#'   Default `FALSE`.
#' @param empty_value The default value for an empty row. When `NULL`
#'   (the default), a type-appropriate blank is used: `NA` for
#'   dropdown/date, `options$min` (or `0`) for numeric, `FALSE` for
#'   checkbox/toggle, `""` for text/notes_input, `NULL` for pickers.
#' @param render_cell_fn Function `(ns, row_key, row_state, col_spec,
#'   settings)` -> HTML string. Only needed for `type = "custom"`.
#' @param validate_fn Function `(value)` -> validated value. Controls
#'   how raw input is cleaned before storage. Default `NULL` uses
#'   type-appropriate validation.
#'
#' @return A `widget_col` list (S3 class `"widget_col"`).
#'
#' @examples
#' # Primitive dropdown
#' widget_col("status", "dropdown", "Status",
#'   options = list(choices = c("Active", "Inactive", "Pending"))
#' )
#'
#' # Numeric gated by a dropdown value
#' widget_col("score", "numeric", "Score",
#'   options = list(min = 0, max = 100),
#'   gate = list(
#'     list(type = "value", col_id = "status", values = c("active"))
#'   )
#' )
#'
#' # Picker widget
#' widget_col("school", "school_picker", "School", min_width = 300)
#'
#' @importFrom rlang `%||%`
#' @export
widget_col <- function(id,
                       type,
                       label,
                       width        = NULL,
                       min_width    = NULL,
                       options      = list(),
                       gate         = NULL,
                       gear_toggle  = NULL,
                       triggers_rerender = FALSE,
                       empty_value  = NULL,
                       render_cell_fn = NULL,
                       validate_fn    = NULL) {

  stopifnot(is.character(id), length(id) == 1L)
  stopifnot(is.character(type), length(type) == 1L)

  valid_types <- c(
    "school_picker", "attendance_picker", "homeschool_picker",
    "notes_input", "custom",
    "dropdown", "numeric", "date", "checkbox", "toggle", "text"
  )
  if (!type %in% valid_types) {
    stop(
      "widget_col type '", type, "' is not supported. ",
      "Must be one of: ", paste(valid_types, collapse = ", "), ".",
      call. = FALSE
    )
  }

  # ── Gate validation ──────────────────────────────────────────────────────

  if (!is.null(gate)) {
    if (!is.list(gate) || length(gate) == 0L) {
      stop("gate must be a non-empty list of conditions.", call. = FALSE)
    }
    purrr::iwalk(gate, function(cond, j) {
      if (is.null(cond$type) || !cond$type %in% c("value", "selected")) {
        stop(
          "gate[[", j, "]]$type must be 'value' or 'selected'.",
          call. = FALSE
        )
      }
      if (cond$type == "value") {
        if (is.null(cond$col_id) || !is.character(cond$col_id)) {
          stop(
            "gate[[", j, "]] (type 'value') requires a character col_id.",
            call. = FALSE
          )
        }
        if (is.null(cond$values) || length(cond$values) == 0L) {
          stop(
            "gate[[", j, "]] (type 'value') requires a non-empty values field.",
            call. = FALSE
          )
        }
      }
    })
  }

  # ── Primitive-type validation & normalization ────────────────────────────

  if (type == "dropdown") {
    if (is.null(options$choices)) {
      stop("options$choices is required for type = 'dropdown'.", call. = FALSE)
    }
    options$choices <- normalize_choices(options$choices)
  }

  if (type == "numeric") {
    if (is.null(options$min)) {
      stop("options$min is required for type = 'numeric'.", call. = FALSE)
    }
    if (!is.numeric(options$min) || length(options$min) != 1L) {
      stop("options$min must be a single numeric value.", call. = FALSE)
    }
  }

  # ── Infer empty_value when not provided ──────────────────────────────────

  if (is.null(empty_value)) {
    empty_value <- switch(
      type,
      dropdown  = NA_character_,
      numeric   = options$min %||% 0,
      date      = NA_character_,
      checkbox  = FALSE,
      toggle    = FALSE,
      text      = "",
      notes_input = "",
      NULL # pickers and custom default to NULL
    )
  }

  structure(
    list(
      id                = id,
      type              = type,
      label             = label,
      width             = width,
      min_width         = min_width,
      options           = options,
      gate              = gate,
      gear_toggle       = gear_toggle,
      triggers_rerender = triggers_rerender,
      empty_value       = empty_value,
      render_cell_fn    = render_cell_fn,
      validate_fn       = validate_fn
    ),
    class = "widget_col"
  )
}
