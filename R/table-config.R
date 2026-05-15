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
#' Two modes are supported:
#'
#' **Static mode** (default): rows are fixed at config time via
#' `row_keys` / `row_labels`. Use when the set of editable rows is
#' known up front (e.g. a grade roster).
#'
#' **Dynamic mode**: rows are derived at runtime from a reactive
#' `source_data` data frame passed to [config_table_server()]. Enable
#' by supplying `row_id_col` and either `row_label_col` or
#' `row_label_fn`. In this mode `row_keys` / `row_labels` are
#' optional (default to `character(0)`).
#'
#' @param row_keys Character vector of row identifiers (e.g. grade
#'   keys). Required in static mode. In dynamic mode, defaults to
#'   `character(0)` — rows are derived from `source_data` at runtime.
#' @param row_labels Character vector of display labels (same length as
#'   `row_keys`). Required in static mode. In dynamic mode, defaults
#'   to `character(0)` — labels are derived via `row_label_col` or
#'   `row_label_fn`.
#' @param columns List of column specs built with `widget_col()`.
#' @param row_id_col `character(1)` or `NULL`. The name of the column
#'   in `source_data` that uniquely identifies each row. When non-NULL
#'   the config operates in dynamic mode. Default `NULL` (static mode).
#' @param row_label_col `character(1)` or `NULL`. The name of the
#'   column in `source_data` to use as the display label. Ignored when
#'   `row_id_col` is `NULL`. Exactly one of `row_label_col` or
#'   `row_label_fn` must be supplied in dynamic mode.
#' @param row_label_fn `function` or `NULL`. A function
#'   `(source_data_row)` -> `character(1)` that computes a display
#'   label from a single row (a one-row data frame) of `source_data`.
#'   Ignored when `row_id_col` is `NULL`. Exactly one of
#'   `row_label_col` or `row_label_fn` must be supplied in dynamic
#'   mode.
#' @param display_cols List of [display_col()] specs, or `NULL`.
#'   Read-only columns whose values are drawn from `source_data` in
#'   dynamic mode. Displayed between the badge column and the editable
#'   widget columns. Requires dynamic mode (`row_id_col` must be set).
#'   Default `NULL` (no display columns).
#' @param selectable Logical. If `TRUE`, a checkbox column is prepended
#'   and row selection is tracked. Required when any column has a
#'   `gate` condition with `type = "selected"`. Default `FALSE`.
#' @param click_to_select Logical. If `TRUE`, clicking on the badge
#'   column or any display column toggles the row's selection
#'   checkbox. Requires `selectable = TRUE`. Widget columns are
#'   excluded — they have their own click interactions. Default
#'   `FALSE`.
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
#'   search (i.e. uses [useTypeaheadSearch()]). Pass `NULL` if no column
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
#' `range_check_fn` (optional function `(row_key, value) -> logical`
#' that returns `TRUE` when the row is in range for fill-down), and
#' optionally `input_name` (character — the Shiny input name the
#' widget JS sends; defaults to `paste0(column, "_fill_down")`).
#'
#' ## Gating
#'
#' Columns with `gate` conditions (set via `widget_col(gate = ...)`)
#' are automatically disabled until all conditions are met. Controller
#' columns referenced in `gate` conditions are automatically marked
#' `triggers_rerender = TRUE` so the table updates when they change.
#'
#' ## Dynamic mode
#'
#' When `row_id_col` is non-NULL, the config is in dynamic mode.
#' Rows are not fixed at config time — they are derived from the
#' `source_data` reactive at runtime. Each time `source_data`
#' changes, the module merges the new row set with existing state:
#' user-entered values for rows that survive are preserved, new rows
#' receive `empty_value` defaults, and departed rows' state is
#' retained internally so it restores if those rows reappear.
#'
#' @return A `table_config` list (S3 class `"table_config"`).
#'
#' @examples
#' \dontrun{
#' # Static mode — rows fixed at config time
#' cfg <- table_config(
#'   row_keys   = c("PK", "K", "01"),
#'   row_labels = c("PreK", "K", "1st"),
#'   columns    = list(
#'     widget_col("school", "search_picker", "School", min_width = 300),
#'     widget_col("attendance", "attendance_picker", "Attendance", width = 200)
#'   ),
#'   badge_col = "grade",
#'   badge_label = "Grade",
#'   badge_render_fn = function(row_key, row_label) {
#'     css_class <- paste0("grade-badge g-", row_key)
#'     sprintf('<span class="%s">%s</span>', css_class, row_label)
#'   }
#' )
#'
#' # Dynamic mode — rows derived from source_data at runtime
#' cfg_dyn <- table_config(
#'   row_id_col    = "student_id",
#'   row_label_col = "student_name",
#'   columns = list(
#'     widget_col("status", "dropdown", "Status",
#'       options = list(choices = c("Active", "Inactive"))
#'     ),
#'     widget_col("score", "numeric", "Score",
#'       options = list(min = 0, max = 100)
#'     )
#'   ),
#'   selectable = TRUE,
#'   to_output_fn = function(row_state, row_key) {
#'     data.frame(
#'       id     = row_key,
#'       status = row_state$status %||% NA_character_,
#'       score  = row_state$score %||% NA_real_,
#'       stringsAsFactors = FALSE
#'     )
#'   }
#' )
#' }
#'
#' @importFrom rlang `%||%`
#' @export
table_config <- function(
  row_keys = NULL,
  row_labels = NULL,
  columns,
  row_id_col = NULL,
  row_label_col = NULL,
  row_label_fn = NULL,
  display_cols = NULL,
  selectable = FALSE,
  click_to_select = FALSE,
  show_reset = FALSE,
  gear_toggles = NULL,
  interactions = list(),
  from_saved_fn = NULL,
  to_output_fn = NULL,
  toolbar_stats_fn = NULL,
  search_fn_col = NULL,
  badge_col = NULL,
  badge_label = "Label",
  badge_render_fn = NULL,
  row_class_fn = NULL,
  year_col = NULL,
  year_range = c(1990L, 2050L)
) {
  stopifnot(is.list(columns))

  dynamic <- !is.null(row_id_col)

  # ── Mode validation ──────────────────────────────────────────────────────
  if (dynamic) {
    stopifnot(
      is.character(row_id_col),
      length(row_id_col) == 1L,
      nzchar(row_id_col)
    )
    if (is.null(row_label_col) && is.null(row_label_fn)) {
      stop(
        "Dynamic mode (row_id_col is set) requires either ",
        "row_label_col or row_label_fn.",
        call. = FALSE
      )
    }
    if (!is.null(row_label_col) && !is.null(row_label_fn)) {
      stop(
        "Supply row_label_col or row_label_fn, not both.",
        call. = FALSE
      )
    }
    if (!is.null(row_label_col)) {
      stopifnot(
        is.character(row_label_col),
        length(row_label_col) == 1L,
        nzchar(row_label_col)
      )
    }
    if (!is.null(row_label_fn)) {
      stopifnot(is.function(row_label_fn))
    }
    # Default row_keys / row_labels to empty in dynamic mode
    row_keys <- row_keys %||% character(0)
    row_labels <- row_labels %||% character(0)
  } else {
    # Static mode — row_keys and row_labels are required
    if (is.null(row_keys) || is.null(row_labels)) {
      stop(
        "row_keys and row_labels are required in static mode ",
        "(when row_id_col is NULL).",
        call. = FALSE
      )
    }
  }

  stopifnot(
    is.character(row_keys),
    is.character(row_labels),
    length(row_keys) == length(row_labels)
  )

  # ── Display column validation ────────────────────────────────────────────
  if (!is.null(display_cols)) {
    if (!dynamic) {
      stop(
        "display_cols requires dynamic mode (row_id_col must be set).",
        call. = FALSE
      )
    }
    if (!is.list(display_cols) || length(display_cols) == 0L) {
      stop("display_cols must be a non-empty list of display_col() specs.",
        call. = FALSE
      )
    }
    purrr::walk(display_cols, function(dc) {
      if (!inherits(dc, "display_col")) {
        stop(
          "Each element of display_cols must be created with display_col().",
          call. = FALSE
        )
      }
    })
    # Check for ID collisions with widget columns
    widget_ids <- purrr::map_chr(columns, "id")
    display_ids <- purrr::map_chr(display_cols, "id")
    overlap <- intersect(widget_ids, display_ids)
    if (length(overlap) > 0L) {
      stop(
        "display_col IDs must not collide with widget_col IDs. ",
        "Overlap: ", paste(overlap, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # ── click_to_select validation ───────────────────────────────────────────
  if (isTRUE(click_to_select) && !isTRUE(selectable)) {
    stop(
      "click_to_select requires selectable = TRUE.",
      call. = FALSE
    )
  }

  label_map <- stats::setNames(as.list(row_labels), row_keys)

  # ── Gate wiring ──────────────────────────────────────────────────────────
  # Collect all col_ids referenced as gate controllers and auto-set
  # triggers_rerender on them. Also validate references and check that
  # "selected" gates have selectable = TRUE.

  all_col_ids <- purrr::map_chr(columns, "id")

  controller_ids <- character(0)
  purrr::walk(columns, function(cs) {
    if (is.null(cs$gate)) {
      return()
    }
    purrr::walk(cs$gate, function(cond) {
      if (cond$type == "value") {
        if (!cond$col_id %in% all_col_ids) {
          stop(
            "widget_col '",
            cs$id,
            "' gate references col_id '",
            cond$col_id,
            "' which does not exist in columns.",
            call. = FALSE
          )
        }
        controller_ids <<- unique(c(controller_ids, cond$col_id))
      }
      if (cond$type == "selected" && !isTRUE(selectable)) {
        stop(
          "widget_col '",
          cs$id,
          "' gate uses type 'selected' but ",
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
      row_keys = row_keys,
      row_labels = row_labels,
      label_map = label_map,
      dynamic = dynamic,
      row_id_col = row_id_col,
      row_label_col = row_label_col,
      row_label_fn = row_label_fn,
      display_cols = display_cols,
      columns = columns,
      selectable = selectable,
      click_to_select = click_to_select,
      show_reset = show_reset,
      gear_toggles = gear_toggles,
      interactions = interactions,
      from_saved_fn = from_saved_fn,
      to_output_fn = to_output_fn,
      toolbar_stats_fn = toolbar_stats_fn,
      search_fn_col = search_fn_col,
      badge_col = badge_col,
      badge_label = badge_label,
      badge_render_fn = badge_render_fn,
      row_class_fn = row_class_fn,
      year_col = year_col,
      year_range = year_range
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
#'   `"search_picker"`, `"attendance_picker"`, `"homeschool_picker"`,
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
#'   `list(show_nces_id = TRUE)` for search picker,
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
#' widget_col("school", "search_picker", "School", min_width = 300)
#'
#' @importFrom rlang `%||%`
#' @export
widget_col <- function(
  id,
  type,
  label,
  width = NULL,
  min_width = NULL,
  options = list(),
  gate = NULL,
  gear_toggle = NULL,
  triggers_rerender = FALSE,
  empty_value = NULL,
  render_cell_fn = NULL,
  validate_fn = NULL
) {
  stopifnot(is.character(id), length(id) == 1L)
  stopifnot(is.character(type), length(type) == 1L)

  valid_types <- c(
    "search_picker",
    "attendance_picker",
    "homeschool_picker",
    "notes_input",
    "custom",
    "dropdown",
    "numeric",
    "date",
    "checkbox",
    "toggle",
    "text"
  )
  if (!type %in% valid_types) {
    stop(
      "widget_col type '",
      type,
      "' is not supported. ",
      "Must be one of: ",
      paste(valid_types, collapse = ", "),
      ".",
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
          "gate[[",
          j,
          "]]$type must be 'value' or 'selected'.",
          call. = FALSE
        )
      }
      if (cond$type == "value") {
        if (is.null(cond$col_id) || !is.character(cond$col_id)) {
          stop(
            "gate[[",
            j,
            "]] (type 'value') requires a character col_id.",
            call. = FALSE
          )
        }
        if (is.null(cond$values) || length(cond$values) == 0L) {
          stop(
            "gate[[",
            j,
            "]] (type 'value') requires a non-empty values field.",
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
      dropdown = NA_character_,
      numeric = options$min %||% 0,
      date = NA_character_,
      checkbox = FALSE,
      toggle = FALSE,
      text = "",
      notes_input = "",
      NULL # pickers and custom default to NULL
    )
  }

  structure(
    list(
      id = id,
      type = type,
      label = label,
      width = width,
      min_width = min_width,
      options = options,
      gate = gate,
      gear_toggle = gear_toggle,
      triggers_rerender = triggers_rerender,
      empty_value = empty_value,
      render_cell_fn = render_cell_fn,
      validate_fn = validate_fn
    ),
    class = "widget_col"
  )
}


# ── Display column specification ─────────────────────────────────────────────

#' Build a read-only display column specification
#'
#' Defines a non-editable column whose values come from
#' `source_data` in dynamic mode. Display columns provide context
#' alongside editable `widget_col()` columns — e.g. an email address,
#' department name, or enrollment date that the user can see but not
#' edit.
#'
#' @param id `character(1)`. The name of the column in `source_data`
#'   to display. Also used as the column key in the reactable data
#'   frame.
#' @param label `character(1)`. The column header text.
#' @param width Integer or `NULL`. Fixed column width in pixels.
#' @param min_width Integer or `NULL`. Minimum column width.
#' @param render_fn Function or `NULL`. An optional custom cell
#'   renderer `(value, row_key)` -> HTML string. When `NULL`
#'   (the default), values are shown as plain escaped text.
#'
#' @return A `display_col` list (S3 class `"display_col"`).
#'
#' @examples
#' display_col("email", "Email", width = 200)
#' display_col("department", "Dept", min_width = 120)
#' display_col("status", "Status", render_fn = function(value, row_key) {
#'   cls <- if (identical(value, "active")) "badge-success" else "badge-muted"
#'   sprintf('<span class="%s">%s</span>', cls, value)
#' })
#'
#' @export
display_col <- function(
  id,
  label,
  width = NULL,
  min_width = NULL,
  render_fn = NULL
) {
  stopifnot(
    is.character(id), length(id) == 1L, nzchar(id),
    is.character(label), length(label) == 1L
  )
  if (!is.null(render_fn)) {
    stopifnot(is.function(render_fn))
  }

  structure(
    list(
      id = id,
      label = label,
      width = width,
      min_width = min_width,
      render_fn = render_fn
    ),
    class = "display_col"
  )
}


# ── Choice normalization ─────────────────────────────────────────────────────

#' Normalize dropdown choices to list-of-lists format
#'
#' Converts shorthand choice formats into the canonical list-of-lists
#' structure expected by [widget_col()] and the cell renderers.
#' Called automatically during `widget_col()` validation for
#' `type = "dropdown"`, but can also be used directly.
#'
#' Three formats are accepted:
#' \describe{
#'   \item{Unnamed character vector}{`c("High", "Medium", "Low")` —
#'     label and value are identical.}
#'   \item{Named character vector}{`c("High" = "high", "Medium" = "med")` —
#'     names become labels, values become values.}
#'   \item{List of lists}{`list(list(label = "High", value = "high"), ...)` —
#'     canonical format, returned as-is.}
#' }
#'
#' @param choices A character vector, named character vector, or list of
#'   lists. Empty strings and `NA` values in character vectors are rejected.
#'
#' @return A list of lists, each with `label` (character) and `value`.
#'
#' @examples
#' # Unnamed vector — label == value
#' normalize_choices(c("High", "Medium", "Low"))
#'
#' # Named vector — names are labels
#' normalize_choices(c("High" = "high", "Medium" = "med", "Low" = "low"))
#'
#' # Already canonical — returned as-is
#' normalize_choices(list(
#'   list(label = "Good", value = 3),
#'   list(label = "Bad",  value = 1)
#' ))
#'
#' @importFrom purrr map imap
#' @export
normalize_choices <- function(choices) {
  if (is.character(choices)) {
    if (length(choices) == 0L) {
      stop("choices must contain at least one entry.", call. = FALSE)
    }
    if (any(is.na(choices))) {
      stop("choices must not contain NA values.", call. = FALSE)
    }
    if (any(nchar(choices) == 0L)) {
      stop(
        "choices must not contain empty strings. ",
        "Each choice needs a non-empty label.",
        call. = FALSE
      )
    }

    nms <- names(choices)
    if (is.null(nms)) {
      # Unnamed vector: label == value
      purrr::map(unname(choices), \(x) list(label = x, value = x))
    } else {
      # Named vector: names are labels, values are values
      if (any(nchar(nms) == 0L)) {
        stop(
          "Named choice vectors must not have empty names. ",
          "Every element needs a non-empty name as its label.",
          call. = FALSE
        )
      }
      purrr::imap(choices, \(val, lab) list(label = lab, value = val)) |>
        unname()
    }
  } else if (is.list(choices)) {
    choices
  } else {
    stop(
      "choices must be a character vector, named character vector, ",
      "or list of lists (each with 'label' and 'value').",
      call. = FALSE
    )
  }
}
