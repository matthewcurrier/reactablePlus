# R/constructors.R
#
# Constructor functions for the raw-spec table module. These produce
# validated, S3-classed objects that are easier to build correctly than
# bare lists. The validators and server function accept both constructors
# and plain lists for backward compatibility.


#' Define a display column
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [table_config()] with `row_labels` instead. Display columns
#' in the config-driven API are handled by `row_keys` / `row_labels`
#' and the optional `badge_col` / `badge_render_fn`.
#'
#' @param col_name Character. The column name in the data frame.
#' @param label Character. The header label shown in the table.
#'
#' @return A `display_col` object (an S3-classed list).
#' @keywords internal
#' @export
display_col <- function(col_name, label) {
  if (!isTRUE(getOption("reactablePlus.suppress_deprecation"))) {
    .Deprecated("table_config", package = "reactablePlus")
  }
  if (missing(col_name) || !is.character(col_name) || length(col_name) != 1L) {
    stop("col_name must be a single character string.", call. = FALSE)
  }
  if (nchar(col_name) == 0L) {
    stop("col_name must not be empty.", call. = FALSE)
  }
  if (missing(label) || !is.character(label) || length(label) != 1L) {
    stop("label must be a single character string.", call. = FALSE)
  }
  if (nchar(label) == 0L) {
    stop("label must not be empty.", call. = FALSE)
  }

  structure(
    list(col_name = col_name, label = label),
    class = "display_col"
  )
}


#' Define the row layout for an editable table
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [table_config()] instead. Row layout in the config-driven API
#' is specified via `row_keys`, `row_labels`, and `selectable`.
#'
#' @param id_col Character. The name of the column that uniquely
#'   identifies each row.
#' @param display_cols A list of `display_col()` objects (or plain lists
#'   with `col_name` and `label`).
#' @param selectable Logical. If `TRUE`, a checkbox column is prepended
#'   and row selection is enabled. Default `NULL` (no selection).
#'
#' @return A `row_def` object (an S3-classed list).
#' @keywords internal
#' @export
row_def <- function(id_col, display_cols, selectable = NULL) {
  if (!isTRUE(getOption("reactablePlus.suppress_deprecation"))) {
    .Deprecated("table_config", package = "reactablePlus")
  }
  if (missing(id_col) || !is.character(id_col) || length(id_col) != 1L) {
    stop("id_col must be a single character string.", call. = FALSE)
  }
  if (nchar(id_col) == 0L) {
    stop("id_col must not be empty.", call. = FALSE)
  }

  if (missing(display_cols) || !is.list(display_cols)) {
    stop("display_cols must be a list of display_col() objects.", call. = FALSE)
  }
  if (length(display_cols) == 0L) {
    stop("display_cols must contain at least one entry.", call. = FALSE)
  }

  # Validate each display_col entry
  purrr::iwalk(display_cols, function(dc, i) {
    if (is.null(dc$col_name) || is.null(dc$label)) {
      stop(
        "display_cols[[", i, "]] must have 'col_name' and 'label'. ",
        "Use display_col() to build entries.",
        call. = FALSE
      )
    }
  })

  if (!is.null(selectable)) {
    if (!is.logical(selectable) || length(selectable) != 1L) {
      stop("selectable must be TRUE, FALSE, or NULL.", call. = FALSE)
    }
  }

  structure(
    list(
      id_col       = id_col,
      display_cols = display_cols,
      selectable   = selectable
    ),
    class = "row_def"
  )
}


#' Define an editable input column
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [widget_col()] instead. `widget_col()` accepts the same
#' primitive types (`"dropdown"`, `"numeric"`, etc.) plus picker
#' widgets, with type-specific options passed via `options = list(...)`.
#'
#' @param col_name Character. The column name.
#' @param label Character. The header label.
#' @param type Character. One of `"dropdown"`, `"numeric"`, `"date"`,
#'   `"checkbox"`, `"toggle"`, or `"text"`.
#' @param choices For `type = "dropdown"` only.
#' @param min For `type = "numeric"`. Required minimum value.
#' @param max For `type = "numeric"`. Optional maximum value.
#' @param step For `type = "numeric"`. Optional step increment.
#' @param min_date For `type = "date"`. Optional minimum date.
#' @param max_date For `type = "date"`. Optional maximum date.
#' @param max_chars For `type = "text"`. Optional maximum character count.
#' @param gate Optional list of gating conditions.
#'
#' @return An `input_col` object (an S3-classed list).
#' @keywords internal
#' @export
input_col <- function(col_name,
                      label,
                      type,
                      choices   = NULL,
                      min       = NULL,
                      max       = NULL,
                      step      = NULL,
                      min_date  = NULL,
                      max_date  = NULL,
                      max_chars = NULL,
                      gate      = NULL) {
  if (!isTRUE(getOption("reactablePlus.suppress_deprecation"))) {
    .Deprecated("widget_col", package = "reactablePlus")
  }

  supported_types <- c(
    "dropdown", "numeric", "date", "checkbox", "toggle", "text"
  )

  # ── Required fields ──────────────────────────────────────────────────────

  if (missing(col_name) || !is.character(col_name) || length(col_name) != 1L) {
    stop("col_name must be a single character string.", call. = FALSE)
  }
  if (nchar(col_name) == 0L) {
    stop("col_name must not be empty.", call. = FALSE)
  }

  if (missing(label) || !is.character(label) || length(label) != 1L) {
    stop("label must be a single character string.", call. = FALSE)
  }

  if (missing(type) || !type %in% supported_types) {
    stop(
      "type must be one of: ",
      paste(supported_types, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  # ── Type-specific checks ─────────────────────────────────────────────────

  if (type == "dropdown") {
    if (is.null(choices)) {
      stop(
        "choices is required for type = \"dropdown\".",
        call. = FALSE
      )
    }
    choices <- normalize_choices(choices)
  }

  if (type == "numeric") {
    if (is.null(min)) {
      stop("min is required for type = \"numeric\".", call. = FALSE)
    }
    if (!is.numeric(min) || length(min) != 1L) {
      stop("min must be a single numeric value.", call. = FALSE)
    }
  }

  # ── Build the object ─────────────────────────────────────────────────────

  obj <- list(
    col_name = col_name,
    label    = label,
    type     = type
  )

  # Attach optional fields only when provided (keeps the object clean)
  if (!is.null(choices))   obj$choices   <- choices
  if (!is.null(min))       obj$min       <- min
  if (!is.null(max))       obj$max       <- max
  if (!is.null(step))      obj$step      <- step
  if (!is.null(min_date))  obj$min_date  <- min_date
  if (!is.null(max_date))  obj$max_date  <- max_date
  if (!is.null(max_chars)) obj$max_chars <- max_chars
  if (!is.null(gate))      obj$gate      <- gate

  structure(obj, class = "input_col")
}


#' @export
print.display_col <- function(x, ...) {
  cat("<display_col>\n")
  cat("  col_name:", x$col_name, "\n")
  cat("  label:   ", x$label, "\n")
  invisible(x)
}

#' @export
print.row_def <- function(x, ...) {
  n <- length(x$display_cols)
  cat("<row_def>\n")
  cat("  id_col:    ", x$id_col, "\n")
  cat("  display:   ", n, "column(s)\n")
  if (!is.null(x$selectable)) {
    cat("  selectable:", x$selectable, "\n")
  }
  invisible(x)
}

#' @export
print.input_col <- function(x, ...) {
  cat("<input_col>\n")
  cat("  col_name:", x$col_name, "\n")
  cat("  label:   ", x$label, "\n")
  cat("  type:    ", x$type, "\n")
  if (x$type == "dropdown" && !is.null(x$choices)) {
    labels <- purrr::map_chr(x$choices, "label")
    cat("  choices: ", paste(labels, collapse = ", "), "\n")
  }
  if (!is.null(x$min)) cat("  min:     ", x$min, "\n")
  if (!is.null(x$max)) cat("  max:     ", x$max, "\n")
  if (!is.null(x$gate)) cat("  gate:     (", length(x$gate), "condition(s))\n")
  invisible(x)
}
