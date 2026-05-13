# R/validate_row_spec.R

#' Validate a row_spec list
#'
#' Checks that a row_spec is well-formed before any rendering or data
#' manipulation occurs. Throws an informative error on the first problem found.
#'
#' @param row_spec A list with fields \code{id_col} (string) and
#'   \code{display_cols} (list of lists, each with \code{col_name} and
#'   \code{label}).
#'
#' @return Called for its side-effect. Returns invisibly on success.
#' @export
validate_row_spec <- function(row_spec) {

  # ── id_col ──────────────────────────────────────────────────────────────────

  if (is.null(row_spec$id_col)) {
    stop("row_spec must contain an 'id_col' field.", call. = FALSE)
  }

  if (!is.character(row_spec$id_col) || length(row_spec$id_col) != 1L) {
    stop("row_spec$id_col must be a single character string.", call. = FALSE)
  }

  if (nchar(row_spec$id_col) == 0L) {
    stop("row_spec$id_col must not be an empty string.", call. = FALSE)
  }

  # ── selectable ────────────────────────────────────────────────────────────────

  if (!is.null(row_spec$selectable)) {
    if (!is.logical(row_spec$selectable) || length(row_spec$selectable) != 1L) {
      stop(
        "row_spec$selectable must be a single logical value (TRUE or FALSE).",
        call. = FALSE
      )
    }
  }

  # ── display_cols ─────────────────────────────────────────────────────────────

  if (is.null(row_spec$display_cols)) {
    stop("row_spec must contain a 'display_cols' field.", call. = FALSE)
  }

  if (!is.list(row_spec$display_cols)) {
    stop("row_spec$display_cols must be a list.", call. = FALSE)
  }

  if (length(row_spec$display_cols) == 0L) {
    stop("row_spec$display_cols must contain at least one entry.", call. = FALSE)
  }

  purrr::iwalk(row_spec$display_cols, function(entry, i) {

    if (is.null(entry$col_name)) {
      stop(
        "row_spec$display_cols[[", i, "]] is missing a 'col_name' field.",
        call. = FALSE
      )
    }

    if (!is.character(entry$col_name) || length(entry$col_name) != 1L) {
      stop(
        "row_spec$display_cols[[", i, "]]$col_name must be a single character string.",
        call. = FALSE
      )
    }

    if (is.null(entry$label)) {
      stop(
        "row_spec$display_cols[[", i, "]] is missing a 'label' field.",
        call. = FALSE
      )
    }

    if (!is.character(entry$label) || length(entry$label) != 1L) {
      stop(
        "row_spec$display_cols[[", i, "]]$label must be a single character string.",
        call. = FALSE
      )
    }
  })

  invisible(row_spec)
}
