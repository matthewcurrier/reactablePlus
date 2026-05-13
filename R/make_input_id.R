# R/make_input_id.R

#' Generate a stable, unique input element ID
#'
#' Combines a namespace prefix, a row identifier, and a column name into a
#' single dash-separated string suitable for use as an HTML element ID or
#' for CSS targeting.
#'
#' @param ns_prefix A single non-empty character string — typically the Shiny
#'   module namespace.
#' @param row_id    A scalar (integer or character) uniquely identifying the
#'   row. Must not be \code{NA}.
#' @param col_name  A single non-empty character string matching a
#'   \code{col_name} field in a col_spec entry.
#'
#' @return A single character string of the form
#'   \code{"<ns_prefix>-<row_id>-<col_name>"}.
#' @export
make_input_id <- function(ns_prefix, row_id, col_name) {
  if (!is.character(ns_prefix) || length(ns_prefix) != 1L) {
    stop("ns_prefix must be a single character string.", call. = FALSE)
  }

  if (length(row_id) != 1L || is.na(row_id)) {
    stop("row_id must be a single non-NA scalar value.", call. = FALSE)
  }

  if (!is.character(col_name) || length(col_name) != 1L) {
    stop("col_name must be a single character string.", call. = FALSE)
  }

  paste(ns_prefix, row_id, col_name, sep = "-")
}
