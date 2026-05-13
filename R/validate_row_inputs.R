# R/validate_row_inputs.R

#' Validate user-supplied input values against col_spec rules
#'
#' Checks every row × col_spec-column combination in \code{df} for:
#' \itemize{
#'   \item Numeric columns: must be non-NA, a whole number, and >= \code{min}.
#'   \item Dropdown columns: must be non-NA and one of the declared choice values.
#' }
#'
#' @param df       A tibble produced by \code{collect_inputs()}, containing the
#'   id_col and one column per col_spec entry.
#' @param row_spec A validated row_spec list (used to identify the id column).
#' @param col_spec A validated col_spec list.
#'
#' @return A list with two fields:
#' \describe{
#'   \item{\code{valid}}{Logical. \code{TRUE} only when no errors are found.}
#'   \item{\code{errors}}{A list of error entries, each a list of
#'     \code{row_id}, \code{col_name}, and \code{message}.}
#' }
#' @export
validate_row_inputs <- function(df, row_spec, col_spec) {
  id_col <- row_spec$id_col
  row_ids <- df[[id_col]]

  errors <- purrr::flatten(
    purrr::map(col_spec, function(spec) {
      col_values <- df[[spec$col_name]]

      purrr::compact(
        purrr::map2(row_ids, col_values, function(row_id, val) {
          check_input_value(val, row_id, spec)
        })
      )
    })
  )

  list(valid = length(errors) == 0L, errors = errors)
}

# ── Internal helpers ──────────────────────────────────────────────────────────

#' Validate a single cell value; returns an error list or NULL if valid.
#' @noRd
#' @noRd
check_input_value <- function(val, row_id, spec) {
  if (spec$type == "numeric") {
    check_numeric_value(val, row_id, spec)
  } else if (spec$type == "dropdown") {
    check_dropdown_value(val, row_id, spec)
  } else if (spec$type == "date") {
    check_date_value(val, row_id, spec)
  } else if (spec$type %in% c("checkbox", "toggle")) {
    check_boolean_value(val, row_id, spec)
  } else if (spec$type == "text") {
    check_text_value(val, row_id, spec)
  }
}

#' @noRd
check_numeric_value <- function(val, row_id, spec) {
  if (is.na(val)) {
    return(list(
      row_id = row_id,
      col_name = spec$col_name,
      message = paste0(spec$label, " cannot be blank.")
    ))
  }

  if (!is.numeric(val) || (val %% 1 != 0)) {
    return(list(
      row_id = row_id,
      col_name = spec$col_name,
      message = paste0(spec$label, " must be a whole number.")
    ))
  }

  if (val < spec$min) {
    return(list(
      row_id = row_id,
      col_name = spec$col_name,
      message = paste0(spec$label, " must be at least ", spec$min, ".")
    ))
  }

  NULL
}

#' @noRd
check_dropdown_value <- function(val, row_id, spec) {
  valid_values <- purrr::map(spec$choices, \(choice) choice$value)

  if (is.na(val) || !val %in% valid_values) {
    return(list(
      row_id = row_id,
      col_name = spec$col_name,
      message = paste0(spec$label, " must be selected.")
    ))
  }

  NULL
}

#' @noRd
check_date_value <- function(val, row_id, spec) {
  if (is.na(val)) {
    return(list(
      row_id = row_id,
      col_name = spec$col_name,
      message = paste0(spec$label, " cannot be blank.")
    ))
  }

  date_val <- tryCatch(as.Date(val), error = function(e) NA)

  if (is.na(date_val)) {
    return(list(
      row_id = row_id,
      col_name = spec$col_name,
      message = paste0(spec$label, " must be a valid date.")
    ))
  }

  if (!is.null(spec$min_date) && date_val < as.Date(spec$min_date)) {
    return(list(
      row_id = row_id,
      col_name = spec$col_name,
      message = paste0(spec$label, " must be on or after ", spec$min_date, ".")
    ))
  }

  if (!is.null(spec$max_date) && date_val > as.Date(spec$max_date)) {
    return(list(
      row_id = row_id,
      col_name = spec$col_name,
      message = paste0(spec$label, " must be on or before ", spec$max_date, ".")
    ))
  }

  NULL
}

#' @noRd
check_boolean_value <- function(val, row_id, spec) {
  if (is.na(val) || !is.logical(val)) {
    return(list(
      row_id = row_id,
      col_name = spec$col_name,
      message = paste0(spec$label, " must be TRUE or FALSE.")
    ))
  }
  NULL
}

#' @noRd
check_text_value <- function(val, row_id, spec) {
  if (is.na(val)) {
    return(list(
      row_id = row_id,
      col_name = spec$col_name,
      message = paste0(
        spec$label,
        " cannot be blank. Use an empty string if no value is needed."
      )
    ))
  }

  if (!is.null(spec$max_chars) && nchar(val) > spec$max_chars) {
    return(list(
      row_id = row_id,
      col_name = spec$col_name,
      message = paste0(
        spec$label,
        " must be ",
        spec$max_chars,
        " characters or fewer (currently ",
        nchar(val),
        ")."
      )
    ))
  }

  NULL
}
