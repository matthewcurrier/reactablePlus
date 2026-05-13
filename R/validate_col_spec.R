# R/validate_col_spec.R

#' Validate a col_spec list
#'
#' Checks that a col_spec is well-formed before any rendering or data
#' manipulation occurs. Throws an informative error on the first problem found.
#'
#' Dropdown choices are normalized before validation: unnamed character
#' vectors, named character vectors, and list-of-lists are all accepted.
#' See `normalize_choices()` for details.
#'
#' @param col_spec A non-empty list of column definition lists. Each entry must
#'   contain \code{col_name} (string), \code{label} (string), and \code{type}
#'   (one of \code{"dropdown"}, \code{"numeric"}, \code{"date"},
#'   \code{"checkbox"}, \code{"toggle"}, or \code{"text"}). Dropdown entries
#'   also require \code{choices} — a character vector, named character vector,
#'   or list of lists (each with \code{label} and \code{value}).
#'   Numeric entries also require \code{min} (numeric).
#' @param row_spec A validated row_spec list, or \code{NULL}. Required when any
#'   \code{gate} condition uses \code{type = "selected"}, which requires
#'   \code{row_spec$selectable == TRUE}.
#'
#' @return The validated (and normalized) col_spec, returned invisibly.
#'   Dropdown choices are guaranteed to be in list-of-lists form.
#' @export
validate_col_spec <- function(col_spec, row_spec = NULL) {
  supported_types <- c(
    "dropdown",
    "numeric",
    "date",
    "checkbox",
    "toggle",
    "text"
  )

  # ── Top-level structure ───────────────────────────────────────────────────────

  if (!is.list(col_spec)) {
    stop("col_spec must be a list.", call. = FALSE)
  }

  if (length(col_spec) == 0L) {
    stop("col_spec must not be empty.", call. = FALSE)
  }

  # ── Duplicate col_names ───────────────────────────────────────────────────────

  col_names <- purrr::map_chr(col_spec, function(entry) {
    if (!is.null(entry$col_name)) entry$col_name else NA_character_
  })

  if (anyDuplicated(col_names[!is.na(col_names)]) > 0L) {
    stop(
      "col_spec contains duplicate col_name values. Each col_name must be unique.",
      call. = FALSE
    )
  }

  # ── Normalize dropdown choices ─────────────────────────────────────────────

  col_spec <- purrr::map(col_spec, function(entry) {
    if (identical(entry$type, "dropdown") && !is.null(entry$choices)) {
      entry$choices <- normalize_choices(entry$choices)
    }
    entry
  })

  # ── Per-entry validation ──────────────────────────────────────────────────────

  purrr::iwalk(col_spec, function(entry, i) {
    # col_name
    if (is.null(entry$col_name)) {
      stop("col_spec[[", i, "]] is missing a 'col_name' field.", call. = FALSE)
    }
    if (!is.character(entry$col_name) || length(entry$col_name) != 1L) {
      stop(
        "col_spec[[",
        i,
        "]]$col_name must be a single character string.",
        call. = FALSE
      )
    }

    # label
    if (is.null(entry$label)) {
      stop("col_spec[[", i, "]] is missing a 'label' field.", call. = FALSE)
    }
    if (!is.character(entry$label) || length(entry$label) != 1L) {
      stop(
        "col_spec[[",
        i,
        "]]$label must be a single character string.",
        call. = FALSE
      )
    }

    # type
    if (is.null(entry$type)) {
      stop("col_spec[[", i, "]] is missing a 'type' field.", call. = FALSE)
    }
    if (!entry$type %in% supported_types) {
      stop(
        "col_spec[[",
        i,
        "]]$type '",
        entry$type,
        "' is not supported. ",
        "Must be one of: ",
        paste(supported_types, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    # type-specific validation
    if (entry$type == "dropdown") {
      validate_dropdown_entry(entry, i)
    } else if (entry$type == "numeric") {
      validate_numeric_entry(entry, i)
    } else if (entry$type == "date") {
      validate_date_entry(entry, i)
    } else if (entry$type %in% c("checkbox", "toggle")) {
      validate_boolean_entry(entry, i)
    } else if (entry$type == "text") {
      validate_text_entry(entry, i)
    }

    # depends_on is the old gating field — reject it with a clear error.
    if (!is.null(entry$depends_on)) {
      stop(
        "col_spec[[",
        i,
        "]] uses 'depends_on' which is no longer supported. ",
        "Use 'gate' instead. See the col_spec documentation for the gate structure.",
        call. = FALSE
      )
    }

    # gate is optional but validated strictly when present.
    if (!is.null(entry$gate)) {
      validate_gate_entry(entry, i, col_spec, row_spec)
    }
  })

  invisible(col_spec)
}


#' Normalize dropdown choices to list-of-lists format
#'
#' Converts shorthand choice formats into the canonical list-of-lists
#' structure expected by `validate_col_spec()` and the cell renderers.
#' Called automatically during validation, but can also be used directly.
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


# ── Internal helpers ──────────────────────────────────────────────────────────

#' @noRd
validate_dropdown_entry <- function(entry, i) {
  if (is.null(entry$choices)) {
    stop(
      "col_spec[[",
      i,
      "]] (dropdown) is missing a 'choices' field.",
      call. = FALSE
    )
  }

  if (!is.list(entry$choices)) {
    stop(
      "col_spec[[",
      i,
      "]]$choices must be a character vector, named character vector, ",
      "or list of lists (each with 'label' and 'value').",
      call. = FALSE
    )
  }

  if (length(entry$choices) == 0L) {
    stop(
      "col_spec[[",
      i,
      "]]$choices must contain at least one entry.",
      call. = FALSE
    )
  }

  purrr::iwalk(entry$choices, function(choice, j) {
    if (is.null(choice$label)) {
      stop(
        "col_spec[[",
        i,
        "]]$choices[[",
        j,
        "]] is missing a 'label' field.",
        call. = FALSE
      )
    }

    if (!is.character(choice$label) || length(choice$label) != 1L) {
      stop(
        "col_spec[[",
        i,
        "]]$choices[[",
        j,
        "]]$label must be a single character string.",
        call. = FALSE
      )
    }

    if (is.null(choice$value)) {
      stop(
        "col_spec[[",
        i,
        "]]$choices[[",
        j,
        "]] is missing a 'value' field.",
        call. = FALSE
      )
    }
  })

  # All choice values must be the same type
  value_classes <- purrr::map_chr(entry$choices, \(c) class(c$value)[[1L]])

  if (length(unique(value_classes)) > 1L) {
    stop(
      "col_spec[[",
      i,
      "]]$choices has mixed value types: ",
      paste(unique(value_classes), collapse = ", "),
      ". ",
      "All choice value entries must be the same type.",
      call. = FALSE
    )
  }
}

#' @noRd
validate_numeric_entry <- function(entry, i) {
  if (is.null(entry$min)) {
    stop(
      "col_spec[[",
      i,
      "]] (numeric) is missing a 'min' field.",
      call. = FALSE
    )
  }

  if (!is.numeric(entry$min) || length(entry$min) != 1L) {
    stop(
      "col_spec[[",
      i,
      "]]$min must be a single numeric value.",
      call. = FALSE
    )
  }
}

#' @noRd
validate_date_entry <- function(entry, i) {
  iso_pattern <- "^\\d{4}-\\d{2}-\\d{2}$"

  if (!is.null(entry$min_date)) {
    if (!is.character(entry$min_date) || length(entry$min_date) != 1L) {
      stop(
        "col_spec[[",
        i,
        "]]$min_date must be a single character string in YYYY-MM-DD format.",
        call. = FALSE
      )
    }
    if (!grepl(iso_pattern, entry$min_date)) {
      stop(
        "col_spec[[",
        i,
        "]]$min_date must be in YYYY-MM-DD format.",
        call. = FALSE
      )
    }
  }

  if (!is.null(entry$max_date)) {
    if (!is.character(entry$max_date) || length(entry$max_date) != 1L) {
      stop(
        "col_spec[[",
        i,
        "]]$max_date must be a single character string in YYYY-MM-DD format.",
        call. = FALSE
      )
    }
    if (!grepl(iso_pattern, entry$max_date)) {
      stop(
        "col_spec[[",
        i,
        "]]$max_date must be in YYYY-MM-DD format.",
        call. = FALSE
      )
    }
  }

  if (!is.null(entry$min_date) && !is.null(entry$max_date)) {
    if (as.Date(entry$max_date) < as.Date(entry$min_date)) {
      stop(
        "col_spec[[",
        i,
        "]]$max_date must not be earlier than $min_date.",
        call. = FALSE
      )
    }
  }
}

#' @noRd
validate_boolean_entry <- function(entry, i) {
  # checkbox and toggle take no extra required fields.
  # Guard against a caller accidentally attaching a choices field, which
  # would suggest they meant to use a dropdown instead.
  if (!is.null(entry$choices)) {
    stop(
      "col_spec[[",
      i,
      "]] (",
      entry$type,
      ") must not have a 'choices' field. ",
      "Use type = 'dropdown' if you need labelled options.",
      call. = FALSE
    )
  }
}

#' @noRd
validate_text_entry <- function(entry, i) {
  if (!is.null(entry$max_chars)) {
    if (!is.numeric(entry$max_chars) || length(entry$max_chars) != 1L) {
      stop(
        "col_spec[[",
        i,
        "]]$max_chars must be a single numeric value.",
        call. = FALSE
      )
    }
    if (entry$max_chars %% 1 != 0 || entry$max_chars < 1L) {
      stop(
        "col_spec[[",
        i,
        "]]$max_chars must be a positive whole number.",
        call. = FALSE
      )
    }
  }
}

#' @noRd
validate_gate_entry <- function(entry, i, col_spec, row_spec) {
  gate <- entry$gate

  # gate must be a non-empty list
  if (!is.list(gate)) {
    stop(
      "col_spec[[",
      i,
      "]]$gate must be a list of condition lists.",
      call. = FALSE
    )
  }

  if (length(gate) == 0L) {
    stop(
      "col_spec[[",
      i,
      "]]$gate must contain at least one condition.",
      call. = FALSE
    )
  }

  all_col_names <- purrr::map_chr(
    col_spec,
    \(e) if (!is.null(e$col_name)) e$col_name else NA_character_
  )

  purrr::iwalk(gate, function(cond, j) {
    # Every condition must have a type
    if (is.null(cond$type)) {
      stop(
        "col_spec[[",
        i,
        "]]$gate[[",
        j,
        "]] is missing a 'type' field.",
        call. = FALSE
      )
    }

    if (!cond$type %in% c("selected", "value")) {
      stop(
        "col_spec[[",
        i,
        "]]$gate[[",
        j,
        "]]$type must be 'selected' or 'value', ",
        "not '",
        cond$type,
        "'.",
        call. = FALSE
      )
    }

    if (cond$type == "selected") {
      # Requires row_spec$selectable == TRUE — using this condition without
      # selectable enabled is always a misconfiguration.
      if (!isTRUE(row_spec$selectable)) {
        stop(
          "col_spec[[",
          i,
          "]]$gate[[",
          j,
          "]] uses type 'selected' but ",
          "row_spec$selectable is not TRUE. Set selectable = TRUE in row_spec ",
          "to use selection-driven gating.",
          call. = FALSE
        )
      }
    }

    if (cond$type == "value") {
      # col_name must be present, a string, and refer to an earlier dropdown
      if (
        is.null(cond$col_name) ||
          !is.character(cond$col_name) ||
          length(cond$col_name) != 1L
      ) {
        stop(
          "col_spec[[",
          i,
          "]]$gate[[",
          j,
          "]] (type 'value') must have a ",
          "'col_name' field that is a single character string.",
          call. = FALSE
        )
      }

      controller_pos <- which(all_col_names == cond$col_name)

      if (length(controller_pos) == 0L) {
        stop(
          "col_spec[[",
          i,
          "]]$gate[[",
          j,
          "]]$col_name '",
          cond$col_name,
          "' ",
          "does not match any col_name in the col_spec.",
          call. = FALSE
        )
      }

      if (controller_pos >= i) {
        stop(
          "col_spec[[",
          i,
          "]]$gate[[",
          j,
          "]]$col_name '",
          cond$col_name,
          "' ",
          "must be declared before the dependent column in the col_spec.",
          call. = FALSE
        )
      }

      controller <- col_spec[[controller_pos]]
      if (controller$type != "dropdown") {
        stop(
          "col_spec[[",
          i,
          "]]$gate[[",
          j,
          "]] references '",
          cond$col_name,
          "' ",
          "which is type '",
          controller$type,
          "'. ",
          "Only dropdown columns can act as gate controllers.",
          call. = FALSE
        )
      }

      # values must be present, non-empty, and exist in the controller choices
      if (is.null(cond$values) || length(cond$values) == 0L) {
        stop(
          "col_spec[[",
          i,
          "]]$gate[[",
          j,
          "]] (type 'value') must have a ",
          "non-empty 'values' field.",
          call. = FALSE
        )
      }

      valid_choice_values <- purrr::map(controller$choices, \(c) c$value)
      invalid_values <- cond$values[!cond$values %in% valid_choice_values]

      if (length(invalid_values) > 0L) {
        stop(
          "col_spec[[",
          i,
          "]]$gate[[",
          j,
          "]]$values contains ",
          paste(invalid_values, collapse = ", "),
          " which are not valid choice values in '",
          cond$col_name,
          "'.",
          call. = FALSE
        )
      }
    }
  })
}
