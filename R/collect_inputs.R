# R/collect_inputs.R

#' Collect inline Shiny input values into a tidy tibble
#'
#' Reads each inline input from the Shiny \code{input} object using the
#' convention \code{paste0(row_id, "_", col_name)}. If an input has not been
#' touched by the user (i.e. it is \code{NULL}), the value is taken from
#' \code{fallback_df} instead. Gating is enforced server-side: if any
#' condition in a field's \code{gate} list fails for a given row, that
#' field's value is forced to \code{NA} regardless of the DOM state.
#'
#' @param input       The Shiny \code{input} object from \code{moduleServer}.
#' @param row_ids     A vector of row identifier values (typically the id_col
#'   column of the display data frame).
#' @param col_spec    A validated col_spec list.
#' @param id_col      A single character string naming the id column.
#' @param fallback_df A tibble from \code{merge_existing_data()} used as a
#'   fallback when an input has not yet been set by the user.
#' @param input_overrides A \code{reactiveValues} of server-controlled overrides.
#'   Checked before \code{input[[local_id]]} so that reset and deselection
#'   wipes take effect in the same reactive flush without a browser round-trip.
#'
#' @return A tibble with one row per element of \code{row_ids} and one column
#'   per col_spec entry, plus the id column.
#' @importFrom tibble as_tibble
#' @export
collect_inputs <- function(
  input,
  row_ids,
  col_spec,
  id_col,
  fallback_df = NULL,
  input_overrides = NULL
) {
  purrr::map(row_ids, function(row_id) {
    # Collect raw values for every column first so gating can read controller
    # values in the same pass.
    raw_values <- purrr::map(col_spec, function(spec) {
      local_id <- paste0(row_id, "_", spec$col_name)

      # input_overrides takes precedence — written synchronously by the server
      # on reset and deselection, before the browser has responded.
      val <- if (
        !is.null(input_overrides) && !is.null(input_overrides[[local_id]])
      ) {
        input_overrides[[local_id]]
      } else {
        input[[local_id]]
      }

      # Fall back to the pre-filled / merged value when the user has not
      # interacted with this input yet.
      if (is.null(val) && !is.null(fallback_df)) {
        fb_row <- fallback_df[fallback_df[[id_col]] == row_id, ]
        val <- fb_row[[spec$col_name]][[1L]]
      }

      # For inputs that have not been touched and have no existing data,
      # mirror the blank default that the cell renderer shows in the browser.
      if (is.null(val) || (length(val) == 1L && is.na(val))) {
        val <- switch(
          spec$type,
          numeric = spec$min,
          checkbox = FALSE,
          toggle = FALSE,
          text = "",
          val # date and dropdown stay NA
        )
      }

      # Coerce date strings coming from the browser to R Date objects.
      if (spec$type == "date" && !is.na(val) && is.character(val)) {
        val <- as.Date(val)
      }

      val
    }) |>
      purrr::set_names(purrr::map_chr(col_spec, \(s) s$col_name))

    # Enforce gating: the server is authoritative.
    # For each field with a gate, ALL conditions must pass or the value is NA.
    gated_values <- purrr::map(col_spec, function(spec) {
      val <- raw_values[[spec$col_name]]

      if (!is.null(spec$gate)) {
        gate_open <- all(purrr::map_lgl(spec$gate, function(cond) {
          if (cond$type == "selected") {
            isTRUE(input[[paste0(row_id, "_selected")]])
          } else if (cond$type == "value") {
            controller_val <- raw_values[[cond$col_name]]
            !is.null(controller_val) &&
              length(controller_val) == 1L &&
              !is.na(controller_val) &&
              controller_val %in% cond$values
          } else {
            FALSE
          }
        }))

        if (!gate_open) val <- NA
      }

      val
    }) |>
      purrr::set_names(purrr::map_chr(col_spec, \(s) s$col_name))

    # Combine id column and gated values into a single-row tibble.
    # purrr::set_names avoids the := operator entirely.
    id_tibble <- purrr::set_names(list(row_id), id_col) |> tibble::as_tibble()
    values_tibble <- tibble::as_tibble(gated_values)
    dplyr::bind_cols(id_tibble, values_tibble)
  }) |>
    dplyr::bind_rows()
}
