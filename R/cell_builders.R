# R/cell_builders.R
#
# Cell builder functions for editable_table_server.
# Each function produces a reactable::colDef whose cell renderer emits a raw
# HTML input element with Shiny.setInputValue() wired to its event handler.
#
# Shared gate helpers:
#   is_initially_locked() — evaluates gate conditions at render time
#   locked_style()        — produces inline CSS for locked / enabled state
#   input_value_for_row() — reads a single cell from the display data frame
#   build_gate_map()      — pre-computes value_gates and selected_gates from col_spec
#   build_selection_gate_js() — generates JS to evaluate selected-gate state in the browser

# ── Shared gate helpers ───────────────────────────────────────────────────────

#' Read a column value for a specific row directly from the display data frame.
#' Used by cell renderers to determine initial gate state without needing input$.
#' @noRd
input_value_for_row <- function(df, row_id, id_col, col_name) {
  row <- df[df[[id_col]] == row_id, ]
  if (nrow(row) == 0L || !col_name %in% names(row)) return(NULL)
  row[[col_name]][[1L]]
}

#' Determine whether a cell should render locked on initial page load.
#' Returns TRUE when ANY gate condition fails, i.e. the gate is closed.
#' Gate conditions are evaluated from the display data frame — no Shiny
#' input$ access needed at render time.
#' @noRd
is_initially_locked <- function(spec, df, id_col, row_id, index) {
  if (is.null(spec$gate)) return(FALSE)

  any(purrr::map_lgl(spec$gate, function(cond) {
    if (cond$type == "selected") {
      if (".selected" %in% names(df)) !isTRUE(df[[".selected"]][[index]]) else TRUE
    } else if (cond$type == "value") {
      controller_val <- input_value_for_row(df, row_id, id_col, cond$col_name)
      is.null(controller_val) || is.na(controller_val) ||
        !controller_val %in% cond$values
    } else {
      FALSE
    }
  }))
}

#' Produce an inline CSS style string with locked-state overrides applied
#' when locked is TRUE.
#' @noRd
locked_style <- function(base, locked) {
  if (locked) {
    paste0(base, " opacity: 0.4; cursor: not-allowed; pointer-events: none;")
  } else {
    paste0(base, " cursor: default;")
  }
}

#' Pre-compute the gate map from a col_spec.
#' Called once per renderUI invocation and shared across all cell builders.
#'
#' Returns a list with two elements:
#'   value_gates    — keyed by controller col_name; lists dependent columns,
#'                    their unlocking values, and whether selection is also needed.
#'   selected_gates — keyed by dependent col_name; lists additional value
#'                    conditions that must also be met when the row is selected.
#' @noRd
build_gate_map <- function(col_spec) {
  purrr::reduce(col_spec, function(acc, spec) {
    if (is.null(spec$gate)) return(acc)

    has_selected <- any(purrr::map_lgl(spec$gate, \(cond) cond$type == "selected"))
    value_conds  <- purrr::keep(spec$gate, \(cond) cond$type == "value")

    if (has_selected) {
      acc$selected_gates[[spec$col_name]] <- purrr::map(value_conds, function(vc) {
        list(col_name = vc$col_name, values = vc$values)
      })
    }

    acc$value_gates <- purrr::reduce(value_conds, function(vg, vc) {
      dep <- list(
        col_name       = spec$col_name,
        values         = vc$values,
        needs_selected = has_selected
      )
      vg[[vc$col_name]] <- c(vg[[vc$col_name]], list(dep))
      vg
    }, .init = acc$value_gates)

    acc
  }, .init = list(value_gates = list(), selected_gates = list()))
}

#' Build inline JS that evaluates the full gate state for every selected-gated
#' field in a given row. Called by both the checkbox onchange and the row
#' onClick handler. Returns an empty string when there are no selected gates.
#'
#' Callers must define \code{isChecked} as a local JS variable before this
#' snippet runs.
#' @noRd
#' @noRd
build_selection_gate_js <- function(row_id, ns, gate_map) {
  selected_gates <- gate_map$selected_gates %||% list()
  if (length(selected_gates) == 0L) return("")

  purrr::imap_chr(selected_gates, function(value_conds, col_name) {
    dep_id <- ns(paste0(row_id, "_", col_name))

    value_check_js <- if (length(value_conds) == 0L) {
      "var valueCondMet = true;"
    } else {
      purrr::map_chr(seq_along(value_conds), function(k) {
        vc          <- value_conds[[k]]
        ctrl_id     <- ns(paste0(row_id, "_", vc$col_name))
        values_json <- jsonlite::toJSON(vc$values, auto_unbox = FALSE)
        sprintf(
          "var ctrlEl%d = document.getElementById('%s');
           var ctrlVal%d = ctrlEl%d ? ctrlEl%d.value : '';
           var cond%d = %s.indexOf(ctrlVal%d) !== -1;",
          k, ctrl_id, k, k, k, k, values_json, k
        )
      }) |>
        paste(collapse = "\n") |>
        paste0(
          "\nvar valueCondMet = ",
          paste(sprintf("cond%d", seq_along(value_conds)), collapse = " && "),
          ";"
        )
    }

    sprintf(
      "(function() {
         var depEl = document.getElementById('%s');
         if (!depEl) return;
         %s
         var isOpen = isChecked && valueCondMet;
         depEl.disabled            = !isOpen;
         depEl.style.opacity       = isOpen ? '1' : '0.4';
         depEl.style.cursor        = isOpen ? 'default' : 'not-allowed';
         depEl.style.pointerEvents = isOpen ? '' : 'none';
         if (!isOpen) { depEl.value = depEl.type === 'number' ? depEl.min : ''; }
       })();",
      dep_id, value_check_js
    )
  }) |> paste(collapse = "\n")
}

# ── Column definition builders ────────────────────────────────────────────────

#' Build the selection checkbox colDef (prepended before display columns)
#' @noRd
build_select_col_def <- function(df, id_col, ns, gate_map = list()) {
  purrr::set_names(
    list(
      reactable::colDef(
        name   = "",
        width  = 40L,
        align  = "center",
        cell   = function(value, index) {
          row_id     <- df[[id_col]][[index]]
          local_id   <- paste0(row_id, "_selected")
          namespaced <- ns(local_id)

          selection_gate_js <- build_selection_gate_js(
            row_id   = row_id,
            ns       = ns,
            gate_map = gate_map
          )

          onchange_js <- paste0(
            sprintf(
              "var isChecked = this.checked; Shiny.setInputValue('%s', isChecked, {priority: 'event'});",
              namespaced
            ),
            if (nchar(selection_gate_js) > 0L) paste0("\n", selection_gate_js) else ""
          )

          htmltools::tags$input(
            id       = namespaced,
            type     = "checkbox",
            checked  = if (isTRUE(value)) "checked" else NULL,
            onchange = onchange_js,
            onclick  = "event.stopPropagation();",
            style    = "width: 16px; height: 16px; cursor: pointer;"
          )
        }
      )
    ),
    ".selected"
  )
}

#' Build reactable colDefs for the read-only display columns
#' @noRd
build_display_col_defs <- function(row_spec) {
  purrr::map(row_spec$display_cols, function(d) {
    reactable::colDef(name = d$label)
  }) |>
    purrr::set_names(purrr::map_chr(row_spec$display_cols, \(d) d$col_name))
}

#' Dispatch builder: produces a colDef for every col_spec entry
#' @noRd
build_input_col_defs <- function(df, id_col, col_spec, ns, gate_map = list()) {
  purrr::map(col_spec, function(spec) {
    if (spec$type == "dropdown") {
      build_dropdown_col_def(df, id_col, spec, ns, gate_map)
    } else if (spec$type == "numeric") {
      build_numeric_col_def(df, id_col, spec, ns)
    } else if (spec$type == "date") {
      build_date_col_def(df, id_col, spec, ns)
    } else if (spec$type == "checkbox") {
      build_checkbox_col_def(df, id_col, spec, ns)
    } else if (spec$type == "toggle") {
      build_toggle_col_def(df, id_col, spec, ns)
    } else if (spec$type == "text") {
      build_text_col_def(df, id_col, spec, ns)
    }
  }) |>
    purrr::set_names(purrr::map_chr(col_spec, \(s) s$col_name))
}

#' @noRd
build_dropdown_col_def <- function(df, id_col, spec, ns, gate_map = list()) {

  dependents <- gate_map$value_gates[[spec$col_name]] %||% list()

  reactable::colDef(
    name = spec$label,
    cell = function(value, index) {
      row_id     <- df[[id_col]][[index]]
      local_id   <- paste0(row_id, "_", spec$col_name)
      namespaced <- ns(local_id)
      has_value  <- !is.null(value) && !is.na(value)

      is_selected_gated <- !is.null(spec$gate) &&
        any(purrr::map_lgl(spec$gate, \(cond) cond$type == "selected"))
      row_is_selected  <- isTRUE(df[[".selected"]][[index]])
      initially_locked <- is_selected_gated && !row_is_selected

      placeholder <- htmltools::tags$option(
        value    = "",
        disabled = "disabled",
        selected = if (!has_value) "selected" else NULL,
        "-- Select --"
      )

      choice_tags <- purrr::map(spec$choices, function(choice) {
        is_selected <- has_value && identical(value, choice$value)
        htmltools::tags$option(
          value    = choice$value,
          selected = if (is_selected) "selected" else NULL,
          choice$label
        )
      })

      gate_js <- purrr::map_chr(dependents, function(dep) {
        dep_id                <- ns(paste0(row_id, "_", dep$col_name))
        unlocking_values_json <- jsonlite::toJSON(dep$values, auto_unbox = FALSE)
        sel_id                <- ns(paste0(row_id, "_selected"))

        selection_check <- if (dep$needs_selected) {
          sprintf(
            "var selEl = document.getElementById('%s'); var isSelected = selEl ? selEl.checked : false;",
            sel_id
          )
        } else {
          "var isSelected = true;"
        }

        sprintf(
          "(function() {
             %s
             var depEl = document.getElementById('%s');
             if (depEl) {
               var unlocking = %s;
               var isOpen = isSelected && unlocking.indexOf(this.value) !== -1;
               depEl.disabled            = !isOpen;
               depEl.style.opacity       = isOpen ? '1' : '0.4';
               depEl.style.cursor        = isOpen ? 'default' : 'not-allowed';
               depEl.style.pointerEvents = isOpen ? '' : 'none';
               if (!isOpen) {
                 depEl.value = depEl.type === 'number' ? depEl.min : '';
               }
             }
           }).call(this);",
          selection_check, dep_id, unlocking_values_json
        )
      }) |> paste(collapse = "\n")

      onchange_js <- paste0(
        sprintf("Shiny.setInputValue('%s', this.value, {priority: 'event'});", namespaced),
        if (length(dependents) > 0L) paste0("\n", gate_js) else ""
      )

      htmltools::tags$select(
        id       = namespaced,
        onchange = onchange_js,
        disabled = if (initially_locked) "disabled" else NULL,
        style    = locked_style(
          "width: 100%; padding: 4px; background-color: white;",
          initially_locked
        ),
        placeholder,
        choice_tags
      )
    }
  )
}

#' @noRd
build_numeric_col_def <- function(df, id_col, spec, ns) {
  reactable::colDef(
    name = spec$label,
    cell = function(value, index) {
      row_id     <- df[[id_col]][[index]]
      local_id   <- paste0(row_id, "_", spec$col_name)
      namespaced <- ns(local_id)
      initial    <- if (is.na(value)) spec$min else value
      locked     <- is_initially_locked(spec, df, id_col, row_id, index)

      htmltools::tags$input(
        id       = namespaced,
        type     = "number",
        value    = initial,
        min      = spec$min,
        disabled = if (locked) "disabled" else NULL,
        oninput  = sprintf(
          "Shiny.setInputValue('%s', parseInt(this.value, 10), {priority: 'event'});",
          namespaced
        ),
        style    = locked_style("width: 100%; padding: 4px;", locked)
      )
    }
  )
}

#' @noRd
build_date_col_def <- function(df, id_col, spec, ns) {
  reactable::colDef(
    name = spec$label,
    cell = function(value, index) {
      row_id     <- df[[id_col]][[index]]
      local_id   <- paste0(row_id, "_", spec$col_name)
      namespaced <- ns(local_id)
      initial    <- if (is.na(value)) "" else format(as.Date(value), "%Y-%m-%d")
      locked     <- is_initially_locked(spec, df, id_col, row_id, index)

      htmltools::tags$input(
        id       = namespaced,
        type     = "date",
        value    = initial,
        min      = if (!is.null(spec$min_date)) spec$min_date else NULL,
        max      = if (!is.null(spec$max_date)) spec$max_date else NULL,
        disabled = if (locked) "disabled" else NULL,
        onchange = sprintf("Shiny.setInputValue('%s', this.value, {priority: 'event'});", namespaced),
        style    = locked_style("width: 100%; padding: 4px;", locked)
      )
    }
  )
}

#' @noRd
build_checkbox_col_def <- function(df, id_col, spec, ns) {
  reactable::colDef(
    name = spec$label,
    cell = function(value, index) {
      row_id     <- df[[id_col]][[index]]
      local_id   <- paste0(row_id, "_", spec$col_name)
      namespaced <- ns(local_id)
      is_checked <- isTRUE(value)
      locked     <- is_initially_locked(spec, df, id_col, row_id, index)

      htmltools::tags$input(
        id       = namespaced,
        type     = "checkbox",
        checked  = if (is_checked) "checked" else NULL,
        disabled = if (locked) "disabled" else NULL,
        onchange = sprintf("Shiny.setInputValue('%s', this.checked, {priority: 'event'});", namespaced),
        style    = locked_style("width: 20px; height: 20px;", locked)
      )
    }
  )
}

#' @noRd
build_toggle_col_def <- function(df, id_col, spec, ns) {
  reactable::colDef(
    name = spec$label,
    cell = function(value, index) {
      row_id     <- df[[id_col]][[index]]
      local_id   <- paste0(row_id, "_", spec$col_name)
      namespaced <- ns(local_id)
      is_on      <- isTRUE(value)
      locked     <- is_initially_locked(spec, df, id_col, row_id, index)
      toggle_id  <- paste0(namespaced, "_btn")
      state_id   <- namespaced

      htmltools::tagList(
        htmltools::tags$input(
          id    = state_id,
          type  = "hidden",
          value = tolower(as.character(is_on))
        ),
        htmltools::tags$button(
          id       = toggle_id,
          type     = "button",
          disabled = if (locked) "disabled" else NULL,
          style    = paste0(
            "padding: 3px 12px; border-radius: 12px; border: none; ",
            "background-color: ", if (is_on) "#28a745" else "#6c757d", "; ",
            "color: white; font-size: 0.85em; ",
            if (locked) "opacity: 0.4; cursor: not-allowed; pointer-events: none;" else "cursor: pointer;"
          ),
          onclick = if (!locked) sprintf(
            "(function() {
               var s = document.getElementById('%s');
               var b = document.getElementById('%s');
               var newVal = s.value !== 'true';
               s.value = newVal;
               b.textContent = newVal ? 'On' : 'Off';
               b.style.backgroundColor = newVal ? '#28a745' : '#6c757d';
               Shiny.setInputValue('%s', newVal, {priority: 'event'});
             })();",
            state_id, toggle_id, namespaced
          ) else NULL,
          if (is_on) "On" else "Off"
        )
      )
    }
  )
}

#' @noRd
build_text_col_def <- function(df, id_col, spec, ns) {
  reactable::colDef(
    name = spec$label,
    cell = function(value, index) {
      row_id     <- df[[id_col]][[index]]
      local_id   <- paste0(row_id, "_", spec$col_name)
      namespaced <- ns(local_id)
      initial    <- if (is.na(value)) "" else as.character(value)
      locked     <- is_initially_locked(spec, df, id_col, row_id, index)

      htmltools::tags$input(
        id        = namespaced,
        type      = "text",
        value     = initial,
        maxlength = if (!is.null(spec$max_chars)) spec$max_chars else NULL,
        disabled  = if (locked) "disabled" else NULL,
        oninput   = sprintf("Shiny.setInputValue('%s', this.value, {priority: 'event'});", namespaced),
        style     = locked_style("width: 100%; padding: 4px;", locked)
      )
    }
  )
}
