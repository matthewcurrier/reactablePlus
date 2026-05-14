# =============================================================================
# config_table_render.R
#
# Rendering helpers for config_table_ui / config_table_server.
# Handles:
#   - Building the reactable data frame skeleton
#   - Building colDef lists (badge, year, selection, widget columns)
#   - Per-type cell rendering (search_picker, attendance_picker, etc.)
#   - Gate evaluation, locked-state CSS, mutual-exclusion cell wrapping
#   - Type-based input validation
#
# These are pure functions with no reactive state — they take config,
# current_rows, settings, and return HTML or colDef objects.
# =============================================================================

#' Default validation by widget type.
#' @noRd
.default_validate <- function(val, type) {
  switch(
    type,
    search_picker = {
      if (is.list(val) && !is.null(val$id)) val else NULL
    },
    attendance_picker = ,
    homeschool_picker = {
      if (is.list(val)) val else NULL
    },
    notes_input = ,
    text = {
      if (is.null(val) || length(val) == 0L) "" else as.character(val)[1L]
    },
    dropdown = {
      if (is.null(val) || identical(val, "")) {
        NA_character_
      } else {
        as.character(val)[1L]
      }
    },
    numeric = {
      num <- suppressWarnings(as.numeric(val))
      if (is.null(num) || length(num) == 0L || is.na(num)) NA_real_ else num
    },
    date = {
      if (is.null(val) || identical(val, "")) {
        NA_character_
      } else {
        as.character(val)[1L]
      }
    },
    checkbox = ,
    toggle = {
      isTRUE(val)
    },
    custom = val,
    val
  )
}


#' Evaluate whether a gate is open for a given row.
#'
#' All conditions must pass (AND logic). Returns TRUE when there is
#' no gate, or when every condition is satisfied.
#' @param gate List of conditions from `widget_col(gate = ...)`, or NULL.
#' @param row_state Named list of the current row's column values.
#' @return Logical scalar.
#' @noRd
.is_gate_open <- function(gate, row_state) {
  if (is.null(gate)) {
    return(TRUE)
  }

  all(purrr::map_lgl(gate, function(cond) {
    if (cond$type == "selected") {
      isTRUE(row_state$.selected)
    } else if (cond$type == "value") {
      ctrl_val <- row_state[[cond$col_id]]
      !is.null(ctrl_val) &&
        length(ctrl_val) == 1L &&
        !is.na(ctrl_val) &&
        as.character(ctrl_val) %in% as.character(cond$values)
    } else {
      FALSE
    }
  }))
}


#' Build inline CSS + disabled attribute for a locked input.
#' Returns a named list of tag attributes to splice into htmltools calls.
#' @noRd
.locked_attrs <- function(locked, base_style = "") {
  if (locked) {
    list(
      disabled = "disabled",
      style = paste0(
        base_style,
        " opacity: 0.4; cursor: not-allowed; pointer-events: none;"
      )
    )
  } else {
    list(
      disabled = NULL,
      style = if (nchar(base_style) > 0L) base_style else NULL
    )
  }
}


#' Build the reactable data frame from config and current rows.
#' @noRd
.build_table_df <- function(config, current_rows, settings) {
  n <- length(config$row_keys)

  # Start with row keys and labels
  tbl <- data.frame(
    .row_key = config$row_keys,
    .row_label = config$row_labels,
    stringsAsFactors = FALSE
  )

  # Selection column
  if (isTRUE(config$selectable)) {
    tbl$.selected <- vapply(
      current_rows,
      function(r) {
        if (!is.list(r)) {
          return(FALSE)
        }
        isTRUE(r$.selected)
      },
      logical(1)
    )
  }

  # Year column
  if (!is.null(config$year_col)) {
    tbl[[config$year_col]] <- vapply(
      current_rows,
      function(r) {
        if (!is.list(r)) {
          return(NA_integer_)
        }
        val <- r[[config$year_col]]
        if (is.null(val) || length(val) == 0L || is.na(val)) {
          NA_integer_
        } else {
          as.integer(val)
        }
      },
      integer(1)
    )
  }

  # Column values — primitive types get real values (enables sorting/filtering),
  # complex widget types get empty placeholders (cell fns render from current_rows).
  # Column values — always include ALL columns (gear-toggled visibility
  # is handled by CSS, not by omitting from the data frame).
  # Primitive types get real values (enables sorting/filtering),
  # complex widget types get empty placeholders (cell fns render from current_rows).

  primitive_types <- c(
    "dropdown",
    "numeric",
    "date",
    "checkbox",
    "toggle",
    "text",
    "notes_input"
  )

  tbl <- purrr::reduce(
    config$columns,
    function(acc, cs) {
      if (cs$type %in% primitive_types) {
        acc[[cs$id]] <- .extract_col_values(current_rows, cs)
      } else {
        acc[[cs$id]] <- rep("", n)
      }
      acc
    },
    .init = tbl
  )

  tbl
}


#' Extract a typed column vector from current_rows for a primitive column.
#' @noRd
.extract_col_values <- function(current_rows, cs) {
  switch(
    cs$type,
    numeric = vapply(
      current_rows,
      function(r) {
        if (!is.list(r)) {
          return(NA_real_)
        }
        val <- r[[cs$id]]
        if (is.null(val) || length(val) == 0L) NA_real_ else as.numeric(val)
      },
      numeric(1)
    ),

    checkbox = ,
    toggle = vapply(
      current_rows,
      function(r) {
        if (!is.list(r)) {
          return(FALSE)
        }
        isTRUE(r[[cs$id]])
      },
      logical(1)
    ),

    # dropdown, date, text, notes_input → character
    vapply(
      current_rows,
      function(r) {
        if (!is.list(r)) {
          return("")
        }
        val <- r[[cs$id]]
        if (is.null(val) || length(val) == 0L || is.na(val)) {
          ""
        } else {
          as.character(val)[1L]
        }
      },
      character(1)
    )
  )
}


#' Build reactable colDef list from config.
#' @noRd
.build_col_defs <- function(config, ns, current_rows, settings, tbl) {
  col_defs <- list()

  # Row key (hidden)
  col_defs$.row_key <- reactable::colDef(show = FALSE)

  # Selection checkbox column
  if (isTRUE(config$selectable)) {
    col_defs$.selected <- reactable::colDef(
      name = "",
      width = 40L,
      align = "center",
      html = TRUE,
      cell = function(value, index) {
        gk <- tbl$.row_key[index]
        row <- current_rows[[gk]]
        is_checked <- isTRUE(if (is.list(row)) row$.selected else FALSE)
        input_id <- ns(paste0(".selected_", gk))

        as.character(htmltools::tags$input(
          id = input_id,
          type = "checkbox",
          checked = if (is_checked) "checked" else NULL,
          onchange = sprintf(
            "Shiny.setInputValue('%s', this.checked, {priority: 'event'});",
            input_id
          ),
          onclick = "event.stopPropagation();",
          style = "width: 16px; height: 16px; cursor: pointer;"
        ))
      }
    )
  }

  # Badge column
  if (!is.null(config$badge_col)) {
    badge_fn <- config$badge_render_fn %||%
      function(row_key, row_label) {
        sprintf("<span>%s</span>", htmltools::htmlEscape(row_label))
      }
    col_defs$.row_label <- reactable::colDef(
      name = config$badge_label %||% "Label",
      width = 76,
      cell = function(value, index) {
        gk <- tbl$.row_key[index]
        badge_fn(gk, value)
      },
      html = TRUE
    )
  } else {
    col_defs$.row_label <- reactable::colDef(show = FALSE)
  }

  # Year column
  if (!is.null(config$year_col)) {
    # Check if the year toggle key exists in gear_toggles; if so, respect it.
    # The config author names the toggle (e.g. "showSchoolYear"); we look it
    # up in the live settings to decide visibility.
    year_toggle_key <- config$year_toggle %||% NULL
    if (is.null(year_toggle_key) && !is.null(config$gear_toggles)) {
      # Auto-detect: look for toggle keys containing "year" (case-insensitive)
      candidates <- grep(
        "year",
        names(config$gear_toggles),
        ignore.case = TRUE,
        value = TRUE
      )
      if (length(candidates) == 1L) year_toggle_key <- candidates[1L]
    }

    year_visible <- if (!is.null(year_toggle_key)) {
      isTRUE(settings[[year_toggle_key]])
    } else {
      TRUE
    }

    col_defs[[config$year_col]] <- if (year_visible) {
      reactable::colDef(
        name = "School Year",
        width = 110,
        cell = function(value, index) {
          gk <- tbl$.row_key[index]
          val <- if (!is.na(value)) value else ""
          sprintf(
            '<input type="number" class="cell-input" value="%s"
              min="%d" max="%d"
              onchange="Shiny.setInputValue(\'%s\', {grade:\'%s\', value: this.value === \'\' ? null : parseInt(this.value)}, {priority:\'event\'})" />',
            val,
            config$year_range[1],
            config$year_range[2],
            ns("year_change"),
            gk
          )
        },
        html = TRUE
      )
    } else {
      reactable::colDef(show = FALSE)
    }
  }

  # Widget columns — always include ALL columns, including gear-toggled
  # ones. Column visibility is handled by CSS classes on the container
  # div (e.g. .hide-col-showHomeschool), not by omitting the column from
  # the reactable definition. This lets us toggle column visibility
  # without a full re-render.
  widget_defs <- purrr::map(config$columns, function(cs) {
    .build_widget_coldef(cs, ns, current_rows, settings, tbl, config)
  }) |>
    purrr::set_names(purrr::map_chr(config$columns, "id"))

  col_defs <- c(col_defs, widget_defs)

  col_defs
}


#' Wrap a picker cell's HTML so it can be toggled between its active
#' widget and a mutual-exclusion display without a table re-render.
#'
#' Emits:
#'   <div class="rp-cell-wrap" data-rp-cell="{ns_key}" data-rp-displaced="{init}">
#'     <div class="rp-cell-active">{widget_html}</div>
#'     <div class="rp-cell-displaced">{displaced_html}</div>
#'   </div>
#'
#' If there is no me_rule for this cell, the displaced div is empty
#' and data-rp-displaced is always "false" — the wrap is still emitted
#' for consistency but has no visual effect.
#'
#' @param widget_html Character. Pre-rendered active widget HTML.
#' @param me_rules   List of mutual-exclusion rules targeting this column.
#' @param row        Current row state (list).
#' @param ns_cell_key Character. Namespaced cell key, e.g. "history-school-PK".
#' @return Character HTML for the wrapped cell.
#' @noRd
.wrap_cell <- function(widget_html, me_rules, row, ns_cell_key) {
  active_rule <- purrr::detect(me_rules, ~ !is.null(row[[.x$when_on]]))
  is_displaced <- !is.null(active_rule)
  displaced_html <- if (is_displaced) {
    active_rule$display %||% ""
  } else if (length(me_rules) > 0L) {
    # Pre-render the first rule's display so JS can flip into it later
    me_rules[[1]]$display %||% ""
  } else {
    ""
  }

  sprintf(
    paste0(
      '<div class="rp-cell-wrap" data-rp-cell="%s" data-rp-displaced="%s">',
      '<div class="rp-cell-active">%s</div>',
      '<div class="rp-cell-displaced">%s</div>',
      "</div>"
    ),
    htmltools::htmlEscape(ns_cell_key, attribute = TRUE),
    if (is_displaced) "true" else "false",
    widget_html,
    displaced_html
  )
}


#' Build a single colDef for a widget column.
#' @noRd
.build_widget_coldef <- function(cs, ns, current_rows, settings, tbl, config) {
  col_def_args <- list(
    name = cs$label,
    html = TRUE
  )

  if (!is.null(cs$width)) {
    col_def_args$width <- cs$width
  }
  if (!is.null(cs$min_width)) {
    col_def_args$minWidth <- cs$min_width
  }

  # Find mutual exclusion rules where this column is the *cleared* target
  me_rules <- Filter(
    function(rule) rule$clears == cs$id,
    config$interactions$mutual_exclusion %||% list()
  )

  col_def_args$cell <- switch(
    cs$type,

    search_picker = function(value, index) {
      gk <- tbl$.row_key[index]
      row <- current_rows[[gk]]
      if (!is.list(row)) {
        row <- list()
      }

      opts <- cs$options
      widget_html <- as.character(searchPickerInput(
        inputId = ns(paste0(cs$id, "_", gk)),
        value = row[[cs$id]],
        show_nces_id = isTRUE(
          settings$showNCESId %||% opts$show_nces_id %||% TRUE
        ),
        grade_label = config$label_map[[gk]],
        grade_key = gk,
        ns = ns(""),
        trigger_label = opts$trigger_label %||% "+ Pick school",
        popover_title = opts$popover_title %||% "Find school",
        search_placeholder = opts$search_placeholder,
        empty_hint = opts$empty_hint,
        no_match_hint = opts$no_match_hint,
        show_fill_down = opts$show_fill_down %||% TRUE
      ))
      .wrap_cell(
        widget_html,
        me_rules,
        row,
        ns_cell_key = ns(paste0(cs$id, "-", gk))
      )
    },

    attendance_picker = function(value, index) {
      gk <- tbl$.row_key[index]
      row <- current_rows[[gk]]
      if (!is.list(row)) {
        row <- list()
      }

      opts <- cs$options
      widget_html <- as.character(attendancePickerInput(
        inputId = ns(paste0(cs$id, "_", gk)),
        value = row[[cs$id]],
        grade_label = config$label_map[[gk]],
        sections = opts$sections,
        trigger_label = opts$trigger_label,
        popover_title = opts$popover_title,
        show_notes = opts$show_notes %||% TRUE,
        notes_placeholder = opts$notes_placeholder
      ))
      .wrap_cell(
        widget_html,
        me_rules,
        row,
        ns_cell_key = ns(paste0(cs$id, "-", gk))
      )
    },

    homeschool_picker = function(value, index) {
      gk <- tbl$.row_key[index]
      row <- current_rows[[gk]]
      if (!is.list(row)) {
        row <- list()
      }

      opts <- cs$options
      widget_html <- as.character(homeschoolPickerInput(
        inputId = ns(paste0(cs$id, "_", gk)),
        value = row[[cs$id]],
        grade_label = config$label_map[[gk]],
        grade_key = gk,
        ns = ns(""),
        providers = opts$providers,
        provider_label = opts$provider_label,
        curriculum_label = opts$curriculum_label,
        curriculum_placeholder = opts$curriculum_placeholder,
        show_curriculum = opts$show_curriculum %||% TRUE,
        show_notes = opts$show_notes %||% TRUE,
        notes_placeholder = opts$notes_placeholder,
        trigger_label = opts$trigger_label,
        trigger_sub_label = opts$trigger_sub_label,
        popover_title = opts$popover_title,
        popover_title_sub = opts$popover_title_sub,
        filled_pill_label = opts$filled_pill_label,
        clear_label = opts$clear_label
      ))
      .wrap_cell(
        widget_html,
        me_rules,
        row,
        ns_cell_key = ns(paste0(cs$id, "-", gk))
      )
    },

    notes_input = function(value, index) {
      gk <- tbl$.row_key[index]
      row <- current_rows[[gk]]
      if (!is.list(row)) {
        row <- list()
      }

      opts <- cs$options
      default_placeholder <- opts$placeholder %||% "Optional"

      # Dynamic placeholder from mutual exclusion
      me_active <- purrr::some(
        config$interactions$mutual_exclusion %||% list(),
        ~ !is.null(row[[.x$when_on]])
      )
      placeholder <- if (me_active && !is.null(opts$alt_placeholder)) {
        opts$alt_placeholder
      } else {
        default_placeholder
      }

      note_val <- row[[cs$id]]
      if (is.null(note_val)) {
        note_val <- ""
      }

      as.character(notesInput(
        inputId = ns(paste0(cs$id, "_", gk)),
        value = note_val,
        placeholder = placeholder
      ))
    },

    custom = function(value, index) {
      if (!is.null(cs$render_cell_fn)) {
        gk <- tbl$.row_key[index]
        row <- current_rows[[gk]]
        cs$render_cell_fn(ns, gk, row, cs, settings)
      } else {
        ""
      }
    },

    # ── Primitive input types ──────────────────────────────────────────────

    dropdown = function(value, index) {
      gk <- tbl$.row_key[index]
      row <- current_rows[[gk]]
      current_val <- if (is.list(row)) row[[cs$id]] else NULL
      has_value <- !is.null(current_val) && !is.na(current_val)
      locked <- !.is_gate_open(cs$gate, if (is.list(row)) row else list())

      opts <- cs$options
      input_id <- ns(paste0(cs$id, "_", gk))
      la <- .locked_attrs(locked, "width: 100%; padding: 4px;")

      placeholder_tag <- htmltools::tags$option(
        value = "",
        disabled = "disabled",
        selected = if (!has_value) "selected" else NULL,
        opts$placeholder %||% "-- Select --"
      )

      choice_tags <- purrr::map(opts$choices, function(choice) {
        is_match <- has_value &&
          identical(as.character(current_val), as.character(choice$value))
        htmltools::tags$option(
          value = choice$value,
          selected = if (is_match) "selected" else NULL,
          choice$label
        )
      })

      as.character(htmltools::tags$select(
        id = input_id,
        onchange = sprintf(
          "Shiny.setInputValue('%s', this.value, {priority: 'event'});",
          input_id
        ),
        disabled = la$disabled,
        style = la$style,
        placeholder_tag,
        choice_tags
      ))
    },

    numeric = function(value, index) {
      gk <- tbl$.row_key[index]
      row <- current_rows[[gk]]
      current_val <- if (is.list(row)) row[[cs$id]] else NULL
      locked <- !.is_gate_open(cs$gate, if (is.list(row)) row else list())

      opts <- cs$options
      initial <- if (is.null(current_val) || is.na(current_val)) {
        opts$min %||% 0
      } else {
        current_val
      }
      input_id <- ns(paste0(cs$id, "_", gk))
      la <- .locked_attrs(locked, "width: 100%; padding: 4px;")

      as.character(htmltools::tags$input(
        id = input_id,
        type = "number",
        value = initial,
        min = opts$min,
        max = opts$max,
        step = opts$step,
        disabled = la$disabled,
        oninput = sprintf(
          "Shiny.setInputValue('%s', parseFloat(this.value), {priority: 'event'});",
          input_id
        ),
        style = la$style
      ))
    },

    date = function(value, index) {
      gk <- tbl$.row_key[index]
      row <- current_rows[[gk]]
      current_val <- if (is.list(row)) row[[cs$id]] else NULL
      locked <- !.is_gate_open(cs$gate, if (is.list(row)) row else list())

      opts <- cs$options
      initial <- if (is.null(current_val) || is.na(current_val)) {
        ""
      } else {
        format(as.Date(current_val), "%Y-%m-%d")
      }
      input_id <- ns(paste0(cs$id, "_", gk))
      la <- .locked_attrs(locked, "width: 100%; padding: 4px;")

      as.character(htmltools::tags$input(
        id = input_id,
        type = "date",
        value = initial,
        min = opts$min_date,
        max = opts$max_date,
        disabled = la$disabled,
        onchange = sprintf(
          "Shiny.setInputValue('%s', this.value, {priority: 'event'});",
          input_id
        ),
        style = la$style
      ))
    },

    checkbox = function(value, index) {
      gk <- tbl$.row_key[index]
      row <- current_rows[[gk]]
      is_checked <- isTRUE(if (is.list(row)) row[[cs$id]] else FALSE)
      locked <- !.is_gate_open(cs$gate, if (is.list(row)) row else list())

      input_id <- ns(paste0(cs$id, "_", gk))
      la <- .locked_attrs(locked, "width: 20px; height: 20px;")

      as.character(htmltools::tags$input(
        id = input_id,
        type = "checkbox",
        checked = if (is_checked) "checked" else NULL,
        disabled = la$disabled,
        onchange = sprintf(
          "Shiny.setInputValue('%s', this.checked, {priority: 'event'});",
          input_id
        ),
        style = la$style
      ))
    },

    toggle = function(value, index) {
      gk <- tbl$.row_key[index]
      row <- current_rows[[gk]]
      is_on <- isTRUE(if (is.list(row)) row[[cs$id]] else FALSE)
      locked <- !.is_gate_open(cs$gate, if (is.list(row)) row else list())

      input_id <- ns(paste0(cs$id, "_", gk))
      state_id <- input_id
      btn_id <- paste0(input_id, "_btn")

      btn_style <- paste0(
        "padding: 3px 12px; border-radius: 12px; border: none; ",
        "background-color: ",
        if (is_on) "#28a745" else "#6c757d",
        "; ",
        "color: white; font-size: 0.85em; ",
        if (locked) {
          "opacity: 0.4; cursor: not-allowed; pointer-events: none;"
        } else {
          "cursor: pointer;"
        }
      )

      as.character(htmltools::tagList(
        htmltools::tags$input(
          id = state_id,
          type = "hidden",
          value = tolower(as.character(is_on))
        ),
        htmltools::tags$button(
          id = btn_id,
          type = "button",
          disabled = if (locked) "disabled" else NULL,
          style = btn_style,
          onclick = if (!locked) {
            sprintf(
              "(function() {
               var s = document.getElementById('%s');
               var b = document.getElementById('%s');
               var newVal = s.value !== 'true';
               s.value = newVal;
               b.textContent = newVal ? 'On' : 'Off';
               b.style.backgroundColor = newVal ? '#28a745' : '#6c757d';
               Shiny.setInputValue('%s', newVal, {priority: 'event'});
             })();",
              state_id,
              btn_id,
              input_id
            )
          } else {
            NULL
          },
          if (is_on) "On" else "Off"
        )
      ))
    },

    text = function(value, index) {
      gk <- tbl$.row_key[index]
      row <- current_rows[[gk]]
      current_val <- if (is.list(row)) row[[cs$id]] else NULL
      locked <- !.is_gate_open(cs$gate, if (is.list(row)) row else list())

      opts <- cs$options
      initial <- if (is.null(current_val) || is.na(current_val)) {
        ""
      } else {
        as.character(current_val)
      }
      input_id <- ns(paste0(cs$id, "_", gk))
      la <- .locked_attrs(locked, "width: 100%; padding: 4px;")

      as.character(htmltools::tags$input(
        id = input_id,
        type = "text",
        value = initial,
        maxlength = opts$max_chars,
        placeholder = opts$placeholder,
        disabled = la$disabled,
        oninput = sprintf(
          "Shiny.setInputValue('%s', this.value, {priority: 'event'});",
          input_id
        ),
        style = la$style
      ))
    },

    # Fallback
    function(value, index) as.character(value)
  )

  # Add CSS class markers for gear-toggled columns so the generated
  # .hide-col-{toggle} CSS rule can target both header and body cells.
  if (!is.null(cs$gear_toggle)) {
    gear_class <- paste0("gear-col-", cs$gear_toggle)
    col_def_args$class <- gear_class
    col_def_args$headerClass <- gear_class
  }

  do.call(reactable::colDef, col_def_args)
}
