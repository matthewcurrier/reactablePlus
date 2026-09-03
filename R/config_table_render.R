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

# ── Click-to-select helper ───────────────────────────────────────────────────
# Inline JS that finds the selection checkbox in the same row and
# toggles it. Used by badge and display column cells when
# config$click_to_select is TRUE.

.click_to_select_js <- paste0(
  "(function(e){",
  "var row=e.target.closest('.rt-tr');",
  "if(row){var cb=row.querySelector('input[type=checkbox]');",
  "if(cb){cb.checked=!cb.checked;",
  "cb.dispatchEvent(new Event('change'));}}",
  "e.stopPropagation();",
  "})(event)"
)

#' Wrap an HTML string in a click-to-select container.
#'
#' Returns the content inside a `<div>` with pointer cursor and an
#' onclick handler that toggles the row's selection checkbox. The
#' wrapper is a block-level element with its own padding, intended to
#' be placed inside a cell whose native padding has been zeroed out
#' via `.click_to_select_cell_style`. This avoids any assumptions
#' about the table's CSS.
#'
#' @param html_content Character. The HTML to wrap.
#' @return Character. The wrapped HTML string.
#' @noRd
.wrap_click_to_select <- function(html_content) {
  sprintf(
    paste0(
      '<div style="cursor:pointer;user-select:none;',
      'padding:8px 12px;" ',
      'onclick="%s">%s</div>'
    ),
    .click_to_select_js,
    html_content
  )
}

#' Cell style applied to colDefs that use click-to-select.
#'
#' Zeroes out the cell's native padding so `.wrap_click_to_select()`'s
#' own padding becomes the sole source. This means the clickable area
#' fills the entire cell regardless of the table's theme or custom CSS.
#'
#' @noRd
.click_to_select_cell_style <- list(padding = "0")


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


# Popover picker types that also report through the per-column store. They
# carry a structured-list value (not a scalar) and render via .store_wrap()
# around the picker widget, with data-rp-type="picker" so the JS type
# registry (reactable-extras.js) reads/writes them via the popover instance.
.PICKER_STORE_TYPES <- c(
  "search_picker", "attendance_picker", "homeschool_picker"
)

# Widget types rendered as store-backed *_extra cells: the value flows
# back through the per-column reactable-extras store rather than a per-cell
# Shiny input. Keep in sync with the primitive renderers in
# .build_widget_coldef() and the cell bricks in R/cell-extras.R.
.STORE_TYPES <- c(
  "dropdown", "numeric", "date", "checkbox", "toggle", "text", "notes_input",
  .PICKER_STORE_TYPES
)


#' The value the *_extra store/DOM expects for a column's current state.
#'
#' Mirrors each primitive renderer's initial-value logic so the server can
#' push authoritative values into the store (via [update_extra()]) that match
#' what a fresh render would show.
#' @noRd
.store_display_value <- function(cs, row_state) {
  v <- if (is.list(row_state)) row_state[[cs$id]] else NULL
  # Pickers carry a structured list (or NULL when off). The JS picker
  # write handler applies it via the popover's setValue as-is, so pass it
  # through untouched — update_extra() serializes the list to the client.
  if (cs$type %in% .PICKER_STORE_TYPES) {
    return(v)
  }
  empty <- is.null(v) || (length(v) == 1L && is.na(v))
  switch(
    cs$type,
    numeric = if (empty) (cs$options$min %||% 0) else v,
    checkbox = isTRUE(v),
    toggle = isTRUE(v),
    date = if (empty) "" else format(as.Date(v), "%Y-%m-%d"),
    dropdown = if (empty) "" else as.character(v)[1L],
    # text, notes_input
    if (empty) "" else as.character(v)[1L]
  )
}


#' Is a cell currently "empty" (eligible to receive a fill-down)?
#'
#' A cell is empty when it is NULL or holds the column's `empty_value`
#' (or an NA scalar). Picker columns use `empty_value = NULL`, so this
#' reduces to the historical `is.null()` check for them.
#' @noRd
.is_cell_empty <- function(cs, v) {
  if (is.null(v)) {
    return(TRUE)
  }
  if (length(v) == 1L && is.na(v)) {
    return(TRUE)
  }
  !is.null(cs$empty_value) && identical(v, cs$empty_value)
}


#' Wrap a store cell's HTML with a "fill down" link.
#'
#' Used for the column configured as the `fill_down` target so store-backed
#' primitives get the same cascade affordance pickers have. The link sends
#' `{from_row}` to the fill-down input; the server reads the source value
#' from its own state and cascades it. Quotes are written as numeric HTML
#' entities so the inline handler survives reactable's HTML rendering.
#' @noRd
.with_fill_link <- function(cell_html, fd, gk, ns) {
  fd_input_name <- fd$input_name %||% paste0(fd$column, "_fill_down")
  input_id <- ns(fd_input_name)
  link <- sprintf(
    paste0(
      "<a href=\"#\" class=\"rp-fill-down\" title=\"Fill value down\" ",
      "onclick=\"Shiny.setInputValue(&#39;%s&#39;, {from_row: &#39;%s&#39;}, ",
      "{priority: &#39;event&#39;}); event.preventDefault();\">&#8615;</a>"
    ),
    input_id,
    gk
  )
  paste0('<span class="rp-fill-wrap">', cell_html, link, "</span>")
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
#'
#' @param config A `table_config` object.
#' @param current_rows Named list of row states.
#' @param settings Current gear settings.
#' @param effective_keys Optional character vector. When non-NULL, used
#'   instead of `config$row_keys` for ordering and scoping rows. Also
#'   subsets `current_rows` to only these keys (important in dynamic
#'   mode where `current_rows` may contain departed rows).
#' @param effective_labels Optional character vector. When non-NULL,
#'   used instead of `config$row_labels`.
#' @param source_snapshot Optional data frame. The current
#'   `source_data()` snapshot, used to extract display column values
#'   in dynamic mode. Pass `NULL` in static mode.
#'
#' @noRd
.build_table_df <- function(
  config,
  current_rows,
  settings,
  effective_keys = NULL,
  effective_labels = NULL,
  source_snapshot = NULL
) {
  row_keys <- effective_keys %||% config$row_keys
  row_labels <- effective_labels %||% config$row_labels
  n <- length(row_keys)

  # Scope current_rows to only visible keys (in dynamic mode,
  # current_rows may contain state for departed rows).
  visible_rows <- current_rows[row_keys]

  # Start with row keys and labels
  tbl <- data.frame(
    .row_key = row_keys,
    .row_label = row_labels,
    stringsAsFactors = FALSE
  )

  # Selection column
  if (isTRUE(config$selectable)) {
    tbl$.selected <- vapply(
      visible_rows,
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
      visible_rows,
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

  # Display columns (read-only values from source_data)
  if (!is.null(config$display_cols) && !is.null(source_snapshot)) {
    src_ids <- as.character(source_snapshot[[config$row_id_col]])
    match_idx <- match(row_keys, src_ids)

    purrr::walk(config$display_cols, function(dc) {
      raw_vals <- source_snapshot[[dc$id]]
      tbl[[dc$id]] <<- if (!is.null(raw_vals)) {
        as.character(raw_vals[match_idx])
      } else {
        rep(NA_character_, n)
      }
    })
  }

  # Column values — primitive types get real values (enables sorting/filtering),
  # complex widget types get empty placeholders (cell fns render from current_rows).
  # Always include ALL columns (gear-toggled visibility is handled by CSS,
  # not by omitting from the data frame).

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
        acc[[cs$id]] <- .extract_col_values(visible_rows, cs)
      } else {
        acc[[cs$id]] <- rep("", n)
      }
      acc
    },
    .init = tbl
  )

  # Delete column placeholder (appendable mode with allow_delete)
  if (isTRUE(config$appendable) && isTRUE(config$allow_delete)) {
    tbl$.delete <- rep("", n)
  }

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
#'
#' @param effective_keys Optional character vector of current row keys.
#' @param effective_labels Optional character vector of current labels.
#' @param source_snapshot Optional data frame. The current
#'   `source_data()` snapshot, used for display column rendering.
#'
#' @noRd
.build_col_defs <- function(
  config,
  ns,
  current_rows,
  settings,
  tbl,
  effective_keys = NULL,
  effective_labels = NULL,
  source_snapshot = NULL
) {
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
  use_click_select <- isTRUE(config$click_to_select)

  if (!is.null(config$badge_col)) {
    badge_fn <- config$badge_render_fn %||%
      function(row_key, row_label) {
        sprintf("<span>%s</span>", htmltools::htmlEscape(row_label))
      }

    badge_args <- list(
      name = config$badge_label %||% "Label",
      width = 76,
      cell = function(value, index) {
        gk <- tbl$.row_key[index]
        content <- badge_fn(gk, value)
        if (use_click_select) .wrap_click_to_select(content) else content
      },
      html = TRUE
    )
    if (use_click_select) {
      badge_args$style <- .click_to_select_cell_style
    }

    col_defs$.row_label <- do.call(reactable::colDef, badge_args)
  } else {
    col_defs$.row_label <- reactable::colDef(show = FALSE)
  }

  # Display columns (read-only, from source_data)
  if (!is.null(config$display_cols)) {
    purrr::walk(config$display_cols, function(dc) {
      col_args <- list(
        name = dc$label,
        sortable = FALSE
      )
      if (!is.null(dc$width)) {
        col_args$width <- dc$width
      }
      if (!is.null(dc$min_width)) {
        col_args$minWidth <- dc$min_width
      }

      has_render <- !is.null(dc$render_fn)
      needs_click <- use_click_select

      if (has_render || needs_click) {
        col_args$html <- TRUE
        col_args$cell <- function(value, index) {
          gk <- tbl$.row_key[index]
          content <- if (has_render) {
            dc$render_fn(value, gk)
          } else {
            htmltools::htmlEscape(as.character(value %||% ""))
          }
          if (needs_click) .wrap_click_to_select(content) else content
        }
      }
      if (needs_click) {
        col_args$style <- .click_to_select_cell_style
      }

      col_defs[[dc$id]] <<- do.call(reactable::colDef, col_args)
    })
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

  # Delete column (appendable mode with allow_delete)
  if (isTRUE(config$appendable) && isTRUE(config$allow_delete)) {
    col_defs$.delete <- reactable::colDef(
      name = "",
      width = 50L,
      sortable = FALSE,
      align = "center",
      html = TRUE,
      cell = function(value, index) {
        gk <- tbl$.row_key[index]
        as.character(htmltools::tags$button(
          class = "btn btn-link btn-sm rp-delete-row-btn",
          style = paste0(
            "color: #dc3545; padding: 2px 6px; ",
            "font-size: 1.1em; line-height: 1;"
          ),
          title = "Delete row",
          onclick = sprintf(
            "Shiny.setInputValue('%s', {key:'%s', nonce:Math.random()}, {priority:'event'});",
            ns(".delete_row"),
            gk
          ),
          htmltools::HTML("&times;")
        ))
      }
    )
  }

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


#' Wrap a picker cell so it reports through the per-column *_extra store.
#'
#' Emits an `.rp-extra` div carrying the store coordinates around the
#' picker's (already mutual-exclusion-wrapped) HTML:
#'
#'   <div class="rp-extra" data-input-id="{ns(col)}" data-row="{gk}"
#'        data-rp-type="picker">{inner_html}</div>
#'
#' The JS "picker" type (reactable-extras.js) reads/writes the value via the
#' inner picker's popover instance, so the whole column collects under one
#' Shiny input (`input[[col]]`) keyed by row key, exactly like the primitive
#' `*_extra` cells. The picker binding still lives inside and renders the
#' widget; this wrapper only adds the store plumbing.
#'
#' @param inner_html Character. The picker cell HTML (typically the output
#'   of `.wrap_cell()`).
#' @param input_id Character. Namespaced column input id, e.g. `ns(cs$id)`.
#' @param row Character. The row key placed in `data-row`.
#' @return Character HTML for the store-wrapped picker cell.
#' @noRd
.store_wrap <- function(inner_html, input_id, row) {
  sprintf(
    paste0(
      '<div class="rp-extra" data-input-id="%s" data-row="%s" ',
      'data-rp-type="picker">%s</div>'
    ),
    htmltools::htmlEscape(input_id, attribute = TRUE),
    htmltools::htmlEscape(row, attribute = TRUE),
    inner_html
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
        show_fill_down = opts$show_fill_down %||% TRUE,
        show_fill_up = opts$show_fill_up %||% FALSE
      ))
      .store_wrap(
        .wrap_cell(
          widget_html,
          me_rules,
          row,
          ns_cell_key = ns(paste0(cs$id, "-", gk))
        ),
        input_id = ns(cs$id),
        row = gk
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
      .store_wrap(
        .wrap_cell(
          widget_html,
          me_rules,
          row,
          ns_cell_key = ns(paste0(cs$id, "-", gk))
        ),
        input_id = ns(cs$id),
        row = gk
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
      .store_wrap(
        .wrap_cell(
          widget_html,
          me_rules,
          row,
          ns_cell_key = ns(paste0(cs$id, "-", gk))
        ),
        input_id = ns(cs$id),
        row = gk
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

      .extra_value_cell(
        ns(cs$id), "text", "text", note_val, gk,
        attrs = list(placeholder = placeholder),
        class = "cell-input"
      )
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

    # Primitive inputs render as store-backed *_extra cells: the cell HTML
    # carries data-input-id = ns(col_id) and data-row = row key, and the
    # shared reactable-extras.js store collects the whole column under one
    # Shiny input. See R/cell-extras.R for the bricks and the value contract.

    dropdown = function(value, index) {
      gk <- tbl$.row_key[index]
      row <- current_rows[[gk]]
      current_val <- if (is.list(row)) row[[cs$id]] else NULL
      locked <- !.is_gate_open(cs$gate, if (is.list(row)) row else list())

      opts <- cs$options
      # Resolve choices: dynamic choices_fn takes precedence over static
      resolved_choices <- if (!is.null(opts$choices_fn) && is.list(row)) {
        tryCatch(
          normalize_choices(opts$choices_fn(row)),
          error = function(e) opts$choices %||% list()
        )
      } else {
        opts$choices %||% list()
      }

      .extra_select_cell(
        ns(cs$id), resolved_choices, current_val, gk,
        placeholder = opts$placeholder %||% "-- Select --",
        attrs = .locked_attrs(locked, "width: 100%; padding: 4px;")
      )
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

      .extra_value_cell(
        ns(cs$id), "numeric", "number", initial, gk,
        attrs = c(
          purrr::compact(list(min = opts$min, max = opts$max, step = opts$step)),
          .locked_attrs(locked, "width: 100%; padding: 4px;")
        )
      )
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

      .extra_value_cell(
        ns(cs$id), "date", "date", initial, gk,
        attrs = c(
          purrr::compact(list(min = opts$min_date, max = opts$max_date)),
          .locked_attrs(locked, "width: 100%; padding: 4px;")
        )
      )
    },

    checkbox = function(value, index) {
      gk <- tbl$.row_key[index]
      row <- current_rows[[gk]]
      is_checked <- isTRUE(if (is.list(row)) row[[cs$id]] else FALSE)
      locked <- !.is_gate_open(cs$gate, if (is.list(row)) row else list())

      .extra_checkbox_cell(
        ns(cs$id), is_checked, gk,
        attrs = .locked_attrs(locked, "width: 20px; height: 20px;")
      )
    },

    toggle = function(value, index) {
      gk <- tbl$.row_key[index]
      row <- current_rows[[gk]]
      is_on <- isTRUE(if (is.list(row)) row[[cs$id]] else FALSE)
      locked <- !.is_gate_open(cs$gate, if (is.list(row)) row else list())

      lock_attrs <- if (locked) {
        list(
          disabled = "disabled",
          style = "opacity: 0.4; cursor: not-allowed; pointer-events: none;"
        )
      } else {
        list()
      }

      .extra_toggle_cell(ns(cs$id), is_on, gk, attrs = lock_attrs)
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

      .extra_value_cell(
        ns(cs$id), "text", "text", initial, gk,
        attrs = c(
          purrr::compact(list(
            maxlength = opts$max_chars,
            placeholder = opts$placeholder
          )),
          .locked_attrs(locked, "width: 100%; padding: 4px;")
        )
      )
    },

    # Fallback
    function(value, index) as.character(value)
  )

  # Fill-down affordance: when this store-backed column is the fill_down
  # target, append a "fill down" link to each cell (pickers render their
  # own link inside the popover, so they are excluded here).
  fd <- config$interactions$fill_down
  if (
    !is.null(fd) &&
      identical(fd$column, cs$id) &&
      cs$type %in% .STORE_TYPES &&
      !cs$type %in% .PICKER_STORE_TYPES
  ) {
    inner_cell <- col_def_args$cell
    col_def_args$cell <- function(value, index) {
      gk <- tbl$.row_key[index]
      .with_fill_link(inner_cell(value, index), fd, gk, ns)
    }
  }

  # Add CSS class markers for gear-toggled columns so the generated
  # .hide-col-{toggle} CSS rule can target both header and body cells.
  if (!is.null(cs$gear_toggle)) {
    gear_class <- paste0("gear-col-", cs$gear_toggle)
    col_def_args$class <- gear_class
    col_def_args$headerClass <- gear_class
  }

  do.call(reactable::colDef, col_def_args)
}
