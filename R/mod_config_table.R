# =============================================================================
# config-table.R
#
# Config-driven editable-table Shiny module driven by a table_config object.
# The module does NOT know what domain it serves — all domain logic
# (columns, data marshaling, toolbar stats) comes from the config.
#
# SECTIONS convention:
#   server signature: function(id, config, data_r, search_fn)
#   return value:     list(get_data = reactive)
# =============================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

#' Config-driven Editable Table UI
#'
#' UI function for the config-driven editable table module. Works with
#' `table_config()` objects and picker widgets.
#' @param id Module namespace ID.
#' @param config A `table_config` object.
#' @return A tagList with dependencies, toolbar, and reactable output.
#' @export
config_table_ui <- function(id, config) {
  ns <- shiny::NS(id)

  has_toolbar <- !is.null(config$toolbar_stats_fn) ||
    !is.null(config$gear_toggles) ||
    isTRUE(config$show_reset)

  toolbar <- if (has_toolbar) {
    shiny::tags$div(
      class = "panel-toolbar",
      style = "display: flex; align-items: center; gap: 8px;",
      if (!is.null(config$toolbar_stats_fn)) {
        shiny::tags$span(
          class = "toolbar-stat",
          shiny::uiOutput(ns("toolbar_stats"), inline = TRUE)
        )
      },
      if (isTRUE(config$show_reset)) {
        shiny::actionButton(
          ns("reset"),
          "Reset",
          class = "btn btn-warning btn-sm",
          style = "margin-left: auto;"
        )
      },
      if (!is.null(config$gear_toggles)) {
        shiny::tags$div(
          style = if (isTRUE(config$show_reset)) NULL else "margin-left: auto;",
          gearPopoverInput(
            inputId = ns("gear"),
            toggles = config$gear_toggles
          )
        )
      }
    )
  }

  # Columns that are gear-toggled: generate CSS rules + initial hide
  # classes so column visibility can be toggled without a full re-render.
  gear_toggled_cols <- purrr::keep(config$columns, ~ !is.null(.x$gear_toggle))

  gear_css <- if (length(gear_toggled_cols) > 0L) {
    rules <- purrr::map_chr(gear_toggled_cols, function(cs) {
      sprintf(
        ".hide-col-%s .gear-col-%s { display: none !important; }",
        cs$gear_toggle,
        cs$gear_toggle
      )
    })
    shiny::tags$style(shiny::HTML(paste(rules, collapse = "\n")))
  }

  initial_hide_classes <- purrr::map_chr(
    purrr::keep(gear_toggled_cols, function(cs) {
      !isTRUE(config$gear_toggles[[cs$gear_toggle]]$value)
    }),
    function(cs) paste0("hide-col-", cs$gear_toggle)
  )

  container_class <- paste(
    c("rp-table-container", initial_hide_classes),
    collapse = " "
  )

  shiny::tagList(
    useReactablePlus(),
    gear_css,
    bslib::card(
      bslib::card_body(
        toolbar,
        shiny::tags$div(
          id = ns("table-container"),
          class = container_class,
          reactable::reactableOutput(ns("table"))
        )
      )
    )
  )
}


# ── Server ───────────────────────────────────────────────────────────────────

#' Config-driven Editable Table Server
#'
#' Server function for the config-driven editable table module. Pairs
#' with `config_table_ui()`. Uses `table_config()` objects to drive
#' picker widget rendering, state management, and cross-column
#' interactions.
#' @param id Module namespace ID.
#' @param config A `table_config` object.
#' @param data_r Reactive returning `NULL` (new/add mode) or a data frame
#'   (edit mode with saved rows).
#' @param search_fn A function `(query, limit)` → data.frame for
#'   server-side typeahead search. Required when any column uses
#'   `search_picker` type. Default `NULL`.
#' @return `list(get_data = reactive)`.
#' @export
config_table_server <- function(
  id,
  config,
  data_r = shiny::reactive(NULL),
  search_fn = NULL
) {
  stopifnot(inherits(config, "table_config"))
  stopifnot(shiny::is.reactive(data_r))

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ROW_KEYS <- config$row_keys
    ROW_LABELS <- config$row_labels
    LABEL_MAP <- config$label_map
    N_ROWS <- length(ROW_KEYS)

    # ── State ──────────────────────────────────────────────────────────────
    rows <- shiny::reactiveVal(.build_empty_rows(config))
    render_key <- shiny::reactiveVal(0L)

    # Seed from saved data
    saved <- shiny::isolate(data_r())
    if (!is.null(saved) && is.data.frame(saved) && nrow(saved) > 0L) {
      rows(.rows_from_saved(config, saved))
    }

    # ── Search wiring ────────────────────────────────────────────────────
    if (!is.null(config$search_fn_col) && !is.null(search_fn)) {
      useTypeaheadSearch(input, session, search_fn = search_fn)
    }

    # ── Context switch ───────────────────────────────────────────────────
    shiny::observeEvent(
      data_r(),
      {
        saved_data <- data_r()
        if (
          is.null(saved_data) ||
            !is.data.frame(saved_data) ||
            nrow(saved_data) == 0L
        ) {
          rows(.build_empty_rows(config))
        } else {
          rows(.rows_from_saved(config, saved_data))
        }
        render_key(render_key() + 1L)
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )

    # ── Gear ─────────────────────────────────────────────────────────────
    gear <- shiny::reactive({
      g <- input$gear
      if (is.null(g) && !is.null(config$gear_toggles)) {
        # Build defaults from config
        defaults <- lapply(config$gear_toggles, function(t) isTRUE(t$value))
        names(defaults) <- names(config$gear_toggles)
        defaults
      } else {
        g
      }
    })

    # Only re-render when gear values actually change — not on the
    # initial sync where the client sends values identical to the
    # computed defaults. The observeEvent pattern fires on any
    # reactive invalidation regardless of value equality, which
    # causes a spurious re-render ~1 s after page load that destroys
    # any popover the user opened in that window.
    #
    # Column-visibility toggles (those referenced by a column's
    # gear_toggle field) are handled via CSS class toggle on the
    # container div — no re-render needed. Content/style toggles
    # (e.g. showNCESId, compactRows) still increment render_key.
    col_vis_toggles <- unique(purrr::map_chr(
      purrr::keep(config$columns, ~ !is.null(.x$gear_toggle)),
      "gear_toggle"
    ))

    prev_gear <- shiny::reactiveVal(NULL)
    shiny::observe({
      g <- gear()
      old_g <- shiny::isolate(prev_gear())
      if (!is.null(old_g) && !identical(old_g, g)) {
        changed_keys <- names(g)[
          !purrr::map2_lgl(g[names(g)], old_g[names(g)], identical)
        ]

        # Column-visibility toggles → CSS class on container (no flicker)
        vis_changed <- intersect(changed_keys, col_vis_toggles)
        purrr::walk(vis_changed, function(key) {
          session$sendCustomMessage(
            "rp_toggle_gear_column",
            list(
              container_id = ns("table-container"),
              toggle_key = key,
              visible = isTRUE(g[[key]])
            )
          )
        })

        # Content/style toggles → full re-render (unavoidable)
        content_changed <- setdiff(changed_keys, col_vis_toggles)
        if (length(content_changed) > 0L) {
          render_key(render_key() + 1L)
        }
      }
      prev_gear(g)
    })

    # ── Toolbar stats ────────────────────────────────────────────────────
    if (!is.null(config$toolbar_stats_fn)) {
      output$toolbar_stats <- shiny::renderUI({
        config$toolbar_stats_fn(rows(), ROW_KEYS)
      })
    }

    # ── Column observers ─────────────────────────────────────────────────
    # One observer per column × row. Updates flow into rows() directly.

    purrr::walk(
      config$columns,
      .wire_column_observers,
      row_keys = ROW_KEYS,
      input = input,
      rows = rows,
      render_key = render_key,
      config = config,
      session = session
    )

    # ── Year input observer ──────────────────────────────────────────────
    if (!is.null(config$year_col)) {
      shiny::observeEvent(input$year_change, {
        msg <- input$year_change
        if (is.null(msg)) {
          return()
        }
        gk <- msg$grade
        val <- msg$value
        if (is.null(gk) || !(gk %in% ROW_KEYS)) {
          return()
        }
        rs <- rows()
        rs[[gk]][[config$year_col]] <- if (
          is.null(val) || is.na(as.integer(val))
        ) {
          NA_integer_
        } else {
          as.integer(val)
        }
        rows(rs)
      })
    }

    # ── Fill-down ────────────────────────────────────────────────────────
    fd <- config$interactions$fill_down
    if (!is.null(fd)) {
      fd_input_name <- fd$input_name %||% paste0(fd$column, "_fill_down")
      shiny::observeEvent(input[[fd_input_name]], {
        msg <- input[[fd_input_name]]
        if (is.null(msg)) {
          return()
        }

        from_grade <- msg$from_grade
        school <- msg$school
        if (is.null(school) || !is.list(school)) {
          return()
        }

        from_idx <- match(from_grade, ROW_KEYS)
        if (is.na(from_idx)) {
          return()
        }

        rs <- rows()
        me_cols <- purrr::map_chr(
          config$interactions$mutual_exclusion %||% list(),
          "when_on"
        )

        fill_keys <- ROW_KEYS[seq_along(ROW_KEYS) > from_idx]

        # Track which row keys actually received a fill, so we can push
        # the new value to each picker via sendInputMessage afterwards.
        # This is necessary because reactable's React reconciliation will
        # SKIP updating a cell when the emitted HTML string is identical
        # to the previous render — which happens for any cell the user
        # locally cleared (popover.render() mutated the DOM but the
        # cell's data-initial-value attribute is unchanged). Without
        # this push, the picker's stale empty state persists even
        # though rs is updated correctly.
        filled_keys <- character(0)

        for (gk in fill_keys) {
          r <- rs[[gk]]
          if (!is.list(r)) {
            next
          }
          if (!is.null(r[[fd$column]])) {
            next
          }

          # Skip if any mutual-exclusion column is active
          if (purrr::some(me_cols, ~ !is.null(r[[.x]]))) {
            next
          }

          # Range check — delegate to caller-supplied predicate
          if (!is.null(fd$range_check_fn)) {
            if (!fd$range_check_fn(gk, school)) next
          }

          rs[[gk]][[fd$column]] <- school
          filled_keys <- c(filled_keys, gk)
        }

        rows(rs)

        # Push the new value into each filled picker's JS state. The
        # picker's receiveMessage handler will call setValue + render(),
        # bringing the visible DOM in sync with the server state.
        purrr::walk(filled_keys, function(gk) {
          session$sendInputMessage(
            paste0(fd$column, "_", gk),
            list(value = school)
          )
        })

        # Update displacement + row class for filled rows (in case
        # fill-down's effect flips any mutual exclusion or row class).
        new_rs <- rs
        all_me_rules <- config$interactions$mutual_exclusion %||% list()

        purrr::walk(filled_keys, function(gk) {
          new_row <- new_rs[[gk]]

          # Displacement
          purrr::walk(all_me_rules, function(rule) {
            cell_key <- ns(paste0(rule$clears, "-", gk))
            is_displaced <- !is.null(new_row[[rule$when_on]])
            session$sendCustomMessage(
              "rp_set_displaced",
              list(cell_key = cell_key, displaced = is_displaced)
            )
          })

          # Row class
          if (!is.null(config$row_class_fn)) {
            new_class <- tryCatch(
              config$row_class_fn(gk, new_row),
              error = function(e) NULL
            )
            session$sendCustomMessage(
              "rp_set_row_class",
              list(
                row_key = ns(gk),
                new_class = paste(
                  as.character(new_class %||% ""),
                  collapse = " "
                )
              )
            )
          }
        })

        # No render_key increment — the in-place updates above keep the
        # DOM in sync. A full re-render would just trigger React's
        # reconciliation, which skips cells with unchanged HTML anyway.
      })
    }

    # ── Mutual exclusion ─────────────────────────────────────────────────
    # (Handled inside .wire_column_observers for the triggering column)

    # ── Selection observer ───────────────────────────────────────────────
    if (isTRUE(config$selectable)) {
      purrr::walk(ROW_KEYS, function(local_gk) {
        input_id <- paste0(".selected_", local_gk)
        shiny::observeEvent(
          input[[input_id]],
          {
            rs <- rows()
            rs[[local_gk]]$.selected <- isTRUE(input[[input_id]])
            rows(rs)
            render_key(render_key() + 1L)
          },
          ignoreInit = TRUE
        )
      })
    }

    # ── Reset handler ────────────────────────────────────────────────────
    if (isTRUE(config$show_reset)) {
      shiny::observeEvent(input$reset, {
        rows(.build_empty_rows(config))
        render_key(render_key() + 1L)
      })
    }

    # ── Render table ─────────────────────────────────────────────────────
    output$table <- reactable::renderReactable({
      force(render_key())

      current_rows <- shiny::isolate(rows())
      settings <- shiny::isolate(gear())

      # Build data frame skeleton
      tbl <- .build_table_df(config, current_rows, settings)

      # Build colDefs
      col_defs <- .build_col_defs(config, ns, current_rows, settings, tbl)

      wrapper_class <- if (isTRUE(settings$compactRows)) {
        "is-compact reactable-wrap"
      } else {
        "reactable-wrap"
      }

      # Determine row class from config or mutual exclusion rules
      me_cols <- vapply(
        config$interactions$mutual_exclusion %||% list(),
        function(rule) rule$when_on,
        character(1)
      )

      rt <- reactable::reactable(
        tbl,
        columns = col_defs,
        pagination = FALSE,
        sortable = FALSE,
        resizable = FALSE,
        defaultColDef = reactable::colDef(sortable = FALSE),
        class = wrapper_class,
        rowClass = function(index) {
          gk <- tbl[[1]][index]
          row <- current_rows[[gk]]

          # Marker class so JS message handlers can find this row for
          # in-place class updates (e.g. toggling "is-homeschool" after
          # mutual exclusion fires) without re-rendering the table.
          marker <- paste0("rp-row-", ns(gk))

          user_class <- if (!is.list(row)) {
            NULL
          } else if (!is.null(config$row_class_fn)) {
            config$row_class_fn(gk, row)
          } else if (
            length(me_cols) > 0L &&
              purrr::some(me_cols, ~ !is.null(row[[.x]]))
          ) {
            # Fallback: apply "me-active" class when any mutual-exclusion
            # column is active (backward compatible with existing CSS)
            "me-active"
          } else {
            NULL
          }

          paste(c(marker, user_class), collapse = " ")
        }
      )

      bindPickersOnRender(rt)
    })

    # ── Return value ─────────────────────────────────────────────────────
    get_data <- shiny::reactive({
      rs <- rows()
      if (is.null(config$to_output_fn)) {
        return(data.frame())
      }
      do.call(
        rbind,
        lapply(ROW_KEYS, function(gk) {
          row_state <- rs[[gk]]
          # Enforce gating: force empty_value where gate is closed
          sanitized <- purrr::reduce(
            config$columns,
            function(acc, cs) {
              if (!is.null(cs$gate) && !.is_gate_open(cs$gate, acc)) {
                acc[[cs$id]] <- cs$empty_value
              }
              acc
            },
            .init = row_state
          )
          config$to_output_fn(sanitized, gk)
        })
      )
    })

    selected_ids <- if (isTRUE(config$selectable)) {
      shiny::reactive({
        rs <- rows()
        ROW_KEYS[purrr::map_lgl(ROW_KEYS, function(gk) {
          isTRUE(rs[[gk]]$.selected)
        })]
      })
    } else {
      shiny::reactive(NULL)
    }

    list(
      get_data = get_data,
      selected_ids = selected_ids
    )
  })
}


# ── Internal helpers ─────────────────────────────────────────────────────────

#' Build the empty row-state list from config.
#' @noRd
.build_empty_rows <- function(config) {
  stats::setNames(
    lapply(config$row_keys, function(gk) {
      row <- list(.row_key = gk)
      # Selection state
      if (isTRUE(config$selectable)) {
        row$.selected <- FALSE
      }
      # Year column
      if (!is.null(config$year_col)) {
        row[[config$year_col]] <- NA_integer_
      }
      # Widget columns
      purrr::reduce(
        config$columns,
        function(acc, cs) {
          acc[[cs$id]] <- cs$empty_value
          acc
        },
        .init = row
      )
    }),
    config$row_keys
  )
}


#' Populate rows from saved data using the config's from_saved_fn.
#' @noRd
.rows_from_saved <- function(config, saved) {
  rows <- .build_empty_rows(config)

  if (is.null(config$from_saved_fn)) {
    return(rows)
  }

  purrr::reduce(
    seq_len(nrow(saved)),
    function(acc, i) {
      db_row <- saved[i, ]
      gk <- as.character(db_row[[1]]) # first column assumed to be row key
      if (!(gk %in% config$row_keys)) {
        return(acc)
      }

      parsed <- config$from_saved_fn(db_row, config$columns)
      if (is.list(parsed)) {
        purrr::iwalk(parsed, function(val, nm) {
          acc[[gk]][[nm]] <<- val
        })
      }
      acc
    },
    .init = rows
  )
}


#' Wire observers for a single column spec across all row keys.
#'
#' Each observer responds to a single cell's value change. After updating
#' the row state and applying any mutual-exclusion clearing, the observer
#' sends targeted in-place update messages to the client so the table
#' mutates without a full re-render:
#'
#'   - `sendInputMessage(cleared_input_id, list(value = NULL))` updates
#'     a sibling widget that was cleared by mutual exclusion.
#'   - `rp_set_displaced` toggles the active vs. displaced display on
#'     any cell that has a mutual-exclusion rule targeting it.
#'   - `rp_set_row_class` updates the row's user-supplied custom class
#'     (e.g. "is-homeschool") from `row_class_fn`.
#'
#' `render_key` is only incremented when a structural change is needed
#' that the in-place mechanism cannot express (currently: never, since
#' mutual exclusion + row class + value updates are all handled by the
#' targeted messages). Gear toggles and fill-down still increment
#' `render_key` from their own observers.
#'
#' @noRd
.wire_column_observers <- function(
  cs,
  row_keys,
  input,
  rows,
  render_key,
  config,
  session
) {
  ns <- session$ns

  # Pre-compute the mutual-exclusion rules where this column is the
  # trigger (`when_on`). When this column changes, those rules dictate
  # which sibling cell gets cleared + which sibling cells need their
  # displaced display flipped.
  me_rules_triggered_by_this <- Filter(
    function(rule) rule$when_on == cs$id,
    config$interactions$mutual_exclusion %||% list()
  )

  # All mutual-exclusion rules — needed to recompute displacement for
  # every column whose own rendering can be displaced by some other
  # column. After this column's value changes, we re-evaluate every
  # rule on this row to keep the displacement state in sync.
  all_me_rules <- config$interactions$mutual_exclusion %||% list()

  # Each row key gets its own observer
  purrr::walk(row_keys, function(local_gk) {
    local_cs <- cs
    input_id <- paste0(local_cs$id, "_", local_gk)

    shiny::observeEvent(
      input[[input_id]],
      {
        rs <- rows()
        val <- input[[input_id]]

        # Validate
        new_val <- if (!is.null(local_cs$validate_fn)) {
          local_cs$validate_fn(val)
        } else {
          .default_validate(val, local_cs$type)
        }

        old_val <- rs[[local_gk]][[local_cs$id]]
        rs[[local_gk]][[local_cs$id]] <- new_val

        # ── Mutual exclusion: clear the target column when this column
        # turns ON (null → non-null transition). Records which sibling
        # widgets were cleared so we can also send them sendInputMessage.
        #
        # NOTE: we use a `for` loop here, not purrr::walk / purrr::map.
        # R's copy-on-modify semantics mean `rs[[...]] <- NULL` inside
        # an anonymous function modifies a LOCAL copy of rs — the outer
        # rs (the one written back via rows(rs)) would be unchanged.
        # The for-loop runs in this handler's scope, so rs mutations
        # are visible to rows(rs).
        cleared_cols <- character(0)
        if (!is.null(new_val) && is.null(old_val)) {
          for (rule in me_rules_triggered_by_this) {
            if (!is.null(rs[[local_gk]][[rule$clears]])) {
              rs[[local_gk]][[rule$clears]] <- NULL
              cleared_cols <- c(cleared_cols, rule$clears)
            }
          }
        }

        rows(rs)

        # ── Send targeted in-place updates ───────────────────────────
        # The popover widgets handle their OWN cell's display via the
        # local Popover.render() call inside close(). What we still
        # need to push to the client:
        #
        #   1. Clear any sibling widget that mutual exclusion just
        #      cleared (so the cell shows its empty state instead of
        #      the now-stale filled state).
        #   2. Recompute the displacement state for every cell on
        #      this row that could be displaced by ANY mutual
        #      exclusion rule, since this column's change may have
        #      flipped displacement on or off for siblings.
        #   3. Recompute the row's user-supplied custom class via
        #      row_class_fn (so e.g. "is-homeschool" is added/removed).

        new_row <- rs[[local_gk]]

        # 1. Clear cleared widgets
        purrr::walk(cleared_cols, function(cleared_col_id) {
          session$sendInputMessage(
            paste0(cleared_col_id, "_", local_gk),
            list(value = NULL)
          )
        })

        # 2. Update displacement state on every cell that has a rule
        #    targeting it. The displacement state of a cell whose
        #    rule says "when_on=X, clears=Y" is determined by whether
        #    X is currently non-null on this row.
        purrr::walk(all_me_rules, function(rule) {
          cell_key <- ns(paste0(rule$clears, "-", local_gk))
          is_displaced <- !is.null(new_row[[rule$when_on]])
          session$sendCustomMessage(
            "rp_set_displaced",
            list(cell_key = cell_key, displaced = is_displaced)
          )
        })

        # 3. Update row custom class
        if (!is.null(config$row_class_fn)) {
          new_class <- tryCatch(
            config$row_class_fn(local_gk, new_row),
            error = function(e) NULL
          )
          session$sendCustomMessage(
            "rp_set_row_class",
            list(
              row_key = ns(local_gk),
              new_class = paste(as.character(new_class %||% ""), collapse = " ")
            )
          )
        }
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )
  })
}


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
