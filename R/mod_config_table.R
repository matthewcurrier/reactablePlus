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
