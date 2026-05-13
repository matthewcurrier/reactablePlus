# =============================================================================
# config-table.R
#
# Config-driven editable-table Shiny module driven by a table_config object.
# The module does NOT know what domain it serves — all domain logic
# (columns, data marshaling, toolbar stats) comes from the config.
#
# This is the higher-level API; see editable_table_ui / editable_table_server
# for the lower-level raw-spec module.
#
# SECTIONS convention:
#   server signature: function(id, config, data_r, search_fn)
#   return value:     list(get_data = reactive)
# =============================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

#' Config-driven Editable Table UI
#'
#' UI function for the config-driven editable table module. Works with
#' `table_config()` objects and picker widgets, as opposed to the
#' lower-level `editable_table_ui()` which uses raw `row_spec`/`col_spec`
#' lists.
#' @param id Module namespace ID.
#' @param config A `table_config` object.
#' @return A tagList with dependencies, toolbar, and reactable output.
#' @export
config_table_ui <- function(id, config) {
  ns <- shiny::NS(id)

  toolbar <- if (
    !is.null(config$toolbar_stats_fn) || !is.null(config$gear_toggles)
  ) {
    shiny::tags$div(
      class = "panel-toolbar",
      if (!is.null(config$toolbar_stats_fn)) {
        shiny::tags$span(
          class = "toolbar-stat",
          shiny::uiOutput(ns("toolbar_stats"), inline = TRUE)
        )
      },
      if (!is.null(config$gear_toggles)) {
        gearPopoverInput(
          inputId = ns("gear"),
          toggles = config$gear_toggles
        )
      }
    )
  }

  shiny::tagList(
    useReactablePlus(),
    bslib::card(
      bslib::card_body(
        toolbar,
        reactable::reactableOutput(ns("table"))
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
#'   `school_picker` type. Default `NULL`.
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
      useSchoolSearch(input, session, search_fn = search_fn)
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

    shiny::observeEvent(
      gear(),
      {
        render_key(render_key() + 1L)
      },
      ignoreInit = TRUE
    )

    # ── Toolbar stats ────────────────────────────────────────────────────
    if (!is.null(config$toolbar_stats_fn)) {
      output$toolbar_stats <- shiny::renderUI({
        config$toolbar_stats_fn(rows(), ROW_KEYS)
      })
    }

    # ── Column observers ─────────────────────────────────────────────────
    # One observer per column × row. Updates flow into rows() directly.

    purrr::walk(config$columns, .wire_column_observers,
      row_keys   = ROW_KEYS,
      input      = input,
      rows       = rows,
      render_key = render_key,
      config     = config
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
      shiny::observeEvent(input$school_fill_down, {
        msg <- input$school_fill_down
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

        rs <- purrr::reduce(fill_keys, function(acc, gk) {
          r <- acc[[gk]]
          if (!is.list(r)) return(acc)
          if (!is.null(r[[fd$column]])) return(acc)

          # Skip if any mutual-exclusion column is active
          me_active <- purrr::some(me_cols, ~ !is.null(r[[.x]]))
          if (me_active) return(acc)

          # Range check
          if (isTRUE(fd$range_check)) {
            low  <- school$low_grade
            high <- school$high_grade
            if (!is.null(low) && !is.null(high)) {
              if (!gradeInRange(gk, low, high)) return(acc)
            }
          }

          acc[[gk]][[fd$column]] <- school
          acc
        }, .init = rs)
        rows(rs)
        render_key(render_key() + 1L)
      })
    }

    # ── Mutual exclusion ─────────────────────────────────────────────────
    # (Handled inside .wire_column_observers for the triggering column)

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

      # Determine homeschool-style row class from mutual exclusion rules
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
          gk <- tbl[[1]][index] # first column is always the row key
          row <- current_rows[[gk]]
          if (!is.list(row)) {
            return(NULL)
          }
          if (purrr::some(me_cols, ~ !is.null(row[[.x]]))) {
            "is-homeschool"
          } else {
            NULL
          }
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
          config$to_output_fn(rs[[gk]], gk)
        })
      )
    })

    list(get_data = get_data)
  })
}


# ── Internal helpers ─────────────────────────────────────────────────────────

#' Build the empty row-state list from config.
#' @noRd
.build_empty_rows <- function(config) {
  stats::setNames(
    lapply(config$row_keys, function(gk) {
      row <- list(.row_key = gk)
      # Year column
      if (!is.null(config$year_col)) {
        row[[config$year_col]] <- NA_integer_
      }
      # Widget columns
      purrr::reduce(config$columns, function(acc, cs) {
        acc[[cs$id]] <- cs$empty_value
        acc
      }, .init = row)
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

  purrr::reduce(seq_len(nrow(saved)), function(acc, i) {
    db_row <- saved[i, ]
    gk <- as.character(db_row[[1]]) # first column assumed to be row key
    if (!(gk %in% config$row_keys)) return(acc)

    parsed <- config$from_saved_fn(db_row, config$columns)
    if (is.list(parsed)) {
      purrr::iwalk(parsed, function(val, nm) {
        acc[[gk]][[nm]] <<- val
      })
    }
    acc
  }, .init = rows)
}


#' Wire observers for a single column spec across all row keys.
#' @noRd
.wire_column_observers <- function(
  cs,
  row_keys,
  input,
  rows,
  render_key,
  config
) {
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

        # Mutual exclusion: if this column turned ON, clear the target
        purrr::walk(
          config$interactions$mutual_exclusion %||% list(),
          function(rule) {
            if (rule$when_on == local_cs$id) {
              if (!is.null(new_val) && is.null(old_val)) {
                rs[[local_gk]][[rule$clears]] <<- NULL
              }
            }
          }
        )

        rows(rs)

        if (isTRUE(local_cs$triggers_rerender)) {
          render_key(render_key() + 1L)
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
    school_picker = {
      if (is.list(val) && !is.null(val$id)) val else NULL
    },
    attendance_picker = ,
    homeschool_picker = {
      if (is.list(val)) val else NULL
    },
    notes_input = {
      if (is.null(val) || length(val) == 0L) "" else as.character(val)[1L]
    },
    custom = val,
    val
  )
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

  # Widget columns — placeholder values (actual content from cell fns)
  visible_cols <- purrr::keep(config$columns, function(cs) {
    is.null(cs$gear_toggle) || isTRUE(settings[[cs$gear_toggle]])
  })

  tbl <- purrr::reduce(visible_cols, function(acc, cs) {
    if (cs$type == "notes_input") {
      acc[[cs$id]] <- vapply(
        current_rows,
        function(r) {
          if (!is.list(r)) return("")
          val <- r[[cs$id]]
          if (is.null(val) || length(val) == 0L) "" else as.character(val)[1L]
        },
        character(1)
      )
    } else {
      acc[[cs$id]] <- rep("", n)
    }
    acc
  }, .init = tbl)

  tbl
}


#' Build reactable colDef list from config.
#' @noRd
.build_col_defs <- function(config, ns, current_rows, settings, tbl) {
  col_defs <- list()

  # Row key (hidden)
  col_defs$.row_key <- reactable::colDef(show = FALSE)

  # Badge column
  if (!is.null(config$badge_col)) {
    col_defs$.row_label <- reactable::colDef(
      name = "Grade",
      width = 76,
      cell = function(value, index) {
        gk <- tbl$.row_key[index]
        css_class <- paste0("grade-badge g-", gk)
        sprintf('<span class="%s">%s</span>', css_class, value)
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

  # Widget columns
  visible_cols <- purrr::keep(config$columns, function(cs) {
    is.null(cs$gear_toggle) || isTRUE(settings[[cs$gear_toggle]])
  })

  widget_defs <- purrr::map(visible_cols, function(cs) {
    .build_widget_coldef(cs, ns, current_rows, settings, tbl, config)
  }) |>
    purrr::set_names(purrr::map_chr(visible_cols, "id"))

  col_defs <- c(col_defs, widget_defs)

  col_defs
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

    school_picker = function(value, index) {
      gk <- tbl$.row_key[index]
      row <- current_rows[[gk]]
      if (!is.list(row)) {
        row <- list()
      }

      # Mutual exclusion: if the triggering column is active, show its display
      active_rule <- purrr::detect(me_rules, ~ !is.null(row[[.x$when_on]]))
      if (!is.null(active_rule)) {
        return(active_rule$display %||% "")
      }

      opts <- cs$options
      as.character(schoolPickerInput(
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
    },

    attendance_picker = function(value, index) {
      gk <- tbl$.row_key[index]
      row <- current_rows[[gk]]

      opts <- cs$options
      as.character(attendancePickerInput(
        inputId = ns(paste0(cs$id, "_", gk)),
        value = if (is.list(row)) row[[cs$id]] else NULL,
        grade_label = config$label_map[[gk]],
        sections = opts$sections,
        trigger_label = opts$trigger_label,
        popover_title = opts$popover_title,
        show_notes = opts$show_notes %||% TRUE,
        notes_placeholder = opts$notes_placeholder
      ))
    },

    homeschool_picker = function(value, index) {
      gk <- tbl$.row_key[index]
      row <- current_rows[[gk]]

      opts <- cs$options
      as.character(homeschoolPickerInput(
        inputId = ns(paste0(cs$id, "_", gk)),
        value = if (is.list(row)) row[[cs$id]] else NULL,
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

    # Fallback
    function(value, index) as.character(value)
  )

  do.call(reactable::colDef, col_def_args)
}
