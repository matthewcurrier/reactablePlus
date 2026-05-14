# R/editable_table_server.R

#' Server function for the editable table Shiny module
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [config_table_server()] instead. The raw-spec module is
#' superseded by the config-driven module. See [table_config()] and
#' [widget_col()] for the replacement API.
#'
#' @param id              A string. The module namespace ID.
#' @param data_r          A reactive data frame containing at least the columns
#'   declared in \code{row_spec}.
#' @param row_spec        A validated row_spec list (see \code{validate_row_spec}).
#' @param col_spec        A validated col_spec list (see \code{validate_col_spec}).
#' @param existing_data_r   A reactive data frame of previously saved values, or
#'   \code{NULL} (default) for a blank start.
#' @param reactable_options A plain list of additional arguments passed to
#'   \code{reactable::reactable()}. Module-controlled arguments (\code{data},
#'   \code{columns}, \code{onClick}) are protected and cannot be overridden.
#'   Defaults that can be overridden include \code{sortable}, \code{pagination},
#'   \code{highlight}, \code{bordered}, and \code{striped}.
#'
#' @return A named list of two reactives:
#' \describe{
#'   \item{\code{current_data}}{A reactive tibble with id_col + one column per
#'     col_spec entry, updated continuously as the user interacts.}
#'   \item{\code{selected_ids}}{A reactive vector of id_col values for checked
#'     rows. \code{NULL} when \code{row_spec$selectable} is not TRUE.}
#' }
#' @keywords internal
#' @export
editable_table_server <- function(
  id,
  data_r,
  row_spec,
  col_spec,
  existing_data_r = NULL,
  reactable_options = list()
) {
  if (!isTRUE(getOption("reactablePlus.suppress_deprecation"))) {
    .Deprecated("config_table_server", package = "reactablePlus")
  }
  validate_row_spec(row_spec)
  col_spec <- validate_col_spec(col_spec, row_spec)

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selectable <- isTRUE(row_spec$selectable)

    # ── Reset tracking ────────────────────────────────────────────────────────
    reset_count <- shiny::reactiveVal(0L)

    # ── Display data ──────────────────────────────────────────────────────────
    display_data_r <- shiny::reactive({
      existing <- if (reset_count() > 0L || is.null(existing_data_r)) {
        NULL
      } else {
        existing_data_r()
      }

      df <- merge_existing_data(
        data_df = data_r(),
        existing_data = existing,
        row_spec = row_spec,
        col_spec = col_spec
      )

      # Prepend a .selected column so the checkbox cell renderer has a column
      # to attach to. Relocated to position 1 so reactable renders it as the
      # first visible column — reactable respects data frame column order, not
      # the col_defs list order. Always FALSE — actual state lives in Shiny inputs.
      if (selectable) {
        df <- dplyr::mutate(df, .selected = FALSE) |>
          dplyr::relocate(.selected, .before = 1L)
      }

      df
    })

    # ── Editable reactable ────────────────────────────────────────────────────
    # Uses renderReactable (not renderUI) to avoid the flicker caused by full
    # widget teardown on reset. DOM cell clearing is handled by
    # session$sendInputMessage in the reset handler; server-side state is
    # handled by input_overrides. Both are reliable without widget teardown.
    output$table <- reactable::renderReactable({
      df <- display_data_r()
      id_col <- row_spec$id_col
      ns_prefix <- ns("")

      # Hide the id_col from the visible table — it is internal plumbing.
      hidden_id_col <- purrr::set_names(
        list(reactable::colDef(show = FALSE)),
        id_col
      )

      # Build the gate map once — used by both the select col def (checkbox JS)
      # and the input col defs (dropdown onchange JS).
      gate_map <- build_gate_map(col_spec)

      select_col_defs <- if (selectable) {
        build_select_col_def(df, id_col, ns, gate_map)
      } else {
        list()
      }
      display_col_defs <- build_display_col_defs(row_spec)
      input_col_defs <- build_input_col_defs(df, id_col, col_spec, ns, gate_map)

      col_defs <- c(
        hidden_id_col,
        select_col_defs,
        display_col_defs,
        input_col_defs
      )

      # Row-click JS: toggles selection only when the click lands on the
      # checkbox column or one of the display columns. Also evaluates the
      # full selection gate state for any gated fields in the clicked row.
      on_click <- if (selectable) {
        selectable_col_ids <- c(
          ".selected",
          purrr::map_chr(row_spec$display_cols, \(d) d$col_name)
        )
        selectable_col_ids_json <- jsonlite::toJSON(
          selectable_col_ids,
          auto_unbox = FALSE
        )

        # Build per-row gate JS snippets for all rows so the onClick handler
        # can evaluate them by row ID at runtime.
        row_ids <- df[[id_col]]
        per_row_gate_js <- purrr::map_chr(row_ids, function(row_id) {
          gate_js <- build_selection_gate_js(row_id, ns, gate_map)
          sprintf(
            "if (rowId == %s) { var isChecked = el ? el.checked : false; %s }",
            jsonlite::toJSON(row_id, auto_unbox = TRUE),
            gate_js
          )
        }) |>
          paste(collapse = "\n")

        htmlwidgets::JS(sprintf(
          "function(rowInfo, colInfo) {
             var allowed = %s;
             if (allowed.indexOf(colInfo.id) === -1) return;
             var rowId = rowInfo.values['%s'];
             var cbId  = '%s' + rowId + '_selected';
             var el    = document.getElementById(cbId);
             var isChecked = false;
             if (el) {
               el.checked = !el.checked;
               isChecked  = el.checked;
               Shiny.setInputValue(cbId, isChecked, {priority: 'event'});
             }
             %s
           }",
          selectable_col_ids_json,
          id_col,
          ns_prefix,
          per_row_gate_js
        ))
      } else {
        NULL
      }

      # Module-controlled arguments — cannot be overridden via reactable_options.
      protected_args <- list(
        data = df,
        columns = col_defs,
        onClick = on_click
      )

      # Caller-supplied options override base defaults but never protected args.
      # When selectable is TRUE, pointer cursor is added to the theme so users
      # can clearly see that rows are clickable. The caller can override the
      # full theme via reactable_options if needed.
      base_defaults <- list(
        sortable = FALSE,
        pagination = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        striped = TRUE,
        theme = if (selectable) {
          reactable::reactableTheme(
            rowStyle = list(cursor = "pointer")
          )
        } else {
          NULL
        }
      )

      final_args <- c(
        base_defaults[!names(base_defaults) %in% names(reactable_options)],
        reactable_options[!names(reactable_options) %in% names(protected_args)],
        protected_args
      )

      do.call(reactable::reactable, final_args)
    })

    # ── Input override store ──────────────────────────────────────────────────
    # A reactiveValues that holds explicit overrides for any input the server
    # needs to control directly (reset, deselection wipe). collect_inputs()
    # checks this store FIRST — if an override exists it takes precedence over
    # input[[local_id]]. This avoids the browser round-trip that made
    # session$sendInputMessage unreliable for immediate reactive invalidation.
    input_overrides <- shiny::reactiveValues()

    # ── current_data ──────────────────────────────────────────────────────────
    # Depends on input_overrides (via collect_inputs) so it invalidates the
    # moment any override is written — including on reset.
    current_data <- shiny::reactive({
      collect_inputs(
        input = input,
        row_ids = data_r()[[row_spec$id_col]],
        col_spec = col_spec,
        id_col = row_spec$id_col,
        fallback_df = display_data_r(),
        input_overrides = input_overrides
      )
    })

    # ── selected_ids ──────────────────────────────────────────────────────────
    selected_ids <- if (selectable) {
      shiny::reactive({
        row_ids <- data_r()[[row_spec$id_col]]
        row_ids[purrr::map_lgl(row_ids, function(row_id) {
          key <- paste0(row_id, "_selected")
          val <- if (!is.null(input_overrides[[key]])) {
            input_overrides[[key]]
          } else {
            input[[key]]
          }
          isTRUE(val)
        })]
      })
    } else {
      shiny::reactive(NULL)
    }

    # ── Deselection wipe ──────────────────────────────────────────────────────
    if (selectable) {
      selected_gate_col_names <- purrr::map_chr(
        purrr::keep(col_spec, \(s) {
          !is.null(s$gate) &&
            any(purrr::map_lgl(s$gate, \(cond) cond$type == "selected"))
        }),
        \(s) s$col_name
      )

      prev_selected <- shiny::reactiveVal(NULL)

      shiny::observeEvent(selected_ids(), ignoreNULL = FALSE, {
        current <- selected_ids() %||% c()
        newly_deselected <- setdiff(prev_selected(), current)

        # Build the JS message payload while updating server state.
        deselect_messages <- purrr::flatten(purrr::map(
          newly_deselected,
          function(row_id) {
            purrr::map(selected_gate_col_names, function(col_name) {
              spec_entry <- col_spec[[which(
                purrr::map_chr(col_spec, \(s) s$col_name) == col_name
              )]]
              blank <- switch(
                spec_entry$type,
                numeric = spec_entry$min,
                checkbox = FALSE,
                toggle = FALSE,
                text = "",
                NA
              )
              local_id <- paste0(row_id, "_", col_name)
              input_overrides[[local_id]] <- blank

              dom_id <- if (spec_entry$type == "toggle") {
                paste0(ns(local_id), "_btn")
              } else {
                ns(local_id)
              }

              list(
                id = dom_id,
                value = if (length(blank) == 1L && is.na(blank)) {
                  NULL
                } else {
                  blank
                },
                locked = TRUE # always locked when we wipe — the gate just closed
              )
            })
          }
        ))

        if (length(deselect_messages) > 0L) {
          session$sendCustomMessage(
            "editableTableReset",
            list(inputs = deselect_messages)
          )
        }

        prev_selected(current)
      })
    }

    # ── Data display output ───────────────────────────────────────────────────
    output$data_display <- reactable::renderReactable({
      reactable::reactable(
        current_data(),
        bordered = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    # ── Reset handler ─────────────────────────────────────────────────────────
    shiny::observeEvent(input$reset, {
      row_ids <- data_r()[[row_spec$id_col]]

      # Build the list of inputs to reset, capturing both server-side state
      # (input_overrides) and the message payload that JS will use to update
      # the DOM directly.
      reset_messages <- purrr::flatten(purrr::map(row_ids, function(row_id) {
        field_msgs <- purrr::map(col_spec, function(spec) {
          local_id <- paste0(row_id, "_", spec$col_name)
          blank <- switch(
            spec$type,
            numeric = spec$min,
            checkbox = FALSE,
            toggle = FALSE,
            text = "",
            NA
          )

          # Server-side state: invalidates current_data immediately so the
          # data display clears in the same reactive flush.
          input_overrides[[local_id]] <- blank

          # Determine if this field should render locked after reset.
          # A field is locked if it has any gate condition — after reset
          # all rows are deselected and the controller fields are blank,
          # so every gate condition fails.
          locked <- !is.null(spec$gate)

          # Toggle uses the button element, not the hidden input
          dom_id <- if (spec$type == "toggle") {
            paste0(ns(local_id), "_btn")
          } else {
            ns(local_id)
          }

          list(
            id = dom_id,
            value = if (length(blank) == 1L && is.na(blank)) NULL else blank,
            locked = locked
          )
        })

        if (selectable) {
          key <- paste0(row_id, "_selected")
          input_overrides[[key]] <- FALSE
          select_msg <- list(
            id = ns(key),
            value = FALSE,
            locked = FALSE
          )
          c(field_msgs, list(select_msg))
        } else {
          field_msgs
        }
      }))

      # Send the reset message to the browser. The JS handler in the UI
      # updates each input's DOM value, disabled attribute, and locked styles.
      session$sendCustomMessage(
        "editableTableReset",
        list(inputs = reset_messages)
      )

      reset_count(reset_count() + 1L)

      # Register a one-shot observer per field that clears its override the
      # moment the user next interacts with it. once = TRUE means each
      # observer auto-destroys after firing exactly once.
      # ignoreInit = TRUE prevents firing immediately on creation.
      #
      # This replaces a broken approach using shiny::observe(priority = -1L)
      # which is a PERSISTENT observer that re-fires on every reactive flush,
      # clearing overrides immediately after they were set.
      purrr::walk(row_ids, function(row_id) {
        purrr::walk(col_spec, function(spec) {
          local_id <- paste0(row_id, "_", spec$col_name)
          shiny::observeEvent(
            input[[local_id]],
            {
              input_overrides[[local_id]] <- NULL
            },
            once = TRUE,
            ignoreInit = TRUE,
            ignoreNULL = FALSE
          )
        })
        if (selectable) {
          key <- paste0(row_id, "_selected")
          shiny::observeEvent(
            input[[key]],
            {
              input_overrides[[key]] <- NULL
            },
            once = TRUE,
            ignoreInit = TRUE,
            ignoreNULL = FALSE
          )
        }
      })
    })

    # ── Return ────────────────────────────────────────────────────────────────
    list(
      current_data = current_data,
      selected_ids = selected_ids
    )
  })
}

# Cell builders and gate helpers live in R/cell_builders.R.

# R/editable_table_ui.R

#' UI function for the editable table Shiny module
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [config_table_ui()] instead. The raw-spec module
#' (`editable_table_ui` / `editable_table_server`) is superseded by
#' the config-driven module (`config_table_ui` / `config_table_server`)
#' which supports all the same input types plus picker widgets, gating,
#' selection, and reset.
#'
#' @param id A string. The module namespace ID.
#' @return A named list of \code{shiny.tag} objects.
#' @keywords internal
#' @export
editable_table_ui <- function(id) {
  if (!isTRUE(getOption("reactablePlus.suppress_deprecation"))) {
    .Deprecated("config_table_ui", package = "reactablePlus")
  }
  ns <- shiny::NS(id)

  list(
    table = shiny::tagList(
      # Custom message handler that directly manipulates the DOM on reset.
      # This is necessary because:
      #   - renderReactable preserves existing custom HTML cells across
      #     re-renders rather than recreating them
      #   - session$sendInputMessage requires a Shiny input binding which
      #     raw HTML inputs do not have
      # Direct DOM manipulation is the only reliable way to clear custom
      # HTML inputs without tearing down the entire reactable widget.
      shiny::tags$script(shiny::HTML(
        "Shiny.addCustomMessageHandler('editableTableReset', function(data) {
          data.inputs.forEach(function(item) {
            var el = document.getElementById(item.id);
            if (!el) return;

            // Toggle buttons need text and background updated
            if (el.tagName === 'BUTTON') {
              el.textContent = item.value ? 'On' : 'Off';
              el.style.backgroundColor = item.value ? '#28a745' : '#6c757d';
              var hidden = document.getElementById(item.id.replace(/_btn$/, ''));
              if (hidden) hidden.value = String(item.value);
            } else if (el.type === 'checkbox') {
              el.checked = item.value === true;
            } else {
              el.value = (item.value === null || item.value === undefined)
                ? '' : item.value;
            }

            // Apply locked / unlocked visual state
            if (item.locked) {
              el.disabled = true;
              el.style.opacity = '0.4';
              el.style.cursor = 'not-allowed';
              el.style.pointerEvents = 'none';
            } else {
              el.disabled = false;
              el.style.opacity = '1';
              el.style.cursor = 'default';
              el.style.pointerEvents = '';
            }
          });
        });"
      )),
      reactable::reactableOutput(ns("table"))
    ),

    reset_button = shiny::actionButton(
      inputId = ns("reset"),
      label = "Reset",
      class = "btn btn-warning"
    ),

    data_display = reactable::reactableOutput(ns("data_display"))
  )
}
