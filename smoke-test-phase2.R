# smoke-test-phase2.R
#
# Smoke test: config_table with gating, selection, and reset.
# Run with: shiny::runApp("smoke-test-phase2.R")
#
# What to verify:
#   1. Checkbox column appears on the left
#   2. "Score" column is LOCKED (dimmed, disabled) until "Status" = "active"
#   3. Changing "Status" to "active" unlocks the "Score" input
#   4. Changing "Status" away from "active" re-locks "Score"
#   5. In the state table, Score shows empty_value (0) when gate is closed
#   6. Reset button clears all inputs and deselects all rows
#   7. selected_ids text updates as checkboxes are toggled

library(shiny)
library(reactablePlus)
library(reactable)

# ── Config ──────────────────────────────────────────────────────────────────

cfg <- table_config(
  row_keys   = c("item_1", "item_2", "item_3"),
  row_labels = c("Widget A", "Widget B", "Widget C"),
  selectable = TRUE,
  show_reset = TRUE,

  columns = list(
    widget_col("status", "dropdown", "Status",
      width = 140,
      options = list(
        choices = c("Active" = "active", "Inactive" = "inactive", "Pending" = "pending")
      )
      # Note: triggers_rerender is auto-set by table_config
      # because "score" has a gate referencing "status"
    ),
    widget_col("score", "numeric", "Score",
      width = 90,
      options = list(min = 0, max = 100, step = 1),
      gate = list(
        list(type = "value", col_id = "status", values = c("active"))
      )
    ),
    widget_col("notes", "text", "Notes",
      min_width = 160,
      options = list(placeholder = "Add notes...")
    )
  ),

  badge_col = NULL,
  year_col  = NULL,

  to_output_fn = function(row_state, row_key) {
    data.frame(
      row_key  = row_key,
      selected = isTRUE(row_state$.selected),
      status   = if (is.null(row_state$status) || is.na(row_state$status)) NA_character_ else row_state$status,
      score    = if (is.null(row_state$score) || is.na(row_state$score)) NA_real_ else as.numeric(row_state$score),
      notes    = row_state$notes %||% "",
      stringsAsFactors = FALSE
    )
  }
)


# ── UI ──────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  titlePanel("Phase 2: Gate + Selection + Reset"),
  br(),
  config_table_ui("demo", cfg),
  br(),
  h4("selected_ids:"),
  verbatimTextOutput("sel_display"),
  h4("get_data (gate-enforced):"),
  reactableOutput("state_display")
)


# ── Server ──────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  result <- config_table_server("demo", cfg)

  output$sel_display <- renderPrint({
    ids <- result$selected_ids()
    if (length(ids) == 0L) cat("(none)") else cat(ids, sep = ", ")
  })

  output$state_display <- renderReactable({
    df <- result$get_data()
    if (nrow(df) == 0L) return(reactable(data.frame(message = "No data yet")))
    reactable(df, bordered = TRUE, striped = TRUE, highlight = TRUE)
  })
}


shinyApp(ui, server)
