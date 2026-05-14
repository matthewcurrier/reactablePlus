# smoke-test-primitives.R
#
# Quick smoke test: config_table with a mix of primitive input types
# and a picker widget. Run with: shiny::runApp("smoke-test-primitives.R")
#
# What to verify:
#   1. Table renders with all 5 columns visible
#   2. Dropdown shows choices and sends value on change
#   3. Numeric spinner respects min/max and sends value on change
#   4. Checkbox toggles and sends TRUE/FALSE
#   5. Text input accepts typing and sends value
#   6. Notes picker widget renders and accepts input
#   7. "Current state" table below updates live as you interact

library(shiny)
library(reactablePlus)
library(reactable)

# ── Config ──────────────────────────────────────────────────────────────────

cfg <- table_config(
  row_keys   = c("item_1", "item_2", "item_3"),
  row_labels = c("Widget A", "Widget B", "Widget C"),

  columns = list(
    widget_col("status", "dropdown", "Status",
      width = 140,
      options = list(
        choices = c("Active" = "active", "Inactive" = "inactive", "Pending" = "pending")
      )
    ),
    widget_col("quantity", "numeric", "Qty",
      width = 90,
      options = list(min = 0, max = 999, step = 1)
    ),
    widget_col("approved", "checkbox", "OK?",
      width = 60
    ),
    widget_col("description", "text", "Description",
      min_width = 160,
      options = list(placeholder = "Enter description...", max_chars = 100)
    ),
    widget_col("notes", "notes_input", "Notes",
      min_width = 140
    )
  ),

  # No grade badge or year spinner — this is generic

  badge_col = NULL,
  year_col  = NULL,

  # Marshal rows → output data frame
  to_output_fn = function(row_state, row_key) {
    data.frame(
      row_key     = row_key,
      status      = if (is.null(row_state$status) || is.na(row_state$status)) NA_character_ else row_state$status,
      quantity    = if (is.null(row_state$quantity) || is.na(row_state$quantity)) NA_real_ else as.numeric(row_state$quantity),
      approved    = isTRUE(row_state$approved),
      description = row_state$description %||% "",
      notes       = row_state$notes %||% "",
      stringsAsFactors = FALSE
    )
  }
)


# ── UI ──────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  titlePanel("Smoke Test: Primitive + Picker Columns"),
  br(),
  config_table_ui("demo", cfg),
  br(),
  h4("Current state (get_data reactive):"),
  reactableOutput("state_display")
)


# ── Server ──────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  result <- config_table_server("demo", cfg)

  output$state_display <- renderReactable({
    df <- result$get_data()
    if (nrow(df) == 0L) return(reactable(data.frame(message = "No data yet")))
    reactable(df, bordered = TRUE, striped = TRUE, highlight = TRUE)
  })
}


shinyApp(ui, server)
