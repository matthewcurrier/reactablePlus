# demo-06-appendable.R
#
# DEMO: Appendable Table (user-managed rows)
# Showcases: appendable mode, add/delete rows, min_rows/max_rows,
#            gating, show_reset, to_output_fn
#
# Run: shiny::runApp("demo-06-appendable.R")

library(shiny)
library(reactablePlus)
library(reactable)


# ── Config ──────────────────────────────────────────────────────────────────

cfg <- table_config(
  appendable = TRUE,
  allow_delete = TRUE,
  min_rows = 1L,
  max_rows = 8L,
  show_reset = TRUE,

  columns = list(
    widget_col("fruit", "dropdown", "Fruit",
      width = 160,
      options = list(
        choices = c(
          "Apple"      = "apple",
          "Banana"     = "banana",
          "Cherry"     = "cherry",
          "Dragonfruit" = "dragonfruit",
          "Elderberry" = "elderberry"
        )
      )
    ),
    widget_col("quantity", "numeric", "Qty",
      width = 90,
      options = list(min = 1, max = 999, step = 1)
    ),
    widget_col("organic", "checkbox", "Organic",
      width = 80
    ),
    widget_col("notes", "text", "Notes",
      min_width = 160,
      options = list(
        placeholder = "Optional notes...",
        max_chars = 200
      )
    )
  ),

  to_output_fn = function(row_state, row_key) {
    data.frame(
      row_id  = row_key,
      fruit   = row_state$fruit %||% NA_character_,
      qty     = row_state$quantity %||% NA_real_,
      organic = isTRUE(row_state$organic),
      notes   = row_state$notes %||% "",
      stringsAsFactors = FALSE
    )
  }
)


# ── UI ──────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  titlePanel("Appendable Table Demo"),
  p(class = "text-muted",
    "User-managed rows: click ", tags$b("Add Row"), " to append,",
    " the ", tags$b("\u00d7"), " button to delete. Min 1 row, max 8.",
    " Click ", tags$b("Reset"), " to clear back to one blank row."
  ),
  br(),
  config_table_ui("order", cfg),
  br(),
  h5("Live output (get_data):"),
  reactableOutput("output_tbl")
)


# ── Server ──────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  result <- config_table_server("order", cfg)

  output$output_tbl <- renderReactable({
    df <- result$get_data()
    if (is.null(df) || nrow(df) == 0L) {
      return(reactable(
        data.frame(note = "Add some rows above and fill them in.")
      ))
    }
    reactable(df, bordered = TRUE, striped = TRUE, compact = TRUE)
  })
}


shinyApp(ui, server)
