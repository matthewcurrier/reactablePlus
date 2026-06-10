# Characterization app for a popover picker (homeschool_picker — a popover
# form, no search backend). Captures pick -> get_data and clear -> get_data,
# which the store port must preserve. Driven by setting the popover value and
# firing 'change' (works for both the current binding and the ported one).

library(shiny)
library(reactable)
library(reactablePlus)
library(jsonlite)

`%||%` <- function(a, b) if (is.null(a)) b else a

cfg <- table_config(
  row_keys   = c("r1", "r2"),
  row_labels = c("R1", "R2"),
  columns = list(
    widget_col("hs", "homeschool_picker", "Homeschool"),
    widget_col("att", "attendance_picker", "Attendance")
  ),
  badge_col   = "row",
  badge_label = "Row",
  to_output_fn = function(rs, gk) {
    v <- rs$hs
    a <- rs$att
    data.frame(
      row        = gk,
      by         = if (is.null(v)) NA_character_ else (v$by %||% NA_character_),
      curriculum = if (is.null(v)) NA_character_ else (v$curriculum %||% NA_character_),
      # Attendance: empty reads back as NULL (the binding normalizes the
      # internal {} to null), so school is NA until a section is set.
      school     = if (is.null(a)) NA_character_ else (a$school %||% NA_character_),
      stringsAsFactors = FALSE
    )
  }
)

ui <- fluidPage(
  config_table_ui("t", cfg),
  verbatimTextOutput("o")
)

server <- function(input, output, session) {
  r <- config_table_server("t", cfg)
  output$o <- renderText(
    jsonlite::toJSON(r$get_data(), dataframe = "rows",
                     auto_unbox = TRUE, na = "null")
  )
}

shinyApp(ui, server)
