# Test app: fill-down onto a store-backed (dropdown) column. Picking a value
# in a row and clicking its fill link should cascade the value into the empty
# rows below it.

library(shiny)
library(reactable)
library(reactablePlus)
library(jsonlite)

`%||%` <- function(a, b) if (is.null(a)) b else a

cfg <- table_config(
  row_keys   = c("r1", "r2", "r3"),
  row_labels = c("R1", "R2", "R3"),
  columns = list(
    widget_col("status", "dropdown", "Status",
      options = list(choices = c("Active" = "a", "Inactive" = "i"))
    )
  ),
  interactions = list(fill_down = list(column = "status")),
  badge_col   = "row",
  badge_label = "Row",
  to_output_fn = function(rs, gk) {
    data.frame(row = gk, status = rs$status %||% NA_character_,
               stringsAsFactors = FALSE)
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
