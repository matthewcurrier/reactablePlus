# Appendable mode through the store: rows the user adds at runtime must edit
# correctly via the once-wired per-column store observer, and Reset must clear
# them.

library(shiny)
library(reactable)
library(reactablePlus)
library(jsonlite)

`%||%` <- function(a, b) if (is.null(a)) b else a

cfg <- table_config(
  appendable   = TRUE,
  min_rows     = 1L,
  max_rows     = 4L,
  show_reset   = TRUE,
  allow_delete = TRUE,
  columns = list(
    widget_col("fruit", "dropdown", "Fruit",
      options = list(choices = c("Apple" = "a", "Banana" = "b"))
    ),
    widget_col("qty", "numeric", "Qty", options = list(min = 0, max = 99))
  ),
  to_output_fn = function(rs, gk) {
    data.frame(
      fruit = rs$fruit %||% NA_character_,
      qty   = rs$qty %||% NA_real_,
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
