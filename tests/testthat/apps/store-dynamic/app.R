# Dynamic mode through the store: rows are derived from source_data. Editing a
# cell then changing source_data must preserve the edited rows' values (synced
# to the store via .sync_store_cells) and seed any new rows.

library(shiny)
library(reactable)
library(reactablePlus)
library(jsonlite)

`%||%` <- function(a, b) if (is.null(a)) b else a

cfg <- table_config(
  row_id_col    = "id",
  row_label_col = "name",
  columns = list(
    widget_col("status", "dropdown", "Status",
      options = list(choices = c("Active" = "a", "Inactive" = "i"))
    ),
    widget_col("score", "numeric", "Score", options = list(min = 0, max = 100))
  ),
  to_output_fn = function(rs, gk) {
    data.frame(
      id     = gk,
      status = rs$status %||% NA_character_,
      score  = rs$score %||% NA_real_,
      stringsAsFactors = FALSE
    )
  }
)

ui <- fluidPage(
  actionButton("more", "Add Carol"),
  config_table_ui("t", cfg),
  verbatimTextOutput("o")
)

server <- function(input, output, session) {
  src <- reactiveVal(
    data.frame(id = c("p1", "p2"), name = c("Alice", "Bob"),
               stringsAsFactors = FALSE)
  )
  observeEvent(input$more, {
    src(data.frame(
      id   = c("p1", "p2", "p3"),
      name = c("Alice", "Bob", "Carol"),
      stringsAsFactors = FALSE
    ))
  })

  r <- config_table_server("t", cfg, source_data = src)
  output$o <- renderText(
    jsonlite::toJSON(r$get_data(), dataframe = "rows",
                     auto_unbox = TRUE, na = "null")
  )
}

shinyApp(ui, server)
