# Characterization app for the config-driven table module.
#
# Exercises the behaviors most at risk in the store re-plumb: per-cell
# value collection (dropdown / numeric / text / checkbox), value-based
# gating (score gated by mode), mutual exclusion (mode clears detail),
# and selection. The browser test drives these and asserts get_data() /
# selected_ids() output. Being browser-driven, the test simulates a user
# and is agnostic to the per-cell vs per-column store mechanism.

library(shiny)
library(reactable)
library(reactablePlus)
library(jsonlite)

`%||%` <- function(a, b) if (is.null(a)) b else a

cfg <- table_config(
  row_keys   = c("r1", "r2"),
  row_labels = c("Row 1", "Row 2"),
  selectable = TRUE,
  show_reset = TRUE,
  columns = list(
    widget_col("mode", "dropdown", "Mode",
      options = list(choices = c("None" = "none", "A" = "a", "B" = "b"))
    ),
    widget_col("score", "numeric", "Score",
      options = list(min = 0, max = 100),
      gate = list(list(type = "value", col_id = "mode", values = c("a", "b")))
    ),
    widget_col("note", "text", "Note"),
    widget_col("flag", "checkbox", "Flag"),
    widget_col("detail", "text", "Detail")
  ),
  badge_col   = "row",
  badge_label = "Row",
  to_output_fn = function(rs, gk) {
    data.frame(
      row    = gk,
      mode   = rs$mode %||% NA_character_,
      score  = rs$score %||% NA_real_,
      note   = rs$note %||% "",
      flag   = isTRUE(rs$flag),
      detail = rs$detail %||% "",
      stringsAsFactors = FALSE
    )
  }
)

ui <- fluidPage(
  config_table_ui("tbl", cfg),
  verbatimTextOutput("data_out"),
  verbatimTextOutput("sel_out")
)

server <- function(input, output, session) {
  res <- config_table_server("tbl", cfg)
  output$data_out <- renderText(
    jsonlite::toJSON(res$get_data(), dataframe = "rows",
                     auto_unbox = TRUE, na = "null")
  )
  output$sel_out <- renderText(
    jsonlite::toJSON(res$selected_ids() %||% character(0), auto_unbox = FALSE)
  )
}

shinyApp(ui, server)
