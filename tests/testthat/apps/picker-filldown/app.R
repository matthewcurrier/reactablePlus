# Characterization app for search_picker fill-down through the *_extra store.
# Picking a school on the first row and firing the fill-down input must
# cascade the value into every empty in-range row via update_extra -> the JS
# picker write handler (setValue). This is the routing the store port changed
# (sendInputMessage -> update_extra), so it is pinned here.

library(shiny)
library(reactable)
library(reactablePlus)
library(jsonlite)

`%||%` <- function(a, b) if (is.null(a)) b else a

schools <- data.frame(
  id = c("S1", "S2"),
  name = c("Alpha Academy", "Beta School"),
  low_grade = c("r1", "r1"),
  high_grade = c("r4", "r4"),
  stringsAsFactors = FALSE
)

search_fn <- function(query, limit = 25L) {
  head(schools[grepl(tolower(query), tolower(schools$name)), ], limit)
}

cfg <- table_config(
  row_keys   = c("r1", "r2", "r3", "r4"),
  row_labels = c("R1", "R2", "R3", "R4"),
  columns = list(
    widget_col(
      "school", "search_picker", "School",
      triggers_rerender = TRUE,
      options = list(show_fill_down = TRUE, trigger_label = "+ Pick school")
    )
  ),
  interactions = list(
    fill_down = list(
      column = "school",
      input_name = "school_fill_down",
      # Every row is in range for this fixture.
      range_check_fn = function(row_key, value) TRUE
    )
  ),
  search_fn_col = "school",
  badge_col   = "row",
  badge_label = "Row",
  to_output_fn = function(rs, gk) {
    v <- rs$school
    data.frame(
      row    = gk,
      school = if (is.list(v)) (v$name %||% NA_character_) else NA_character_,
      stringsAsFactors = FALSE
    )
  }
)

ui <- fluidPage(
  config_table_ui("t", cfg),
  verbatimTextOutput("o")
)

server <- function(input, output, session) {
  r <- config_table_server("t", cfg, search_fn = search_fn)
  output$o <- renderText(
    jsonlite::toJSON(r$get_data(), dataframe = "rows",
                     auto_unbox = TRUE, na = "null")
  )
}

shinyApp(ui, server)
