# app.R — Team Roster demo
#
# DEMO: Team Roster
# Showcases: row selection, selection-based gating, chained gates,
#            reset, selected_ids reactive, to_output_fn
#

library(shiny)
library(reactablePlus)
library(reactable)

# ── Config ──────────────────────────────────────────────────────────────────
#
# Gate chain:
#   1. "role" dropdown is LOCKED until the row is selected (checkbox)
#   2. "level" dropdown is LOCKED until selected AND role = "engineer"
#   3. "notes" text is LOCKED until selected AND role = "engineer"
#
# This demonstrates both selection gating and value gating working
# together in a chain.

cfg <- table_config(
  row_keys = c("emp_1", "emp_2", "emp_3", "emp_4", "emp_5", "emp_6"),
  row_labels = c(
    "Alice Chen", "Bob Martinez", "Carol Okafor",
    "David Kim", "Eva Novak", "Frank Abadi"
  ),

  selectable = TRUE,
  show_reset = TRUE,

  columns = list(
    # Step 1: Select the row to unlock the role dropdown
    widget_col("role", "dropdown", "Role",
      width = 140,
      options = list(
        choices = c(
          "Engineer"  = "engineer",
          "Designer"  = "designer",
          "Manager"   = "manager",
          "Analyst"   = "analyst"
        )
      ),
      gate = list(
        list(type = "selected")
      )
    ),

    # Step 2: Pick "Engineer" to unlock the level dropdown
    widget_col("level", "dropdown", "Eng Level",
      width = 120,
      options = list(
        choices = c("Junior" = "L3", "Mid" = "L4",
                    "Senior" = "L5", "Staff" = "L6",
                    "Principal" = "L7")
      ),
      gate = list(
        list(type = "selected"),
        list(type = "value", col_id = "role", values = c("engineer"))
      )
    ),

    # Step 3: Notes also gated on selected + engineer
    widget_col("notes", "text", "Notes",
      min_width = 200,
      options = list(
        placeholder = "Team, project, skills...",
        max_chars = 300
      ),
      gate = list(
        list(type = "selected"),
        list(type = "value", col_id = "role", values = c("engineer"))
      )
    )
  ),

  badge_col   = "name",
  badge_label = "Employee",

  to_output_fn = function(row_state, row_key) {
    data.frame(
      id       = row_key,
      selected = isTRUE(row_state$.selected),
      role     = row_state$role %||% NA_character_,
      level    = row_state$level %||% NA_character_,
      notes    = row_state$notes %||% "",
      stringsAsFactors = FALSE
    )
  }
)


# ── UI ──────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  titlePanel("Team Roster"),
  p(class = "text-muted",
    "Check a row to unlock Role. Set Role to 'Engineer' to unlock",
    "Eng Level and Notes (chained gating). Reset clears everything."
  ),
  br(),
  config_table_ui("roster", cfg),
  br(),
  fluidRow(
    column(4,
      h5("selected_ids:"),
      verbatimTextOutput("sel_ids")
    ),
    column(8,
      h5("get_data (gate-enforced):"),
      reactableOutput("output_tbl")
    )
  )
)


# ── Server ──────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  result <- config_table_server("roster", cfg)

  output$sel_ids <- renderPrint({
    ids <- result$selected_ids()
    if (length(ids) == 0L) cat("(none selected)")
    else cat(ids, sep = "\n")
  })

  output$output_tbl <- renderReactable({
    df <- result$get_data()
    if (nrow(df) == 0L) {
      return(reactable(data.frame(note = "Select rows above")))
    }
    reactable(df, bordered = TRUE, striped = TRUE, compact = TRUE)
  })
}


shinyApp(ui, server)

