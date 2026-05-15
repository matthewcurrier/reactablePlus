# demo-05-dynamic-roster.R
#
# DEMO: Dynamic Student Roster
# Showcases: dynamic rows (source_data), display columns, selection-
#            based gating, state preservation across filter changes
#
# Run: shiny::runApp("demo-05-dynamic-roster.R")

library(shiny)
library(reactablePlus)
library(reactable)

# ── Sample data ─────────────────────────────────────────────────────────────

students <- data.frame(
  student_id = paste0("s", 1:12),
  name = c(
    "Amara Johnson", "Ben Martinez", "Clara Chen", "David Kim",
    "Elena Rossi", "Femi Adeyemi", "Grace Liu", "Hassan Ali",
    "Iris Patel", "James Wilson", "Keiko Tanaka", "Lucas Silva"
  ),
  email = paste0(
    c("amara", "ben", "clara", "david", "elena", "femi",
      "grace", "hassan", "iris", "james", "keiko", "lucas"),
    "@school.edu"
  ),
  department = c(
    "Engineering", "Engineering", "Science", "Science",
    "Arts", "Arts", "Engineering", "Science",
    "Arts", "Engineering", "Science", "Arts"
  ),
  stringsAsFactors = FALSE
)


# ── Config (dynamic mode) ──────────────────────────────────────────────────

cfg <- table_config(
  row_id_col    = "student_id",
  row_label_col = "name",

  display_cols = list(
    display_col("email", "Email", min_width = 180),
    display_col("department", "Dept", width = 120)
  ),

  columns = list(
    widget_col("status", "dropdown", "Status",
      width = 140,
      options = list(
        choices = c(
          "Active"    = "active",
          "On Leave"  = "on_leave",
          "Graduated" = "graduated"
        )
      )
    ),

    # Score is gated: only editable when the row is selected
    widget_col("score", "numeric", "Score",
      width = 90,
      options = list(min = 0, max = 100, step = 1),
      gate = list(list(type = "selected"))
    ),

    widget_col("notes", "text", "Notes",
      min_width = 160,
      options = list(
        placeholder = "Comments...",
        max_chars = 200
      )
    )
  ),

  selectable = TRUE,
  click_to_select = TRUE,
  show_reset = TRUE,

  badge_col   = "student",
  badge_label = "Student",

  to_output_fn = function(row_state, row_key) {
    data.frame(
      student_id = row_key,
      selected   = isTRUE(row_state$.selected),
      status     = row_state$status %||% NA_character_,
      score      = row_state$score %||% NA_real_,
      notes      = row_state$notes %||% "",
      stringsAsFactors = FALSE
    )
  }
)


# ── UI ──────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  titlePanel("Dynamic Student Roster"),
  p(class = "text-muted",
    "Dynamic rows from reactive source_data. Filter by department —",
    "edits survive across filter changes. Click a student name, email,",
    "or dept to toggle selection. Select a row to unlock the Score",
    "column (gating)."
  ),
  br(),
  fluidRow(
    column(4,
      selectInput("dept_filter", "Filter by Department",
        choices = c("All", sort(unique(students$department))),
        selected = "All"
      )
    ),
    column(8,
      p(class = "text-muted mt-4",
        textOutput("row_count", inline = TRUE)
      )
    )
  ),
  config_table_ui("roster", cfg),
  br(),
  h5("Live output (get_data):"),
  reactableOutput("output_tbl")
)


# ── Server ──────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Reactive source data — filtered by department
  filtered <- reactive({
    if (identical(input$dept_filter, "All")) {
      students
    } else {
      students[students$department == input$dept_filter, ]
    }
  })

  output$row_count <- renderText({
    n <- nrow(filtered())
    sprintf("Showing %d of %d students", n, nrow(students))
  })

  result <- config_table_server("roster", cfg,
    source_data = filtered
  )

  output$output_tbl <- renderReactable({
    df <- result$get_data()
    if (is.null(df) || nrow(df) == 0L) {
      return(reactable(
        data.frame(note = "Interact with the table above")
      ))
    }
    reactable(df, bordered = TRUE, striped = TRUE, compact = TRUE,
              defaultPageSize = 15)
  })
}


shinyApp(ui, server)
