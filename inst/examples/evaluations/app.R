# app.R — Student Evaluations demo
#
# DEMO: Student Evaluations
# Showcases: attendance_picker with custom sections, notes_input picker,
#            badge_col with badge_render_fn, row_class_fn,
#            mixing picker widgets with primitive inputs
#

library(shiny)
library(reactablePlus)
library(reactable)

# ── Config ──────────────────────────────────────────────────────────────────
#
# Each student gets:
#   - A "Behavior" picker (attendance_picker with custom sections for
#     classroom behavior and participation — not attendance at all!)
#   - A "Conduct" dropdown (primitive)
#   - A "Notes" picker widget (notes_input)
#
# This demonstrates that attendance_picker is reusable for any
# multi-field rating scenario, not just school attendance.

behavior_sections <- list(
  list(
    key    = "classroom",
    label  = "Classroom Behavior",
    levels = c("Exemplary", "Meets Expectations",
               "Needs Improvement", "Unsatisfactory")
  ),
  list(
    key         = "participation",
    label       = "Class Participation",
    levels      = c("Active", "Moderate", "Minimal", "None"),
    pill_prefix = "Part: ",
    pill_icon   = "pencil"
  ),
  list(
    key            = "concern",
    label          = "Requires parent conference?",
    levels         = c("Yes", "No"),
    impact_display = list(
      Yes = "\u26A0 conference recommended",
      No  = "no conference needed"
    )
  )
)

students <- data.frame(
  id    = paste0("s", 1:8),
  name  = c("Amara Johnson", "Ben Martinez", "Clara Chen", "David Kim",
            "Eva Novak", "Frank Abadi", "Grace Liu", "Henry Park"),
  grade = c("9th", "9th", "10th", "10th", "11th", "11th", "12th", "12th"),
  stringsAsFactors = FALSE
)

cfg <- table_config(
  row_keys   = students$id,
  row_labels = students$name,

  columns = list(
    # Behavior assessment — attendance_picker with custom sections
    widget_col("behavior", "attendance_picker", "Behavior",
      width = 220,
      options = list(
        sections          = behavior_sections,
        trigger_label     = "+ Assess behavior",
        popover_title     = "Behavior Assessment",
        show_notes        = TRUE,
        notes_placeholder = "Specific incidents, patterns, context\u2026"
      )
    ),

    # Conduct — a simple dropdown (primitive input mixed with pickers)
    widget_col("conduct", "dropdown", "Conduct Grade",
      width = 130,
      options = list(
        choices = c("Outstanding", "Satisfactory", "Needs Improvement", "Unacceptable")
      )
    ),

    # Notes — notes_input picker widget
    widget_col("notes", "notes_input", "Teacher Notes",
      min_width = 180,
      options = list(
        placeholder = "Comments for report card\u2026"
      )
    )
  ),

  badge_col   = "student",
  badge_label = "Student",

  # Custom badge rendering: show name with grade as a subtle tag
  badge_render_fn = function(row_key, row_label) {
    grade <- students$grade[students$id == row_key]
    sprintf(
      '<div style="line-height: 1.3;">
         <strong>%s</strong>
         <br><span style="font-size: 0.8em; color: var(--color-text-tertiary, #888);">%s</span>
       </div>',
      htmltools::htmlEscape(row_label),
      htmltools::htmlEscape(grade)
    )
  },

  # Custom row class: highlight rows where a parent conference is needed
  row_class_fn = function(row_key, row_state) {
    beh <- row_state$behavior
    if (is.list(beh) && identical(beh$concern, "Yes")) {
      "conference-needed"
    } else {
      NULL
    }
  },

  to_output_fn = function(row_state, row_key) {
    beh <- row_state$behavior
    data.frame(
      student_id    = row_key,
      classroom     = if (is.list(beh)) beh$classroom %||% NA_character_ else NA_character_,
      participation = if (is.list(beh)) beh$participation %||% NA_character_ else NA_character_,
      conference    = if (is.list(beh)) beh$concern %||% NA_character_ else NA_character_,
      conduct       = row_state$conduct %||% NA_character_,
      notes         = row_state$notes %||% "",
      stringsAsFactors = FALSE
    )
  }
)


# ── UI ──────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  tags$style(HTML("
    .conference-needed {
      background-color: rgba(255, 200, 200, 0.3) !important;
    }
    @media (prefers-color-scheme: dark) {
      .conference-needed {
        background-color: rgba(180, 60, 60, 0.2) !important;
      }
    }
  ")),

  titlePanel("Student Behavior Evaluations"),
  p(class = "text-muted",
    "The Behavior column uses an attendance_picker with custom sections",
    "for classroom behavior, participation, and conference flagging.",
    "Rows with 'conference recommended' are highlighted in red.",
    "The badge shows the student name with grade level beneath."
  ),
  br(),
  config_table_ui("evals", cfg),
  br(),
  h5("Evaluation data:"),
  reactableOutput("output_tbl")
)


# ── Server ──────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  result <- config_table_server("evals", cfg)

  output$output_tbl <- renderReactable({
    df <- result$get_data()
    if (nrow(df) == 0L) {
      return(reactable(data.frame(note = "Assess students above")))
    }
    reactable(df, bordered = TRUE, striped = TRUE, compact = TRUE)
  })
}


shinyApp(ui, server)

