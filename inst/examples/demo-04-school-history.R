# demo-04-school-history.R
#
# DEMO: School History Record
# Showcases: search_picker with server-side search, attendance_picker
#            with default sections, homeschool_picker, notes_input,
#            mutual exclusion (school vs homeschool), fill-down,
#            grade badge column with custom rendering, gear toggles,
#            grade utilities (gradeChoices, gradeInRange)
#
# This is the original domain the picker widgets were built for:
# tracking a student's school history across grade levels PreK–12.
#
# Run: shiny::runApp("demo-04-school-history.R")

library(shiny)
library(reactablePlus)
library(reactable)

# ── Fake school database for search ─────────────────────────────────────────
# In production this would query a real database (e.g., NCES school data).

fake_schools <- data.frame(
  id = c(
    "S001",
    "S002",
    "S003",
    "S004",
    "S005",
    "S006",
    "S007",
    "S008",
    "S009",
    "S010"
  ),
  name = c(
    "Lincoln Elementary",
    "Washington Middle School",
    "Jefferson High School",
    "Roosevelt Academy",
    "Adams Charter School",
    "Madison Preparatory",
    "Monroe STEM Academy",
    "Jackson Arts Magnet",
    "Harrison International",
    "Tyler Montessori"
  ),
  district = c(
    "Springfield USD",
    "Springfield USD",
    "Springfield USD",
    "Capital City Schools",
    "Capital City Schools",
    "Westside District",
    "Westside District",
    "Eastside ISD",
    "Eastside ISD",
    "Northgate Schools"
  ),
  city = c(
    "Springfield",
    "Springfield",
    "Springfield",
    "Capital City",
    "Capital City",
    "Westville",
    "Westville",
    "Eastburg",
    "Eastburg",
    "Northgate"
  ),
  state = c("IL", "IL", "IL", "IL", "IL", "IL", "IL", "IL", "IL", "IL"),
  type = c(
    "Public",
    "Public",
    "Public",
    "Public",
    "Charter",
    "Private",
    "Public",
    "Public",
    "Public",
    "Private"
  ),
  low_grade = c("PK", "06", "09", "PK", "K", "06", "K", "06", "PK", "PK"),
  high_grade = c("05", "08", "12", "12", "08", "12", "05", "12", "12", "05"),
  stringsAsFactors = FALSE
)

# Search function: filters by name, district, or city (case-insensitive)
search_schools <- function(query, limit = 25L) {
  query_lower <- tolower(query)
  matches <- fake_schools[
    grepl(query_lower, tolower(fake_schools$name)) |
      grepl(query_lower, tolower(fake_schools$district)) |
      grepl(query_lower, tolower(fake_schools$city)),
  ]
  head(matches, limit)
}


# ── Grade setup ─────────────────────────────────────────────────────────────

grades <- gradeChoices()
grade_keys <- unname(grades)
grade_labels <- names(grades)


# ── Config ──────────────────────────────────────────────────────────────────

cfg <- table_config(
  row_keys = grade_keys,
  row_labels = grade_labels,

  columns = list(
    # School picker — typeahead search with fill-down
    widget_col(
      "school",
      "search_picker",
      "School",
      min_width = 300,
      triggers_rerender = TRUE,
      options = list(
        show_nces_id = TRUE,
        trigger_label = "+ Pick school",
        popover_title = "Find school",
        show_fill_down = TRUE,
        search_placeholder = "Search by name, city, or district\u2026",
        empty_hint = "Type 2+ characters to search schools",
        no_match_hint = "No schools found. Try a different search."
      )
    ),

    # Homeschool picker — mutually exclusive with school
    widget_col(
      "homeschool",
      "homeschool_picker",
      "Homeschool",
      width = 200,
      triggers_rerender = TRUE,
      gear_toggle = "showHomeschool",
      options = list(
        trigger_label = "+ Mark homeschool",
        popover_title = "Homeschool details",
        show_curriculum = TRUE,
        show_notes = TRUE
      )
    ),

    # Attendance picker — default school attendance sections
    widget_col(
      "attendance",
      "attendance_picker",
      "Attendance",
      width = 220,
      options = list(
        trigger_label = "+ Mark attendance",
        popover_title = "Attendance",
        show_notes = TRUE,
        notes_placeholder = "Absences, tardies, context\u2026"
      )
    ),

    # Notes — free-form teacher notes
    widget_col(
      "notes",
      "notes_input",
      "Notes",
      min_width = 150,
      options = list(
        placeholder = "Optional notes\u2026"
      )
    )
  ),

  # Mutual exclusion: picking a homeschool clears the school, and vice versa
  interactions = list(
    mutual_exclusion = list(
      list(
        when_on = "homeschool",
        clears = "school",
        display = '<span style="opacity:0.5; font-style:italic;">homeschooled</span>'
      ),
      list(when_on = "school", clears = "homeschool", display = "")
    ),
    fill_down = list(
      column = "school",
      range_check_fn = function(row_key, value) {
        gradeInRange(row_key, value$low_grade, value$high_grade)
      },
      input_name = "school_fill_down"
    )
  ),

  # Search wiring: tells the server which column needs useTypeaheadSearch()
  search_fn_col = "school",

  # Grade badge column
  badge_col = "grade",
  badge_label = "Grade",
  badge_render_fn = function(row_key, row_label) {
    css_class <- paste0("grade-badge g-", row_key)
    sprintf('<span class="%s">%s</span>', css_class, row_label)
  },

  # Row styling: dim homeschooled rows
  row_class_fn = function(row_key, row_state) {
    if (!is.null(row_state$homeschool)) "is-homeschool" else NULL
  },

  # Gear toggles
  gear_toggles = list(
    showNCESId = list(label = "Show NCES ID", value = TRUE),
    showHomeschool = list(label = "Show Homeschool", value = FALSE),
    compactRows = list(label = "Compact rows", value = FALSE)
  ),

  # Toolbar stats
  toolbar_stats_fn = function(rows, row_keys) {
    filled <- sum(purrr::map_lgl(row_keys, function(gk) {
      r <- rows[[gk]]
      !is.null(r$school) || !is.null(r$homeschool)
    }))
    shiny::HTML(sprintf(
      '<span style="font-size: 0.85em; color: var(--color-text-secondary, #666);">%d / %d grades completed</span>',
      filled,
      length(row_keys)
    ))
  },

  # Output marshaling
  to_output_fn = function(row_state, row_key) {
    sch <- row_state$school
    hs <- row_state$homeschool
    att <- row_state$attendance

    data.frame(
      grade_key = row_key,
      school_name = if (is.list(sch)) {
        sch$name %||% NA_character_
      } else {
        NA_character_
      },
      school_id = if (is.list(sch)) {
        sch$id %||% NA_character_
      } else {
        NA_character_
      },
      homeschool = !is.null(hs),
      hs_provider = if (is.list(hs)) {
        hs$by %||% NA_character_
      } else {
        NA_character_
      },
      att_school = if (is.list(att)) {
        att$school %||% NA_character_
      } else {
        NA_character_
      },
      att_class = if (is.list(att)) {
        att$class_ %||% NA_character_
      } else {
        NA_character_
      },
      notes = row_state$notes %||% "",
      stringsAsFactors = FALSE
    )
  }
)


# ── UI ──────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),

  tags$style(HTML(
    "
    .is-homeschool {
      background-color: rgba(200, 220, 255, 0.2) !important;
    }
    .grade-badge {
      display: inline-block;
      padding: 2px 8px;
      border-radius: 10px;
      font-weight: 600;
      font-size: 0.85em;
      background-color: #e8e8e8;
      color: #444;
    }
  "
  )),

  titlePanel("School History Record"),
  p(
    class = "text-muted",
    "Complete school history tracker for PreK\u201312. Search and pick",
    "schools (with fill-down), mark homeschool years (mutually exclusive",
    "with school), record attendance, and add notes. Use the gear icon",
    "to toggle NCES IDs and compact mode."
  ),
  br(),
  config_table_ui("history", cfg),
  br(),
  h5("Output data:"),
  reactableOutput("output_tbl")
)


# ── Server ──────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  result <- config_table_server("history", cfg, search_fn = search_schools)

  output$output_tbl <- renderReactable({
    df <- result$get_data()
    if (nrow(df) == 0L) {
      return(reactable(data.frame(note = "Fill in the history above")))
    }
    reactable(
      df,
      bordered = TRUE,
      striped = TRUE,
      compact = TRUE,
      defaultPageSize = 14
    )
  })
}


shinyApp(ui, server)
