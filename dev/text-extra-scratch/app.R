# Throwaway app to eyeball the text_extra slice.
#
# Run from an R session at the repo root:
#   shiny::runApp("dev/text-extra-scratch")
#
# What to look for:
#   1. The "comment" and "owner" columns are editable text inputs.
#   2. The live output on the right shows input$comment / input$owner as a
#      NAMED LIST keyed by row index — seeded before you touch anything.
#   3. Edit a few cells → the list updates immediately.
#   4. RECONCILE TEST: edit a comment, then click the "Product" header to
#      sort. The edited value must STAY (not revert to the original),
#      because the JS store wins on repaint.
#
# Note: the editable columns are deliberately sortable = FALSE. reactable
# sorts by its own load-time data, which goes stale the moment you edit a
# cell (edits live in the JS store, not reactable's data) — so sorting an
# editable column would order rows by values you can no longer see. We sort
# by the non-editable "Product" column to exercise reconcile instead.

# Load the in-development package so system.file() finds inst/assets.
pkgload::load_all(
  "I:/2026/p/projects/reactableClaudePlus",
  quiet = TRUE,
  export_all = FALSE
)

library(shiny)
library(reactable)

df <- data.frame(
  id      = 1:4,
  product = c("Widget", "Gadget", "Gizmo", "Doohickey"),
  comment = c("first note", "", "needs review", ""),
  qty     = c(5, 12, 0, 3),
  due     = c("2024-01-10", "2024-03-22", "", "2024-06-01"),
  active  = c(TRUE, FALSE, TRUE, FALSE),
  status  = c("a", "b", "", "a"),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  useReactablePlus(),
  titlePanel("text_extra — scratch"),
  fluidRow(
    column(
      7,
      reactableOutput("tbl")
    ),
    column(
      5,
      tags$h4("input$comment"),
      verbatimTextOutput("comment_out"),
      tags$h4("input$qty (numeric)"),
      verbatimTextOutput("qty_out"),
      tags$h4("input$active (logical)"),
      verbatimTextOutput("active_out"),
      tags$h4("input$status (dropdown)"),
      verbatimTextOutput("status_out")
    )
  )
)

server <- function(input, output, session) {
  output$tbl <- renderReactable({
    bindExtrasOnRender(
      reactable(
        df,
        sortable = TRUE,
        columns = list(
          id      = colDef(name = "ID", width = 60, sortable = FALSE),
          product = colDef(name = "Product"), # sortable — use this to test reconcile
          comment = colDef(
            name = "Comment",
            html = TRUE,
            sortable = FALSE,
            filterable = FALSE,
            cell = text_extra("comment", placeholder = "add a note…")
          ),
          qty = colDef(
            name = "Qty", html = TRUE, sortable = FALSE, filterable = FALSE,
            cell = numeric_extra("qty", min = 0, max = 999)
          ),
          due = colDef(
            name = "Due", html = TRUE, sortable = FALSE, filterable = FALSE,
            cell = date_extra("due")
          ),
          active = colDef(
            name = "Active", html = TRUE, sortable = FALSE, filterable = FALSE,
            cell = checkbox_extra("active")
          ),
          status = colDef(
            name = "Status", html = TRUE, sortable = FALSE, filterable = FALSE,
            cell = dropdown_extra(
              "status",
              choices = c("Alpha" = "a", "Beta" = "b"),
              placeholder = "-- pick --"
            )
          )
        )
      )
    )
  })

  output$comment_out <- renderPrint(str(input$comment))
  output$qty_out     <- renderPrint(str(input$qty))
  output$active_out  <- renderPrint(str(input$active))
  output$status_out  <- renderPrint(str(input$status))
}

shinyApp(ui, server)
