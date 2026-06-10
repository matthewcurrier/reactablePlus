# Test app for the typed *_extra factories (numeric / checkbox / dropdown).
# Used by tests/testthat/test-cell-extras-shinytest2.R to verify JS -> R
# type coercion across the value-return contract.

library(shiny)
library(reactable)
library(reactablePlus)

df <- data.frame(
  id      = 1:2,
  qty     = c(5, 10),
  active  = c(TRUE, FALSE),
  status  = c("a", "b"),
  enabled = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  useReactablePlus(),
  reactableOutput("tbl")
)

server <- function(input, output, session) {
  output$tbl <- renderReactable({
    bindExtrasOnRender(reactable(
      df,
      columns = list(
        id = colDef(name = "ID"),
        qty = colDef(
          name = "Qty", html = TRUE, sortable = FALSE,
          cell = numeric_extra("qty", min = 0)
        ),
        active = colDef(
          name = "Active", html = TRUE, sortable = FALSE,
          cell = checkbox_extra("active")
        ),
        status = colDef(
          name = "Status", html = TRUE, sortable = FALSE,
          cell = dropdown_extra(
            "status",
            choices = c("A" = "a", "B" = "b"),
            placeholder = "--"
          )
        ),
        enabled = colDef(
          name = "Enabled", html = TRUE, sortable = FALSE,
          cell = toggle_extra("enabled")
        )
      )
    ))
  })
}

shinyApp(ui, server)
