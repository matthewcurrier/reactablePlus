# Test app for tests/testthat/test-cell-extras-shinytest2.R
# Exercises a single text_extra column so the browser test can check the
# value-return contract and reconcile-on-repaint behavior.

library(shiny)
library(reactable)
library(reactablePlus)

df <- data.frame(
  product = c("Widget", "Gadget", "Gizmo"),
  comment = c("alpha", "beta", "gamma"),
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
        product = colDef(name = "Product"),
        comment = colDef(
          name = "Comment",
          html = TRUE,
          sortable = FALSE,
          cell = text_extra("comment")
        )
      )
    ))
  })
}

shinyApp(ui, server)
