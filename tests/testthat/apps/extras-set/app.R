# Test app for the server -> store channel (update_extra / rp_extras_set).
# A button sets cell values from the server; the test verifies the store,
# the reported input, and the live DOM all reflect the server-set values.

library(shiny)
library(reactable)
library(reactablePlus)

df <- data.frame(
  comment = c("a", "b"),
  qty     = c(1, 2),
  active  = c(FALSE, FALSE),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  useReactablePlus(),
  actionButton("set", "Set from server"),
  reactableOutput("tbl")
)

server <- function(input, output, session) {
  output$tbl <- renderReactable({
    bindExtrasOnRender(reactable(
      df,
      columns = list(
        comment = colDef(
          html = TRUE, sortable = FALSE, cell = text_extra("comment")
        ),
        qty = colDef(
          html = TRUE, sortable = FALSE, cell = numeric_extra("qty", min = 0)
        ),
        active = colDef(
          html = TRUE, sortable = FALSE, cell = checkbox_extra("active")
        )
      )
    ))
  })

  observeEvent(input$set, {
    update_extra(session, "comment", list(`2` = "server-set"))
    update_extra(session, "qty", list(`1` = 99))
    update_extra(session, "active", list(`1` = TRUE))
  })
}

shinyApp(ui, server)
