library(shiny)
library(reactablePlus)


# The data frame drives the rows — must contain the id column
# and any display columns declared in row_spec.
# my_data <- data.frame(
#   id   = c("row1", "row2", "row3"),
#   name = c("Alice", "Bob", "Carol")
# )

mtcars$id <- rownames(mtcars)
row.names(mtcars) <- NULL
my_data <- mtcars

row_spec <- list(
  id_col = "id",
  display_cols = list(
    list(col_name = "qsec", label = "qsec"),
    list(col_name = "cyl", label = "cylinder")
  )
)

col_spec <- list(
  list(
    col_name = "disp_cat",
    label = "Displacement Category",
    type = "dropdown",
    choices = c("High" = "high", "Medium" = "medium", "Low" = "low")
  ),
  list(
    col_name = "score",
    label = "Score",
    type = "numeric",
    min = 0,
    max = 100,
    step = 1
  ),
  list(
    col_name = "active",
    label = "Active",
    type = "checkbox"
  ),
  list(
    col_name = "assessment",
    label = "Assessment",
    type = "dropdown",
    choices = list(
      list(label = "good", value = 3),
      list(label = "meh", value = 2),
      list(label = "bad", value = 1)
    )
  )
)

ui <- fluidPage(
  editable_table_ui("demo")
)

server <- function(input, output, session) {
  editable_table_server(
    "demo",
    data_r = reactive(my_data),
    row_spec = row_spec,
    col_spec = col_spec
  )
}

shinyApp(ui, server)
