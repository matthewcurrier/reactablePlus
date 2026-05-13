library(shiny)
library(reactablePlus)

roster <- data.frame(
  row_id       = c("s1_c1", "s1_c2", "s2_c1", "s2_c2", "s3_c1", "s3_c2"),
  student_name = c("Amara Johnson", "Amara Johnson",
                   "Ben Martinez",  "Ben Martinez",
                   "Clara Chen",    "Clara Chen"),
  class_name   = c("Geometry", "Literature",
                   "Geometry", "Literature",
                   "Geometry", "Literature"),
  student_id   = c(1L, 1L, 2L, 2L, 3L, 3L),
  class_id     = c(1L, 2L, 1L, 2L, 1L, 2L)
)




rows <- row_def(
  id_col = "row_id",
  display_cols = list(
    display_col("student_name", "Student"),
    display_col("class_name", "Course")
  )
)

cols <- list(
  input_col("grade", "Grade", "dropdown",
            choices = c("A+", "A", "A-",
                        "B+", "B", "B-",
                        "C+", "C", "C-",
                        "D+", "D", "D-",
                        "F")),
  input_col("notes", "Notes", "text", max_chars = 200)
)



ui <- fluidPage(
  titlePanel("Student Grade Entry"),
  editable_table_ui("grades")
)




server <- function(input, output, session) {
  editable_table_server(
    "grades",
    data_r   = reactive(roster),
    row_spec = rows,
    col_spec = cols
  )
}

shinyApp(ui, server)
