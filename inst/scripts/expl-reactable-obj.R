library(reactable)


cr <- reactable(mtcars)

class(cr)
str(cr)


# See the JSON payload that goes to the browser
cr$x$tag$attribs$data |> jsonlite::prettify()

# See the full generated HTML
htmltools::renderTags(cr)
# or save and inspect in your editor:
htmlwidgets::saveWidget(cr, "cr.html", selfcontained = FALSE)

# Modify a widget after creation (it's just a list!)
cr$x$tag$attribs$columns[[2]]$name <- "Miles per Gallon"
cr
