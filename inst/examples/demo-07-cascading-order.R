# demo-07-cascading-order.R
#
# DEMO: Restaurant Order Builder
# Showcases: appendable mode + cascading dropdown choices.
# Pick a category, then an item (choices update dynamically),
# set quantity and special instructions. Add/remove rows freely.
#
# Run: shiny::runApp("demo-07-cascading-order.R")

library(shiny)
library(reactablePlus)
library(reactable)

# ── Menu data ───────────────────────────────────────────────────────────────

menu <- list(
  Appetizer = c(
    "Bruschetta" = "bruschetta",
    "Calamari" = "calamari",
    "Soup of the Day" = "soup",
    "Garden Salad" = "salad"
  ),
  Entree = c(
    "Grilled Salmon" = "salmon",
    "Ribeye Steak" = "ribeye",
    "Chicken Parmesan" = "chicken_parm",
    "Mushroom Risotto" = "risotto",
    "Fish & Chips" = "fish_chips"
  ),
  Dessert = c(
    "Tiramisu" = "tiramisu",
    "Cheesecake" = "cheesecake",
    "Gelato" = "gelato"
  ),
  Drink = c(
    "Sparkling Water" = "water",
    "Lemonade" = "lemonade",
    "Iced Tea" = "iced_tea",
    "Espresso" = "espresso"
  )
)

# ── Config ──────────────────────────────────────────────────────────────────

cfg <- table_config(
  appendable = TRUE,
  allow_delete = TRUE,
  min_rows = 1L,
  max_rows = 12L,
  show_reset = TRUE,

  columns = list(
    # Category — static choices
    widget_col(
      "category",
      "dropdown",
      "Category",
      width = 150,
      options = list(
        choices = c("Appetizer", "Entree", "Dessert", "Drink"),
        placeholder = "-- Category --"
      )
    ),

    # Item — cascades from category
    widget_col(
      "item",
      "dropdown",
      "Item",
      width = 180,
      options = list(
        # Dynamic choices based on the current row's category value.
        # Returns an empty list when no category is selected — the
        # dropdown shows its placeholder text in that case.
        choices_fn = function(row_state) {
          cat_val <- row_state[["category"]]
          if (is.null(cat_val) || is.na(cat_val) || !cat_val %in% names(menu)) {
            return(list())
          }
          menu[[cat_val]]
        },
        choices_depends_on = "category",
        placeholder = "-- pick a category first --"
      )
    ),

    widget_col(
      "qty",
      "numeric",
      "Qty",
      width = 80,
      options = list(min = 1, max = 20, step = 1)
    ),

    widget_col(
      "instructions",
      "text",
      "Special Instructions",
      min_width = 200,
      options = list(
        placeholder = "Allergies, modifications...",
        max_chars = 200
      )
    )
  ),

  toolbar_stats_fn = function(rows, row_keys) {
    n <- length(row_keys)
    shiny::HTML(sprintf(
      "<span style='color: #6c757d;'>%d item%s</span>",
      n,
      if (n == 1L) "" else "s"
    ))
  },

  to_output_fn = function(row_state, row_key) {
    data.frame(
      category = row_state[["category"]] %||% NA_character_,
      item = row_state[["item"]] %||% NA_character_,
      qty = row_state[["qty"]] %||% NA_real_,
      instructions = row_state[["instructions"]] %||% "",
      stringsAsFactors = FALSE
    )
  }
)


# ── UI ──────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  titlePanel("Restaurant Order Builder"),
  p(
    class = "text-muted",
    "Pick a ",
    tags$b("Category"),
    " to see matching ",
    tags$b("Items"),
    " appear in the next column (cascading choices).",
    " Add rows with ",
    tags$b("Add Row"),
    ", remove with the ",
    tags$b("\u00d7"),
    " button, or ",
    tags$b("Reset"),
    " to start over. Min 1 row, max 12."
  ),
  br(),
  config_table_ui("order", cfg),
  br(),
  h5("Order summary:"),
  reactableOutput("summary_tbl")
)


# ── Server ──────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  result <- config_table_server("order", cfg)

  output$summary_tbl <- renderReactable({
    df <- result$get_data()
    if (is.null(df) || nrow(df) == 0L) {
      return(reactable(
        data.frame(note = "Add items to your order above.")
      ))
    }

    # Look up item labels from the menu
    df$item_label <- vapply(
      seq_len(nrow(df)),
      function(i) {
        cat <- df$category[i]
        itm <- df$item[i]
        if (is.na(cat) || is.na(itm)) {
          return(NA_character_)
        }
        items <- menu[[cat]]
        if (is.null(items)) {
          return(itm)
        }
        matched <- names(items)[items == itm]
        if (length(matched) == 1L) matched else itm
      },
      character(1)
    )

    display <- data.frame(
      Category = df$category,
      Item = df$item_label,
      Qty = df$qty,
      Instructions = df$instructions,
      stringsAsFactors = FALSE
    )

    reactable(
      display,
      bordered = TRUE,
      striped = TRUE,
      compact = TRUE,
      defaultPageSize = 15
    )
  })
}


shinyApp(ui, server)
