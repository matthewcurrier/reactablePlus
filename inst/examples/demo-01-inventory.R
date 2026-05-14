# demo-01-inventory.R
#
# DEMO: Product Inventory Tracker
# Showcases: all 6 primitive input types, value-based gating, reset
#
# Run: shiny::runApp("demo-01-inventory.R")

library(shiny)
library(reactablePlus)
library(reactable)

# ── Config ──────────────────────────────────────────────────────────────────

cfg <- table_config(
  row_keys   = c("SKU-101", "SKU-202", "SKU-303", "SKU-404", "SKU-505"),
  row_labels = c(
    "Wireless Mouse", "Mechanical Keyboard", "USB-C Hub",
    "Monitor Stand", "Webcam HD"
  ),

  columns = list(
    # Dropdown: product status drives gating on restock_qty and restock_date
    widget_col("status", "dropdown", "Status",
      width = 130,
      options = list(
        choices = c(
          "In Stock"    = "in_stock",
          "Low Stock"   = "low_stock",
          "Out of Stock" = "out_of_stock",
          "Discontinued" = "discontinued"
        )
      )
    ),

    # Numeric: current quantity (always editable)
    widget_col("qty", "numeric", "On Hand",
      width = 90,
      options = list(min = 0, max = 9999, step = 1)
    ),

    # Numeric: restock quantity — only unlocked when status is
    # "low_stock" or "out_of_stock"
    widget_col("restock_qty", "numeric", "Restock Qty",
      width = 110,
      options = list(min = 1, max = 5000, step = 10),
      gate = list(
        list(type = "value", col_id = "status",
             values = c("low_stock", "out_of_stock"))
      )
    ),

    # Date: expected restock date — same gate as restock_qty
    widget_col("restock_date", "date", "Restock ETA",
      width = 140,
      options = list(min_date = "2025-01-01", max_date = "2027-12-31"),
      gate = list(
        list(type = "value", col_id = "status",
             values = c("low_stock", "out_of_stock"))
      )
    ),

    # Checkbox: flag for featured product
    widget_col("featured", "checkbox", "Featured",
      width = 80
    ),

    # Toggle: active / inactive
    widget_col("active", "toggle", "Active",
      width = 80
    ),

    # Text: free-form notes
    widget_col("notes", "text", "Notes",
      min_width = 180,
      options = list(
        placeholder = "Supplier, location, comments...",
        max_chars = 300
      )
    )
  ),

  badge_col   = "product",
  badge_label = "Product",

  show_reset = TRUE,

  to_output_fn = function(row_state, row_key) {
    data.frame(
      sku          = row_key,
      status       = row_state$status %||% NA_character_,
      qty          = row_state$qty %||% NA_real_,
      restock_qty  = row_state$restock_qty %||% NA_real_,
      restock_date = row_state$restock_date %||% NA_character_,
      featured     = isTRUE(row_state$featured),
      active       = isTRUE(row_state$active),
      notes        = row_state$notes %||% "",
      stringsAsFactors = FALSE
    )
  }
)


# ── UI ──────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  titlePanel("Product Inventory Tracker"),
  p(class = "text-muted",
    "All 6 primitive input types. Set Status to 'Low Stock' or",
    "'Out of Stock' to unlock the Restock Qty and ETA columns (gating)."
  ),
  br(),
  config_table_ui("inv", cfg),
  br(),
  h5("Live output (get_data):"),
  reactableOutput("output_tbl")
)


# ── Server ──────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  result <- config_table_server("inv", cfg)

  output$output_tbl <- renderReactable({
    df <- result$get_data()
    if (nrow(df) == 0L) {
      return(reactable(data.frame(note = "Interact with the table above")))
    }
    reactable(df, bordered = TRUE, striped = TRUE, compact = TRUE,
              defaultPageSize = 10)
  })
}


shinyApp(ui, server)
