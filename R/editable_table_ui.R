# R/editable_table_ui.R

#' UI function for the editable table Shiny module
#'
#' @param id A string. The module namespace ID.
#' @return A named list of \code{shiny.tag} objects.
#' @export
editable_table_ui <- function(id) {
  ns <- shiny::NS(id)

  list(
    table = shiny::tagList(
      # Custom message handler that directly manipulates the DOM on reset.
      # This is necessary because:
      #   - renderReactable preserves existing custom HTML cells across
      #     re-renders rather than recreating them
      #   - session$sendInputMessage requires a Shiny input binding which
      #     raw HTML inputs do not have
      # Direct DOM manipulation is the only reliable way to clear custom
      # HTML inputs without tearing down the entire reactable widget.
      shiny::tags$script(shiny::HTML(
        "Shiny.addCustomMessageHandler('editableTableReset', function(data) {
          data.inputs.forEach(function(item) {
            var el = document.getElementById(item.id);
            if (!el) return;

            // Toggle buttons need text and background updated
            if (el.tagName === 'BUTTON') {
              el.textContent = item.value ? 'On' : 'Off';
              el.style.backgroundColor = item.value ? '#28a745' : '#6c757d';
              var hidden = document.getElementById(item.id.replace(/_btn$/, ''));
              if (hidden) hidden.value = String(item.value);
            } else if (el.type === 'checkbox') {
              el.checked = item.value === true;
            } else {
              el.value = (item.value === null || item.value === undefined)
                ? '' : item.value;
            }

            // Apply locked / unlocked visual state
            if (item.locked) {
              el.disabled = true;
              el.style.opacity = '0.4';
              el.style.cursor = 'not-allowed';
              el.style.pointerEvents = 'none';
            } else {
              el.disabled = false;
              el.style.opacity = '1';
              el.style.cursor = 'default';
              el.style.pointerEvents = '';
            }
          });
        });"
      )),
      reactable::reactableOutput(ns("table"))
    ),

    reset_button = shiny::actionButton(
      inputId = ns("reset"),
      label = "Reset",
      class = "btn btn-warning"
    ),

    data_display = reactable::reactableOutput(ns("data_display"))
  )
}
