# R/run-example.R

#' Run a reactablePlus example app
#'
#' Launches one of the bundled demo apps that showcase package features.
#' Call with no arguments to see available examples.
#'
#' @param example Character. The name of the example to run.
#'   Available examples:
#'   \describe{
#'     \item{`"inventory"`}{Product inventory tracker — all 6 primitive
#'       input types, value-based gating, reset, badge column.}
#'     \item{`"roster"`}{Team roster — row selection, chained
#'       selection + value gating, reset, selected_ids output.}
#'     \item{`"evaluations"`}{Student evaluations — attendance_picker
#'       with custom sections, notes_input, badge_render_fn,
#'       row_class_fn, mixing picker and primitive columns.}
#'   }
#' @param port Integer. Port to run the app on. Default `NULL`
#'   (Shiny picks an available port).
#'
#' @return Called for its side effect (launches a Shiny app).
#'
#' @examples
#' \dontrun{
#' # List available examples
#' runExample()
#'
#' # Launch a specific example
#' runExample("inventory")
#' runExample("roster")
#' runExample("evaluations")
#' }
#'
#' @importFrom utils menu
#' @export
runExample <- function(example = NULL, port = NULL) {
  examples_dir <- system.file("examples", package = "reactablePlus")

  if (!nzchar(examples_dir)) {
    stop(
      "Examples directory not found. ",
      "Is reactablePlus installed correctly?",
      call. = FALSE
    )
  }

  available <- list.dirs(examples_dir, full.names = FALSE, recursive = FALSE)

  if (is.null(example)) {
    message("Available reactablePlus examples:\n")
    descriptions <- c(
      inventory   = "Product inventory — primitive types, gating, reset",
      roster      = "Team roster — selection, chained gating, reset",
      evaluations = "Student evaluations — picker widgets, badge, row class"
    )
    purrr::iwalk(descriptions, function(desc, name) {
      marker <- if (name %in% available) "\u2713" else "\u2717"
      message("  ", marker, " ", name, " — ", desc)
    })
    message("\nRun with: reactablePlus::runExample(\"inventory\")")
    return(invisible(NULL))
  }

  if (!example %in% available) {
    stop(
      "Example '", example, "' not found. ",
      "Available: ", paste(available, collapse = ", "), ".",
      call. = FALSE
    )
  }

  app_dir <- file.path(examples_dir, example)

  run_args <- list(appDir = app_dir)
  if (!is.null(port)) run_args$port <- port

  do.call(shiny::runApp, run_args)
}
