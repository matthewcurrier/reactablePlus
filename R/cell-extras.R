# =============================================================================
# cell-extras.R
#
# Cell-level editable input factories for reactable — the *_extra family.
#
# Each factory returns a reactable cell renderer: a function(value, index,
# name) that emits an inert HTML control carrying data attributes. Editing
# is wired by the shared store binding in inst/assets/js/reactable-extras.js,
# which reports each column under a single Shiny input as a named list keyed
# by row index (see ?text_extra for the value-return contract).
#
# These factories require three things in the host app:
#   - useReactablePlus() at the page level (reactable drops cell-returned
#     htmlDependency objects, so the JS must be injected once at page level)
#   - colDef(html = TRUE) on the column
#   - bindExtrasOnRender() on the reactable widget
# =============================================================================

# Internal cell "bricks" shared by the *_extra factories AND the config
# table module, so each cell type's HTML lives in exactly one place. Each
# takes an explicit `row` (the value placed in data-row — a reactable index
# for raw factory use, or a row key when the module renders) plus optional
# `attrs` (e.g. disabled + dim style for a locked cell) and extra `class`.

# <input>-based cell reporting via its `value` (text/date) or parsed value
# (numeric).
.extra_value_cell <- function(
  input_id,
  rp_type,
  html_type,
  value,
  row,
  attrs = list(),
  class = NULL
) {
  val <- if (is.null(value) || (length(value) == 1L && is.na(value))) {
    ""
  } else {
    as.character(value)[1L]
  }
  args <- c(
    list(
      type = html_type,
      class = paste(c("rp-extra", class), collapse = " "),
      value = val
    ),
    attrs,
    list(
      `data-input-id` = input_id,
      `data-row` = row,
      `data-rp-type` = rp_type
    )
  )
  as.character(do.call(tags$input, args))
}

# Checkbox cell — reports a logical via el.checked.
.extra_checkbox_cell <- function(
  input_id,
  value,
  row,
  attrs = list(),
  class = NULL
) {
  checked <- isTRUE(as.logical(value))
  args <- c(
    list(
      type = "checkbox",
      class = paste(c("rp-extra", class), collapse = " ")
    ),
    attrs,
    if (checked) list(checked = NA),
    list(
      `data-input-id` = input_id,
      `data-row` = row,
      `data-rp-type` = "checkbox"
    )
  )
  as.character(do.call(tags$input, args))
}

# Select cell — reports the selected option value (string). `choices` must
# already be normalized (list of list(label, value)).
.extra_select_cell <- function(
  input_id,
  choices,
  current,
  row,
  placeholder = NULL,
  attrs = list(),
  class = NULL
) {
  cur <- if (is.null(current) || (length(current) == 1L && is.na(current))) {
    ""
  } else {
    as.character(current)[1L]
  }
  opts <- map(choices, function(ch) {
    tags$option(
      value = as.character(ch$value),
      selected = if (identical(as.character(ch$value), cur)) NA else NULL,
      ch$label
    )
  })
  if (!is.null(placeholder)) {
    ph <- tags$option(
      value = "",
      selected = if (!nzchar(cur)) NA else NULL,
      disabled = NA,
      placeholder
    )
    opts <- c(list(ph), opts)
  }
  args <- c(
    list(class = paste(c("rp-extra", class), collapse = " ")),
    attrs,
    list(
      `data-input-id` = input_id,
      `data-row` = row,
      `data-rp-type` = "dropdown"
    ),
    list(opts)
  )
  as.character(do.call(tags$select, args))
}

# Toggle cell — a button that flips a boolean; reports a logical. The store
# binding owns the click/flip behavior (no inline JS here). On/off labels
# travel in data attributes so the binding can relabel on change.
.extra_toggle_cell <- function(
  input_id,
  value,
  row,
  on_label = "On",
  off_label = "Off",
  attrs = list(),
  class = NULL
) {
  on <- isTRUE(as.logical(value))
  args <- c(
    list(
      type = "button",
      class = paste(c("rp-extra", "rp-toggle", class), collapse = " "),
      `aria-pressed` = if (on) "true" else "false",
      `data-input-id` = input_id,
      `data-row` = row,
      `data-rp-type` = "toggle",
      `data-rp-value` = if (on) "true" else "false",
      `data-on-label` = on_label,
      `data-off-label` = off_label
    ),
    attrs,
    list(if (on) on_label else off_label)
  )
  as.character(do.call(tags$button, args))
}


#' Editable text cell for reactable
#'
#' Returns a \pkg{reactable} cell renderer that draws an editable text
#' input in every cell of a column. Edits are reported to the Shiny
#' server under a single input named by `col_id` (prefixed by `ns`), as a
#' named list keyed by row index.
#'
#' @section Value-return contract:
#' The column reports under one Shiny input, `paste0(ns, col_id)`. Its
#' value is a named list keyed by the reactable row index (original data
#' order), seeded with the starting cell values at render and updated as
#' the user edits:
#'
#' \preformatted{
#'   input$comment   # list(`1` = "first", `2` = "second", `3` = "third")
#' }
#'
#' Call `unlist(input$comment)` for a plain character vector. Keying by
#' row index (rather than DOM position) keeps the mapping correct when
#' \pkg{reactable} sorts or filters.
#'
#' @section Sorting and filtering:
#' Edited values live in a JS-side store, not in \pkg{reactable}'s data.
#' \pkg{reactable} sorts and filters by its original load-time values, so
#' an editable column would order rows by data the user can no longer see
#' once they start editing. Set `sortable = FALSE` (and `filterable =
#' FALSE`) on `*_extra` columns to avoid this; sort and filter on the
#' table's read-only columns instead.
#'
#' @section Required wiring:
#' Because \pkg{reactable} renders cells with React and ignores
#' [htmltools::htmlDependency] objects returned from cell functions, the
#' supporting JS must be loaded at the page level, the column must opt
#' into HTML cells, and the table must attach the render hook:
#'
#' \preformatted{
#'   ui <- fluidPage(useReactablePlus(), reactableOutput("tbl"))
#'
#'   output$tbl <- renderReactable({
#'     bindExtrasOnRender(reactable(df, columns = list(
#'       comment = colDef(html = TRUE,
#'                        cell = text_extra("comment", ns = session$ns("")))
#'     )))
#'   })
#' }
#'
#' @param col_id `character(1)`. Column identifier. Becomes the Shiny
#'   input name (prefixed by `ns`).
#' @param ns `character(1)`. Module namespace prefix. Pass
#'   `session$ns("")` inside a Shiny module; leave `""` at the top level.
#' @param placeholder `character(1)`. Placeholder shown in empty cells.
#'   Default `""`.
#' @param class `character(1)` or `NULL`. Extra CSS class(es) added to
#'   the `<input>` element alongside `rp-extra`. Default `NULL`.
#'
#' @return A function `(value, index, name)` suitable for
#'   `reactable::colDef(cell = )`. The column's `colDef` must set
#'   `html = TRUE`.
#'
#' @seealso [bindExtrasOnRender()], [useReactablePlus()]
#'
#' @examples
#' \dontrun{
#' reactable::colDef(
#'   html = TRUE,
#'   cell = text_extra("comment", ns = session$ns(""))
#' )
#' }
#'
#' @importFrom htmltools tags
#' @export
text_extra <- function(col_id, ns = "", placeholder = "", class = NULL) {
  stopifnot(
    is.character(col_id),
    length(col_id) == 1L,
    nzchar(col_id),
    is.character(ns),
    length(ns) == 1L
  )
  input_id <- paste0(ns, col_id)

  function(value, index, name = NULL) {
    .extra_value_cell(
      input_id,
      rp_type = "text",
      html_type = "text",
      value = value,
      row = index,
      attrs = list(placeholder = placeholder),
      class = class
    )
  }
}


#' Editable numeric cell for reactable
#'
#' Like [text_extra()] but renders an `<input type="number">`. Edited
#' values are reported as numbers (empty cells report as `NULL`).
#'
#' @inheritParams text_extra
#' @inheritSection text_extra Value-return contract
#' @inheritSection text_extra Required wiring
#' @inheritSection text_extra Sorting and filtering
#' @param min,max Numeric or `NULL`. Optional bounds passed to the
#'   `<input>` `min` / `max` attributes. Default `NULL`.
#' @param step Numeric or `NULL`. Optional `step` attribute (e.g. `0.5`
#'   or `"any"`). Default `NULL`.
#'
#' @return A function `(value, index, name)` for
#'   `reactable::colDef(cell = )`; the `colDef` must set `html = TRUE`.
#'
#' @seealso [text_extra()], [bindExtrasOnRender()]
#'
#' @examples
#' \dontrun{
#' reactable::colDef(
#'   html = TRUE,
#'   cell = numeric_extra("qty", min = 0, max = 100, ns = session$ns(""))
#' )
#' }
#'
#' @importFrom htmltools tags
#' @importFrom purrr compact
#' @export
numeric_extra <- function(
  col_id,
  ns = "",
  min = NULL,
  max = NULL,
  step = NULL,
  placeholder = "",
  class = NULL
) {
  stopifnot(
    is.character(col_id),
    length(col_id) == 1L,
    nzchar(col_id),
    is.character(ns),
    length(ns) == 1L
  )
  input_id <- paste0(ns, col_id)

  function(value, index, name = NULL) {
    .extra_value_cell(
      input_id,
      rp_type = "numeric",
      html_type = "number",
      value = value,
      row = index,
      attrs = compact(list(
        min = min,
        max = max,
        step = step,
        placeholder = if (nzchar(placeholder)) placeholder else NULL
      )),
      class = class
    )
  }
}


#' Editable date cell for reactable
#'
#' Like [text_extra()] but renders an `<input type="date">`. Values are
#' reported as `YYYY-MM-DD` strings (empty cells report as `NULL`).
#'
#' @inheritParams text_extra
#' @inheritSection text_extra Value-return contract
#' @inheritSection text_extra Required wiring
#' @inheritSection text_extra Sorting and filtering
#' @param min_date,max_date `Date`, `YYYY-MM-DD` string, or `NULL`.
#'   Optional bounds passed to the `<input>` `min` / `max` attributes.
#'   Default `NULL`.
#'
#' @return A function `(value, index, name)` for
#'   `reactable::colDef(cell = )`; the `colDef` must set `html = TRUE`.
#'
#' @seealso [text_extra()], [bindExtrasOnRender()]
#'
#' @examples
#' \dontrun{
#' reactable::colDef(
#'   html = TRUE,
#'   cell = date_extra("registered", ns = session$ns(""))
#' )
#' }
#'
#' @importFrom htmltools tags
#' @importFrom purrr compact
#' @export
date_extra <- function(
  col_id,
  ns = "",
  min_date = NULL,
  max_date = NULL,
  class = NULL
) {
  stopifnot(
    is.character(col_id),
    length(col_id) == 1L,
    nzchar(col_id),
    is.character(ns),
    length(ns) == 1L
  )
  input_id <- paste0(ns, col_id)

  function(value, index, name = NULL) {
    .extra_value_cell(
      input_id,
      rp_type = "date",
      html_type = "date",
      value = value,
      row = index,
      attrs = compact(list(
        min = if (!is.null(min_date)) as.character(min_date),
        max = if (!is.null(max_date)) as.character(max_date)
      )),
      class = class
    )
  }
}


#' Editable checkbox cell for reactable
#'
#' Like [text_extra()] but renders an `<input type="checkbox">`. Values
#' are reported as logicals (`TRUE` / `FALSE`).
#'
#' @inheritParams text_extra
#' @inheritSection text_extra Value-return contract
#' @inheritSection text_extra Required wiring
#' @inheritSection text_extra Sorting and filtering
#'
#' @return A function `(value, index, name)` for
#'   `reactable::colDef(cell = )`; the `colDef` must set `html = TRUE`.
#'
#' @seealso [text_extra()], [bindExtrasOnRender()]
#'
#' @examples
#' \dontrun{
#' reactable::colDef(
#'   html = TRUE,
#'   cell = checkbox_extra("active", ns = session$ns(""))
#' )
#' }
#'
#' @importFrom htmltools tags
#' @export
checkbox_extra <- function(col_id, ns = "", class = NULL) {
  stopifnot(
    is.character(col_id),
    length(col_id) == 1L,
    nzchar(col_id),
    is.character(ns),
    length(ns) == 1L
  )
  input_id <- paste0(ns, col_id)

  function(value, index, name = NULL) {
    .extra_checkbox_cell(input_id, value, index, class = class)
  }
}


#' Editable dropdown cell for reactable
#'
#' Like [text_extra()] but renders a `<select>` populated from `choices`.
#' The selected option's value is reported as a string.
#'
#' @inheritParams text_extra
#' @inheritSection text_extra Value-return contract
#' @inheritSection text_extra Required wiring
#' @inheritSection text_extra Sorting and filtering
#' @param choices Choices for the dropdown, in any format accepted by
#'   [normalize_choices()]: an unnamed character vector, a named
#'   character vector (names are labels), or a list of
#'   `list(label, value)`.
#' @param placeholder `character(1)` or `NULL`. When supplied, a disabled
#'   empty option shown when the cell has no value. Recommended so empty
#'   cells seed as `""` rather than the browser's default first option.
#'   Default `NULL`.
#'
#' @return A function `(value, index, name)` for
#'   `reactable::colDef(cell = )`; the `colDef` must set `html = TRUE`.
#'
#' @seealso [text_extra()], [normalize_choices()], [bindExtrasOnRender()]
#'
#' @examples
#' \dontrun{
#' reactable::colDef(
#'   html = TRUE,
#'   cell = dropdown_extra("status", choices = c("Active", "Inactive"),
#'                         placeholder = "-- pick --", ns = session$ns(""))
#' )
#' }
#'
#' @importFrom htmltools tags
#' @importFrom purrr map
#' @export
dropdown_extra <- function(
  col_id,
  choices,
  ns = "",
  placeholder = NULL,
  class = NULL
) {
  stopifnot(
    is.character(col_id),
    length(col_id) == 1L,
    nzchar(col_id),
    is.character(ns),
    length(ns) == 1L
  )
  norm <- normalize_choices(choices)
  input_id <- paste0(ns, col_id)

  function(value, index, name = NULL) {
    .extra_select_cell(
      input_id, norm, value, index,
      placeholder = placeholder, class = class
    )
  }
}


#' Editable toggle (on/off button) cell for reactable
#'
#' Like [text_extra()] but renders a button that flips between two states.
#' The value is reported as a logical (`TRUE` = on).
#'
#' @inheritParams text_extra
#' @inheritSection text_extra Value-return contract
#' @inheritSection text_extra Required wiring
#' @inheritSection text_extra Sorting and filtering
#' @param on_label,off_label `character(1)`. Button text in each state.
#'   Defaults `"On"` / `"Off"`.
#'
#' @return A function `(value, index, name)` for
#'   `reactable::colDef(cell = )`; the `colDef` must set `html = TRUE`.
#'
#' @seealso [text_extra()], [checkbox_extra()], [bindExtrasOnRender()]
#'
#' @examples
#' \dontrun{
#' reactable::colDef(
#'   html = TRUE,
#'   cell = toggle_extra("enabled", ns = session$ns(""))
#' )
#' }
#'
#' @importFrom htmltools tags
#' @export
toggle_extra <- function(
  col_id,
  ns = "",
  on_label = "On",
  off_label = "Off",
  class = NULL
) {
  stopifnot(
    is.character(col_id),
    length(col_id) == 1L,
    nzchar(col_id),
    is.character(ns),
    length(ns) == 1L
  )
  input_id <- paste0(ns, col_id)

  function(value, index, name = NULL) {
    .extra_toggle_cell(
      input_id, value, index,
      on_label = on_label, off_label = off_label, class = class
    )
  }
}


#' Editable notes (single-line text) cell for reactable
#'
#' A thin variant of [text_extra()] for free-text notes: it adds the
#' `cell-input` class (so it inherits the package's inline-input styling)
#' and is otherwise identical — values are reported as strings.
#'
#' @inheritParams text_extra
#' @inheritSection text_extra Value-return contract
#' @inheritSection text_extra Required wiring
#' @inheritSection text_extra Sorting and filtering
#'
#' @return A function `(value, index, name)` for
#'   `reactable::colDef(cell = )`; the `colDef` must set `html = TRUE`.
#'
#' @seealso [text_extra()], [bindExtrasOnRender()]
#'
#' @examples
#' \dontrun{
#' reactable::colDef(
#'   html = TRUE,
#'   cell = notes_extra("comment", placeholder = "Optional", ns = session$ns(""))
#' )
#' }
#'
#' @importFrom htmltools tags
#' @export
notes_extra <- function(col_id, ns = "", placeholder = "", class = NULL) {
  stopifnot(
    is.character(col_id),
    length(col_id) == 1L,
    nzchar(col_id),
    is.character(ns),
    length(ns) == 1L
  )
  input_id <- paste0(ns, col_id)

  function(value, index, name = NULL) {
    .extra_value_cell(
      input_id,
      rp_type = "text",
      html_type = "text",
      value = value,
      row = index,
      attrs = list(placeholder = placeholder),
      class = c("cell-input", class)
    )
  }
}


#' Set `*_extra` cell values from the server
#'
#' Pushes authoritative values into the client-side store for an
#' `*_extra` column, updating both the store and the live DOM. Use this
#' when the **server** needs to change a cell's value — e.g. to clear a
#' cell, copy a value down, or reset a column — so the change survives
#' reactable's repaints instead of being overruled by the store's
#' "store wins on repaint" reconcile.
#'
#' @param session The Shiny session object. Defaults to the current
#'   reactive domain.
#' @param col_id `character(1)`. The column id used when the cells were
#'   created with a factory such as [text_extra()].
#' @param values A named list or named vector keyed by **row id**
#'   (matching the cells' `data-row`), each entry the new value for that
#'   row. Rows not named are left unchanged.
#' @param ns `character(1)`. The same namespace prefix passed to the
#'   factory (e.g. `session$ns("")` inside a module). Default `""`.
#'
#' @return Called for its side effect; returns invisibly.
#'
#' @seealso [text_extra()], [bindExtrasOnRender()]
#'
#' @examples
#' \dontrun{
#' # Clear row 2 and set row 3, server-side:
#' update_extra(session, "comment", list(`2` = "", `3` = "set by server"))
#' }
#'
#' @importFrom shiny getDefaultReactiveDomain
#' @export
update_extra <- function(
  session = shiny::getDefaultReactiveDomain(),
  col_id,
  values,
  ns = ""
) {
  stopifnot(
    is.character(col_id),
    length(col_id) == 1L,
    nzchar(col_id),
    is.character(ns),
    length(ns) == 1L
  )
  if (is.null(session)) {
    stop("update_extra() must be called within a Shiny session.", call. = FALSE)
  }
  session$sendCustomMessage(
    "rp_extras_set",
    list(
      inputId = paste0(ns, col_id),
      values = as.list(values)
    )
  )
  invisible(NULL)
}


#' Wire `*_extra` cell inputs after a reactable renders
#'
#' Attaches an `onRender` hook that initializes the shared store binding
#' for [text_extra()] (and the rest of the `*_extra` family). The hook
#' seeds each column's Shiny input from the rendered cells and keeps the
#' values reconciled across \pkg{reactable} repaints (sort, filter,
#' paginate), so user edits are not visually lost when reactable
#' re-renders a cell from its own data.
#'
#' @param widget A \pkg{reactable} widget (the return value of
#'   [reactable::reactable()]).
#' @param fallback_ms Integer. Safety-net timeout (ms) for the initial
#'   reconcile if the `MutationObserver` never fires (e.g. an empty
#'   table). Default `600L`.
#'
#' @return The widget with the render hook attached.
#'
#' @seealso [text_extra()]
#'
#' @examples
#' \dontrun{
#' output$tbl <- renderReactable({
#'   bindExtrasOnRender(reactable(df, columns = list(
#'     comment = colDef(html = TRUE, cell = text_extra("comment"))
#'   )))
#' })
#' }
#'
#' @importFrom htmlwidgets onRender
#' @export
bindExtrasOnRender <- function(widget, fallback_ms = 600L) {
  htmlwidgets::onRender(
    widget,
    sprintf(
      "function(el){ if(window.rpExtrasOnReady){ window.rpExtrasOnReady(el,%d); } }",
      as.integer(fallback_ms)
    )
  )
}
