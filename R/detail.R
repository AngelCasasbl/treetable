#' Automatic per-leaf detail table
#'
#' Builds a `detail` spec for [tree_table()]/[show_detail_modal()] that shows
#' a table of the raw rows behind a leaf, filtered by `id_col`. Uses
#' `reactable` when available, falling back to `DT`, and finally to a plain
#' HTML table if neither is installed.
#'
#' @param raw_data Data frame with the raw (pre-aggregation) rows, including
#'   an `id_col` column matching the `id_col` used to build the
#'   [tree_table()] hierarchy.
#' @param id_col Column in `raw_data` identifying the rows belonging to each
#'   leaf.
#' @param columns Optional character vector of columns to show (defaults to
#'   all columns of `raw_data`).
#' @return An object of class `treetable_detail_table`, to pass as the
#'   `detail` argument of [tree_table()] and [show_detail_modal()].
#' @examples
#' raw_data <- data.frame(id = c("a", "a", "b"), amount = c(10, 20, 5))
#' detail_table(raw_data, id_col = "id", columns = c("id", "amount"))
#' @export
detail_table <- function(raw_data, id_col = "id", columns = NULL) {
  abort_missing_columns(raw_data, c(id_col, columns %||% character()))
  structure(
    list(raw_data = raw_data, id_col = id_col, columns = columns),
    class = "treetable_detail_table"
  )
}

#' Show the per-leaf detail modal for a [tree_table()] widget
#'
#' Wraps the `showModal()` + `modalDialog()` boilerplate needed to react to a
#' `tree_table()` detail-button click, so a Shiny app doesn't have to rewrite
#' it for every table. Wire it up with:
#'
#' ```r
#' observeEvent(input$my_table_detail, {
#'   show_detail_modal(input$my_table_detail$id, raw_data, detail)
#' })
#' ```
#'
#' @param id Leaf id, as sent by the widget's `<output_id>_detail` Shiny
#'   input (i.e. `input$my_table_detail$id`).
#' @param raw_data Raw data forwarded to a custom `detail` function; ignored
#'   when `detail` is a [detail_table()] object, which already carries its
#'   own data.
#' @param detail Either a [detail_table()] object (automatic table, filtered
#'   by `id`) or a `function(id, raw_data)` returning an
#'   `htmltools::tagList()`/Shiny UI to show in the modal.
#' @param title Modal title (defaults to `id`).
#' @param ... Additional arguments passed to `shiny::modalDialog()`.
#' @return The value of `shiny::showModal()`, invisibly.
#' @export
show_detail_modal <- function(id, raw_data, detail, title = NULL, ...) {
  rlang::check_installed("shiny", reason = "to show a treetable detail modal.")

  if (is.null(detail)) {
    cli::cli_abort(
      "{.arg detail} is {.code NULL}: the detail modal is disabled for this table."
    )
  }

  body <- if (is.function(detail)) {
    detail(id, raw_data)
  } else if (inherits(detail, "treetable_detail_table")) {
    render_detail_table(id, detail)
  } else {
    cli::cli_abort("{.arg detail} must be a function or a {.fn detail_table} object.")
  }

  shiny::showModal(shiny::modalDialog(
    title = title %||% id,
    body,
    easyClose = TRUE,
    size = "l",
    ...
  ))
}

render_detail_table <- function(id, detail) {
  filtered <- detail$raw_data[detail$raw_data[[detail$id_col]] == id, , drop = FALSE]
  if (!is.null(detail$columns)) {
    filtered <- filtered[, intersect(detail$columns, names(filtered)), drop = FALSE]
  }

  if (rlang::is_installed("reactable")) {
    reactable::reactable(filtered, defaultPageSize = 10, striped = TRUE, highlight = TRUE)
  } else if (rlang::is_installed("DT")) {
    DT::datatable(filtered, options = list(pageLength = 10))
  } else {
    simple_html_table(filtered)
  }
}

simple_html_table <- function(data) {
  header <- htmltools::tags$tr(lapply(names(data), htmltools::tags$th))
  rows <- lapply(seq_len(nrow(data)), function(i) {
    cells <- lapply(data[i, , drop = FALSE], function(cell) htmltools::tags$td(format(cell)))
    htmltools::tags$tr(cells)
  })
  htmltools::tags$table(
    class = "table table-sm treetable-detail-fallback",
    htmltools::tags$thead(header),
    htmltools::tags$tbody(rows)
  )
}
