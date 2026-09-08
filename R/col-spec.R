#' Define one field (leaf column) inside a col_spec() group
#'
#' Building block for [col_spec()]: one actual value column shown under a
#' column group's header, with its own source column in `data`, aggregation,
#' display format, and (optionally) a custom cell renderer.
#'
#' `format`'s `type` covers the common display kinds: `"number"` (plain
#' value; also what you want for "decimal" — just set `decimals`),
#' `"currency"`, `"percentage"`, or `"text"`. See [column_format()].
#'
#' @param value Name of the `data` column holding this field's value.
#' @param aggregate How this field rolls up from leaf rows to every ancestor
#'   node: `"sum"` (default) or `"mean"`. Every node, at every depth,
#'   aggregates directly over the leaf rows underneath it (not over its
#'   immediate children's already-aggregated values). `"mean"` ignores `NA`
#'   leaf values (as `sum` already does via `na.rm = TRUE`); a node with no
#'   non-`NA` leaves shows as blank rather than `NaN`.
#' @param format A [column_format()] object controlling how this field is
#'   displayed (currency, number/decimal, percentage, or text) when `cell`
#'   is `NULL` or returns `NULL` for a given row.
#' @param label Sub-header text for this field, shown when its [col_spec()]
#'   group has more than one field (so they can be told apart under the
#'   shared group header, e.g. `"Budget"`/`"Actual"`). Ignored when the
#'   group has only this one field — the group's own `name` already labels
#'   it. `NULL` (default) leaves the sub-header blank.
#' @param cell Optional `function(value, row)` to fully customize how this
#'   field renders for a given node, reactable-style: `value` is the
#'   already-aggregated number for that node, and `row` is
#'   `list(label, is_leaf)` for that node. Return:
#'   * `NULL` to fall back to the default, `format`-based rendering (the
#'     common case: only override specific rows/values).
#'   * An `htmltools` tag/tag list (e.g. `htmltools::tags$span(...)`, or
#'     `htmltools::HTML(...)` for a raw HTML string) for custom colors,
#'     shapes, badges, icons, etc.
#'   * Anything else (a plain string or number) to override just the
#'     displayed text, still safely escaped, bypassing `format`.
#' @return An object of class `treetable_col_field`, to pass to [col_spec()].
#' @examples
#' col_field("enero_nota", aggregate = "mean", format = column_format(decimals = 1))
#'
#' col_field(
#'   "grade", label = "Grade",
#'   cell = function(value, row) {
#'     if (is.na(value)) return(NULL)
#'     color <- if (value >= 90) "#1a7f37" else if (value >= 70) "#9a6700" else "#cf222e"
#'     htmltools::tags$span(
#'       style = paste0("color:", color, ";font-weight:600"),
#'       sprintf("%.1f", value)
#'     )
#'   }
#' )
#' @export
col_field <- function(
  value,
  aggregate = c("sum", "mean"),
  format = column_format(),
  label = NULL,
  cell = NULL
) {
  if (!rlang::is_scalar_character(value)) {
    cli::cli_abort("{.arg value} must be a single string (a column name in {.arg data}).")
  }
  aggregate <- rlang::arg_match(aggregate)
  if (!inherits(format, "treetable_format")) {
    cli::cli_abort("{.arg format} must be created with {.fn column_format}.")
  }
  if (!is.null(label) && !rlang::is_scalar_character(label)) {
    cli::cli_abort("{.arg label} must be a single string or {.code NULL}.")
  }
  if (!is.null(cell) && !is.function(cell)) {
    cli::cli_abort("{.arg cell} must be a function, or {.code NULL}.")
  }

  structure(
    list(value = value, aggregate = aggregate, format = format, label = label, cell = cell),
    class = "treetable_col_field"
  )
}

#' @export
print.treetable_col_field <- function(x, ...) {
  cli::cli_text(
    "<col_field> {.field value}={.val {x$value}} ({.val {x$aggregate}}, {.val {x$format$type}})"
  )
  invisible(x)
}

#' Define one column group of a tree_table(), reactable-style
#'
#' `tree_table()`'s `columns` is a list of `col_spec()` groups; each group
#' has a header `name` and one or more [col_field()]s underneath it, every
#' one with its own source column in `data` (no shared naming pattern
#' required across groups or fields), its own aggregation, its own display
#' format, and optionally its own custom cell renderer.
#'
#' A group with a single field renders as one plain column under `name`. A
#' group with several fields renders `name` spanning a sub-header row with
#' one cell per field (its `label`), e.g. a "Q1" group with `"Budget"`/
#' `"Actual"` fields.
#'
#' @param name Header text shown for this column group.
#' @param ... One or more [col_field()] objects.
#' @return An object of class `treetable_col_spec`, to pass inside a list as
#'   [tree_table()]'s `columns` argument.
#' @examples
#' col_spec("Enero", col_field("enero_nota", aggregate = "mean"))
#'
#' col_spec(
#'   "Q1",
#'   col_field("q1_budget", label = "Budget", format = column_format(type = "currency")),
#'   col_field("q1_actual", label = "Actual", format = column_format(type = "currency"))
#' )
#' @export
col_spec <- function(name, ...) {
  fields <- unname(list(...))

  if (!rlang::is_scalar_character(name) || is.na(name)) {
    cli::cli_abort("{.arg name} must be a single string.")
  }
  if (length(fields) == 0) {
    cli::cli_abort("{.fn col_spec} needs at least one {.fn col_field} in {.arg ...}.")
  }
  is_field <- vapply(fields, inherits, logical(1), "treetable_col_field")
  if (!all(is_field)) {
    cli::cli_abort("Every argument in {.arg ...} must be created with {.fn col_field}.")
  }

  structure(list(name = name, fields = fields), class = "treetable_col_spec")
}

#' @export
print.treetable_col_spec <- function(x, ...) {
  cli::cli_text("<col_spec> {.field {x$name}}: {length(x$fields)} field(s)")
  for (field in x$fields) print(field)
  invisible(x)
}

# Internal: TRUE when `columns` is a list of col_spec() objects, the only
# form tree_table() accepts for its `columns` argument.
is_col_spec_list <- function(columns) {
  is.list(columns) &&
    length(columns) > 0 &&
    all(vapply(columns, inherits, logical(1), "treetable_col_spec"))
}

# Internal: evaluates a col_field()'s `cell` function (if set) for one node,
# returning NULL (fall back to the field's own `format`), or a `{html}`/
# `{text}` override for the widget to render instead of the formatted
# value - `html` for an htmltools tag/tag list/HTML() string (rendered
# as-is), `text` for anything else (rendered as plain, escaped text,
# bypassing `format` entirely).
render_cell <- function(cell_fn, value, row) {
  if (is.null(cell_fn)) return(NULL)
  result <- cell_fn(value, row)
  if (is.null(result)) return(NULL)
  if (inherits(result, c("shiny.tag", "shiny.tag.list", "html"))) {
    list(html = as.character(htmltools::tagList(result)))
  } else {
    list(text = as.character(result))
  }
}
