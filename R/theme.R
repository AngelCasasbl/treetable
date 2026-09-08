#' Build a theme for [tree_table()]
#'
#' Constructs a validated theme object controlling the colors, fonts and
#' column widths of a [tree_table()] widget, so callers can restyle the
#' table entirely from R without touching CSS or JavaScript.
#'
#' @param header_bg,header_text Background/text color of the sticky column
#'   header.
#' @param subtotal_bg,subtotal_text Background/text color of subtotal rows
#'   (any row with children, other than the top level).
#' @param leaf_hover_bg Background color of a leaf row on mouse hover.
#' @param font_family,font_size CSS font family and base font size for the
#'   whole table.
#' @param label_column_width,value_column_width CSS width of the sticky label
#'   column and of each numeric value column.
#' @return An object of class `treetable_theme`.
#' @examples
#' theme <- treetable_theme(header_bg = "#102a43", subtotal_bg = "#e3f2fd")
#' @export
treetable_theme <- function(
  header_bg = "#1d2d3d",
  header_text = "#ffffff",
  subtotal_bg = "#eef6ff",
  subtotal_text = "#1d1f20",
  leaf_hover_bg = "#f7fafc",
  font_family = "Inter, 'Segoe UI', sans-serif",
  font_size = "13px",
  label_column_width = "280px",
  value_column_width = "88px"
) {
  theme <- list(
    header_bg = header_bg,
    header_text = header_text,
    subtotal_bg = subtotal_bg,
    subtotal_text = subtotal_text,
    leaf_hover_bg = leaf_hover_bg,
    font_family = font_family,
    font_size = font_size,
    label_column_width = label_column_width,
    value_column_width = value_column_width
  )

  is_scalar_string <- vapply(
    theme,
    function(x) is.character(x) && length(x) == 1 && !is.na(x),
    logical(1)
  )
  if (!all(is_scalar_string)) {
    cli::cli_abort(
      "All {.fn treetable_theme} arguments must be a single string, but
      {.field {names(theme)[!is_scalar_string]}} {?is/are} not."
    )
  }

  structure(theme, class = "treetable_theme")
}

#' @export
print.treetable_theme <- function(x, ...) {
  cli::cli_h3("<treetable_theme>")
  for (name in names(x)) {
    cli::cli_li("{.field {name}}: {.val {x[[name]]}}")
  }
  invisible(x)
}

#' Describe how a numeric column should be formatted in [tree_table()]
#'
#' @param type One of `"number"`, `"currency"`, `"percentage"` or `"text"`.
#' @param decimals Number of decimal places to display.
#' @param symbol Symbol shown next to the value (e.g. `"$"` for currency).
#'   Defaults to `"$"` when `type = "currency"` and to `NULL` otherwise.
#' @param thousands_sep Whether to group digits with a thousands separator.
#' @return An object of class `treetable_format`.
#' @examples
#' column_format(type = "currency", decimals = 0)
#' column_format(type = "percentage", decimals = 1)
#' @export
column_format <- function(
  type = c("number", "currency", "percentage", "text"),
  decimals = 0,
  symbol = NULL,
  thousands_sep = TRUE
) {
  type <- rlang::arg_match(type)

  if (!rlang::is_scalar_integerish(decimals) || decimals < 0) {
    cli::cli_abort("{.arg decimals} must be a single non-negative integer.")
  }
  if (!is.null(symbol) && !rlang::is_scalar_character(symbol)) {
    cli::cli_abort("{.arg symbol} must be a single string or {.code NULL}.")
  }
  if (!rlang::is_scalar_logical(thousands_sep)) {
    cli::cli_abort("{.arg thousands_sep} must be a single {.code TRUE}/{.code FALSE}.")
  }

  if (is.null(symbol) && type == "currency") {
    symbol <- "$"
  }

  structure(
    list(
      type = type,
      decimals = as.integer(decimals),
      symbol = symbol,
      thousands_sep = thousands_sep
    ),
    class = "treetable_format"
  )
}

#' @export
print.treetable_format <- function(x, ...) {
  cli::cli_text(
    "<treetable_format> {.field type}: {.val {x$type}}, {.field decimals}: {.val {x$decimals}}"
  )
  invisible(x)
}

default_format <- function() column_format("number")
