#' Render a hierarchy as an interactive tree table
#'
#' Builds an `htmlwidgets` widget that renders `data` as an expandable/
#' collapsible tree table (grouped columns with one or two metrics, automatic
#' percentage variance when there are two), usable both inside and outside
#' Shiny.
#'
#' `data` must be a flat data frame (one row per full combination of levels,
#' as for [hierarchical_table()]) with:
#' * One column per hierarchy level in `levels`, from most general to most
#'   specific (e.g. `c("group", "subgroup", "item")`).
#' * For every entry in `columns` (the "periods" compared side by side:
#'   months, quarters, branches, years, ...), one or two numeric columns:
#'   `<value><suffix_a>` and, to compare two metrics (with automatic percent
#'   variance), also `<value><suffix_b>`. E.g. with
#'   `columns = c("2025-01", "2025-02")`, `suffix_a = "_actual"`,
#'   `suffix_b = "_budget"`, the widget looks for `"2025-01_actual"`,
#'   `"2025-01_budget"`, `"2025-02_actual"`, `"2025-02_budget"`.
#' * A column uniquely identifying every leaf row (`"id"` by default; change
#'   it with `id_col`), used for the per-row detail button.
#'
#' If you only have a single metric per column (no comparison), leave
#' `suffix_b = NULL`: each column then shows one number, with no percent
#' variance.
#'
#' `tree_table()` can be called directly in the console or an R
#' Markdown/Quarto chunk (auto-printed like any `htmlwidgets` widget), or
#' inside [renderTreetable()] in a Shiny server.
#'
#' @param data A flat data frame (see Details).
#' @param levels Character vector of hierarchy columns, from most general to
#'   most specific (e.g. `c("group", "subgroup", "item")`).
#' @param columns Character vector of values to compare side by side (months,
#'   branches, years, ...). Each expects `<value><suffix_a>` (and
#'   `<value><suffix_b>` if applicable) columns in `data`.
#' @param suffix_a Suffix of the first metric (e.g. `"_actual"`).
#' @param suffix_b Suffix of the second metric, optional. If `NULL`, each
#'   column shows only `label_a`, with no comparison or percent variance.
#' @param label_a,label_b Column-header titles for each metric.
#' @param column_labels Labels to display per column (defaults to `columns`
#'   itself).
#' @param id_col Column identifying every leaf row (detail button).
#' @param theme A [treetable_theme()] object controlling colors, fonts and
#'   column widths.
#' @param format_a,format_b [column_format()] objects controlling how the
#'   first/second metric is displayed.
#' @param detail Controls the per-leaf detail button: `NULL` (default)
#'   disables it; otherwise pass a [detail_table()] object or a custom
#'   `function(id, raw_data)` (see [show_detail_modal()]). `tree_table()`
#'   only uses this to decide whether to show the detail button — the actual
#'   rendering happens later via [show_detail_modal()].
#' @param skip_level Optional `function(level, sub_data)` returning `TRUE`
#'   when that level should be skipped for that subset of data (e.g. when a
#'   subgroup has no real subdivision at that level). By default no level is
#'   skipped.
#' @param width,height Width/height forwarded to `htmlwidgets::createWidget()`
#'   (overall widget sizing; use `theme` for column widths).
#' @param element_id Optional widget DOM id, forwarded to
#'   `htmlwidgets::createWidget()`.
#' @return An `htmlwidgets` widget of class `treetable`.
#' @examples
#' data <- tibble::tibble(
#'   area = c("Sales", "Sales", "Costs"),
#'   category = c("Product A", "Product B", "Payroll"),
#'   id = c("v-a", "v-b", "c-payroll"),
#'   Q1_budget = c(1000, 500, 300), Q1_actual = c(950, 600, 280),
#'   Q2_budget = c(1100, 550, 300), Q2_actual = c(1200, 500, 310)
#' )
#' tree_table(
#'   data,
#'   levels = c("area", "category"),
#'   columns = c("Q1", "Q2"),
#'   suffix_a = "_budget", suffix_b = "_actual",
#'   label_a = "Budget", label_b = "Actual"
#' )
#' @export
tree_table <- function(
  data,
  levels,
  columns,
  suffix_a,
  suffix_b = NULL,
  label_a = "Value A",
  label_b = "Value B",
  column_labels = NULL,
  id_col = "id",
  theme = treetable_theme(),
  format_a = column_format(),
  format_b = column_format(),
  detail = NULL,
  skip_level = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL
) {
  if (!inherits(theme, "treetable_theme")) {
    cli::cli_abort("{.arg theme} must be created with {.fn treetable_theme}.")
  }
  if (!inherits(format_a, "treetable_format")) {
    cli::cli_abort("{.arg format_a} must be created with {.fn column_format}.")
  }
  if (!is.null(format_b) && !inherits(format_b, "treetable_format")) {
    cli::cli_abort("{.arg format_b} must be created with {.fn column_format}.")
  }

  payload <- build_tree_payload(
    data = data,
    levels = levels,
    columns = columns,
    suffix_a = suffix_a,
    suffix_b = suffix_b,
    label_a = label_a,
    label_b = label_b,
    column_labels = column_labels,
    id_col = id_col,
    skip_level = skip_level
  )

  x <- list(
    columns = payload$columns,
    metrics = payload$metrics,
    tree = payload$tree,
    theme = unclass(theme),
    format_a = unclass(format_a),
    format_b = if (!is.null(suffix_b)) unclass(format_b) else NULL,
    detail_enabled = !is.null(detail)
  )

  htmlwidgets::createWidget(
    name = "treetable",
    x = x,
    width = width,
    height = height,
    package = "treetable",
    elementId = element_id,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      defaultHeight = "auto",
      viewer.defaultHeight = "100%",
      viewer.fill = TRUE,
      browser.fill = TRUE,
      knitr.figure = FALSE
    )
  )
}

# Internal: builds the nested-tree payload (`list(columns, metrics, tree)`)
# consumed by the JS renderer, from a flat data frame and hierarchy spec.
build_tree_payload <- function(
  data,
  levels,
  columns,
  suffix_a,
  suffix_b = NULL,
  label_a = "Value A",
  label_b = "Value B",
  column_labels = NULL,
  id_col = "id",
  skip_level = NULL
) {
  abort_missing_columns(data, c(levels, id_col))
  if (is.null(column_labels)) column_labels <- columns
  has_b <- !is.null(suffix_b)

  build_series <- function(sub) {
    lapply(columns, function(col) {
      value <- list(a = sum(sub[[paste0(col, suffix_a)]], na.rm = TRUE))
      if (has_b) value$b <- sum(sub[[paste0(col, suffix_b)]], na.rm = TRUE)
      value
    })
  }

  build_level <- function(sub_data, remaining_levels) {
    if (nrow(sub_data) == 0 || length(remaining_levels) == 0) {
      return(list())
    }
    if (!is.null(skip_level) && isTRUE(skip_level(remaining_levels[1], sub_data))) {
      return(build_level(sub_data, remaining_levels[-1]))
    }
    level <- remaining_levels[1]
    rest <- remaining_levels[-1]
    order <- unique(sub_data[[level]])
    # An empty/NA value means this row has no real deeper level: it is
    # dropped instead of creating an unlabeled child node (the parent level
    # is then left without `children` and the widget treats it as a leaf).
    order <- order[!is.na(order) & nzchar(order)]

    lapply(order, function(value) {
      sub <- sub_data[sub_data[[level]] == value, , drop = FALSE]
      node <- list(label = as.character(value), series = build_series(sub))
      children <- if (length(rest) > 0) build_level(sub, rest) else list()
      if (length(children) > 0) {
        node$children <- children
      } else {
        node$id <- as.character(sub[[id_col]][1])
      }
      node
    })
  }

  list(
    columns = as.list(as.character(column_labels)),
    metrics = if (has_b) list(label_a, label_b) else list(label_a),
    tree = build_level(data, levels)
  )
}
