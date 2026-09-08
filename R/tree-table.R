#' Render a hierarchy as an interactive tree table
#'
#' Builds an `htmlwidgets` widget that renders `data` as an expandable/
#' collapsible tree table, usable both inside and outside Shiny.
#'
#' `data` must be a flat data frame (one row per full combination of levels,
#' as for [hierarchical_table()]) with:
#' * One column per hierarchy level in `levels`, from most general to most
#'   specific (e.g. `c("group", "subgroup", "item")`).
#' * The source columns named by every [col_field()] across `columns`
#'   (`col_field()`'s `value`) — no shared naming pattern required between
#'   them.
#' * A column uniquely identifying every leaf row (`"id"` by default; change
#'   it with `id_col`), used for the per-row detail button.
#'
#' `columns` is a list of [col_spec()] groups, reactable-style: each group
#' has a header `name` and one or more [col_field()]s underneath it, every
#' one with its own source column, aggregation, display format, and
#' optionally a custom cell renderer (`col_field()`'s `cell`) for full
#' control over a cell's HTML — colors, badges, icons, anything
#' `htmltools` can build. There is no automatic comparison/percent-variance
#' column: with more than one field per group, every field just renders as
#' its own plain column; add one yourself (another [col_field()], computed
#' in `data`) if you want it, so you control what it means and how (or
#' whether) it's styled.
#'
#' `tree_table()` can be called directly in the console or an R
#' Markdown/Quarto chunk (auto-printed like any `htmlwidgets` widget), or
#' inside [renderTreetable()] in a Shiny server.
#'
#' @param data A flat data frame (see Details).
#' @param levels Character vector of hierarchy columns, from most general to
#'   most specific (e.g. `c("group", "subgroup", "item")`).
#' @param columns A list of [col_spec()] objects, from left to right.
#' @param id_col Column identifying every leaf row (detail button).
#' @param theme A [treetable_theme()] object controlling colors, fonts and
#'   column widths.
#' @param detail Controls the per-leaf detail button: `NULL` (default)
#'   disables it; otherwise pass a [detail_table()] object or a custom
#'   `function(id, raw_data)` (see [show_detail_modal()]). `tree_table()`
#'   only uses this to decide whether to show the detail button — the actual
#'   rendering happens later via [show_detail_modal()].
#' @param skip_level Optional `function(level, sub_data)` returning `TRUE`
#'   when that level should be skipped for that subset of data (e.g. when a
#'   subgroup has no real subdivision at that level). By default no level is
#'   skipped.
#' @param lang Language for the widget's own UI text (the expand/collapse
#'   toggle button, the detail-button tooltip, the "no data" message):
#'   `"en"`, `"es"`, or `NULL` (default) to auto-detect from the viewer's
#'   browser, falling back to English. This does not translate anything you
#'   provide yourself (column/field names, node labels).
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
#'   columns = list(
#'     col_spec(
#'       "Q1",
#'       col_field("Q1_budget", label = "Budget", format = column_format(type = "currency")),
#'       col_field("Q1_actual", label = "Actual", format = column_format(type = "currency"))
#'     ),
#'     col_spec(
#'       "Q2",
#'       col_field("Q2_budget", label = "Budget", format = column_format(type = "currency")),
#'       col_field("Q2_actual", label = "Actual", format = column_format(type = "currency"))
#'     )
#'   )
#' )
#'
#' # A parent node can show the average of its leaf rows instead of their
#' # sum, e.g. a student's grade as the mean of their subjects' grades - and
#' # different columns can pull from unrelated source columns/aggregations/
#' # formats, with no shared naming pattern required between them.
#' mixed <- tibble::tibble(
#'   student = c("Ana", "Ana", "Leo", "Leo"),
#'   subject = c("Math", "Art", "Math", "Art"),
#'   id = c("ana-math", "ana-art", "leo-math", "leo-art"),
#'   Grade_T1 = c(90, 100, 80, 95),
#'   attendance_days = c(18, 20, 15, 19)
#' )
#' tree_table(
#'   mixed,
#'   levels = c("student", "subject"),
#'   columns = list(
#'     col_spec("Grade", col_field("Grade_T1", aggregate = "mean")),
#'     col_spec(
#'       "Attendance",
#'       col_field("attendance_days", format = column_format(type = "number", decimals = 0))
#'     )
#'   )
#' )
#'
#' # Custom cell rendering, reactable-style: color a grade by how good it is.
#' tree_table(
#'   mixed,
#'   levels = c("student", "subject"),
#'   columns = list(
#'     col_spec(
#'       "Grade",
#'       col_field(
#'         "Grade_T1", aggregate = "mean",
#'         cell = function(value, row) {
#'           if (is.na(value)) return(NULL)
#'           color <- if (value >= 90) "#1a7f37" else if (value >= 70) "#9a6700" else "#cf222e"
#'           htmltools::tags$span(
#'             style = paste0("color:", color, ";font-weight:600"),
#'             sprintf("%.1f", value)
#'           )
#'         }
#'       )
#'     )
#'   )
#' )
#' @export
tree_table <- function(
  data,
  levels,
  columns,
  id_col = "id",
  theme = treetable_theme(),
  detail = NULL,
  skip_level = NULL,
  lang = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL
) {
  if (!inherits(theme, "treetable_theme")) {
    cli::cli_abort("{.arg theme} must be created with {.fn treetable_theme}.")
  }
  if (!is.null(lang) && !(rlang::is_scalar_character(lang) && lang %in% c("en", "es"))) {
    cli::cli_abort('{.arg lang} must be {.code NULL}, {.val en}, or {.val es}.')
  }

  payload <- build_tree_payload(
    data = data,
    levels = levels,
    columns = columns,
    id_col = id_col,
    skip_level = skip_level
  )

  x <- list(
    column_groups = payload$column_groups,
    subcolumns = payload$subcolumns,
    tree = payload$tree,
    theme = unclass(theme),
    detail_enabled = !is.null(detail),
    lang = lang
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

# Internal: builds the nested-tree payload (`list(column_groups, subcolumns,
# tree)`) consumed by the JS renderer, from a flat data frame and a list of
# col_spec() column groups.
build_tree_payload <- function(
  data,
  levels,
  columns,
  id_col = "id",
  skip_level = NULL
) {
  abort_missing_columns(data, c(levels, id_col))

  if (!is_col_spec_list(columns)) {
    cli::cli_abort("{.arg columns} must be a list of {.fn col_spec} objects.")
  }

  # Flattened across every group's fields; unname() guards against a JSON
  # array silently turning into a JSON object if a caller ever names an
  # argument in col_spec()'s `...` (e.g. `col_spec("Q1", a = col_field(...))`).
  fields <- unname(unlist(lapply(columns, function(cs) cs$fields), recursive = FALSE))
  abort_missing_columns(data, vapply(fields, `[[`, character(1), "value"))

  column_groups <- unname(lapply(columns, function(cs) {
    list(name = cs$name, span = length(cs$fields))
  }))
  subcolumns <- unname(lapply(fields, function(f) {
    list(label = f$label %||% "", format = unclass(f$format))
  }))

  build_values <- function(sub) {
    unname(lapply(fields, function(f) aggregate_fun(f$aggregate)(sub[[f$value]])))
  }

  build_renders <- function(values, label, is_leaf) {
    row <- list(label = label, is_leaf = is_leaf)
    renders <- unname(Map(function(f, val) render_cell(f$cell, val, row), fields, values))
    if (all(vapply(renders, is.null, logical(1)))) NULL else renders
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
      values <- build_values(sub)
      children <- if (length(rest) > 0) build_level(sub, rest) else list()
      is_leaf <- length(children) == 0
      node <- list(label = as.character(value), series = values)
      renders <- build_renders(values, node$label, is_leaf)
      if (!is.null(renders)) node$renders <- renders
      if (length(children) > 0) {
        node$children <- children
      } else {
        node$id <- as.character(sub[[id_col]][1])
      }
      node
    })
  }

  list(
    column_groups = column_groups,
    subcolumns = subcolumns,
    tree = build_level(data, levels)
  )
}
