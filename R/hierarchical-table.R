#' Aggregate a flat data frame into a hierarchical table with subtotals
#'
#' Groups `data` by an arbitrary number of hierarchy columns (from most
#' general to most specific) and sums one or more numeric columns at every
#' level, producing subtotal rows for each intermediate level plus a leaf row
#' for the finest grouping.
#'
#' `data` must be a "flat" data frame (one row per record) with:
#' * One column per hierarchy level, from most general to most specific
#'   (e.g. `c("group", "subgroup", "category")`). Columns may be character or
#'   factor; `NA` is treated as its own category.
#' * One or more *numeric* columns to sum at each level (e.g. `"income"`,
#'   `"expense"`, `"net"`, or any metric you want to accumulate).
#' * (Optional) an ordering column to control the sequence in which
#'   groups/subgroups/categories appear (alphabetical by default).
#'
#' @param data A flat data frame/tibble.
#' @param levels Character vector of column names, from most general to most
#'   specific (e.g. `c("group", "subgroup", "category")`).
#' @param values Character vector of numeric column names to sum at every
#'   node of the hierarchy.
#' @param order_col Optional column name used to order nodes within each
#'   level (defaults to alphabetical order of the level's value).
#' @param level_names Optional labels for the `level` column at each depth
#'   (defaults to `"level_1"`, `"level_2"`, ...; the deepest level is always
#'   labeled `"leaf"`).
#' @return A long-format tibble, one row per node of the tree, with columns:
#'   * `level`: `"level_1"`, `"level_2"`, ... (or `"leaf"` for the deepest
#'     level).
#'   * `label`: the value of that category at that level.
#'   * `n`: number of original rows that make up that node.
#'   * `row_order`: final order to draw the table top to bottom.
#'   * one column per entry in `levels`, holding that level's value for the
#'     node (`NA` if the node sits at a shallower level), useful to
#'     filter/expand on screen.
#'   * one column per entry in `values`, already summed for that node.
#' @examples
#' data <- tibble::tibble(
#'   area     = c("Sales", "Sales", "Costs"),
#'   category = c("Product A", "Product B", "Payroll"),
#'   amount   = c(1000, 500, -300),
#'   units    = c(10, 5, NA)
#' )
#' hierarchical_table(data, levels = c("area", "category"), values = c("amount", "units"))
#' @export
hierarchical_table <- function(
  data,
  levels,
  values,
  order_col = NULL,
  level_names = NULL
) {
  stopifnot(length(levels) >= 1, length(values) >= 1)
  abort_missing_columns(data, c(levels, values, order_col))

  if (nrow(data) == 0) {
    return(empty_hierarchical_table(data, levels, values))
  }

  if (is.null(level_names)) {
    level_names <- paste0("level_", seq_along(levels))
  }
  level_names[length(level_names)] <- "leaf"

  data <- data |>
    dplyr::mutate(
      ..original_order = if (!is.null(order_col)) .data[[order_col]] else 0
    )

  aggregated <- data |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(values), \(x) sum(x, na.rm = TRUE)),
      n = dplyr::n(),
      ..original_order = min(.data$..original_order, na.rm = TRUE),
      .by = dplyr::all_of(levels)
    )

  table <- build_hierarchy_node(
    aggregated,
    levels,
    values,
    level_names,
    depth = 1L
  )

  table |> dplyr::mutate(row_order = dplyr::row_number())
}

# Internal: empty-input shortcut for hierarchical_table(), avoiding the
# `min()`-on-zero-rows warning that dplyr::summarise() would otherwise raise
# while still returning a tibble with the right columns and types.
empty_hierarchical_table <- function(data, levels, values) {
  empty <- tibble::tibble(level = character(0), label = character(0), n = integer(0))
  for (val in values) empty[[val]] <- data[[val]][0]
  for (lvl in levels) empty[[lvl]] <- data[[lvl]][0]
  empty$row_order <- integer(0)
  empty
}

# Internal recursive helper: groups `data` (already aggregated over the full
# combination of levels) by the current level, builds the subtotal row for
# each category, and recurses into the next level if any remain.
build_hierarchy_node <- function(
  data,
  levels,
  values,
  level_names,
  depth
) {
  current_level <- levels[depth]
  level_label <- level_names[depth]
  is_last <- depth == length(levels)

  category_order <- data |>
    dplyr::summarise(
      ..category_order = min(.data$..original_order, na.rm = TRUE),
      .by = dplyr::all_of(current_level)
    ) |>
    dplyr::arrange(.data$..category_order, .data[[current_level]])

  purrr::map_dfr(category_order[[current_level]], function(key) {
    sub <- data[
      data[[current_level]] %in%
        key |
        (is.na(data[[current_level]]) & is.na(key)),
    ]

    totals <- sub |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(values), sum),
        n = sum(.data$n)
      )

    current_row <- tibble::tibble(
      level = level_label,
      label = key,
      n = totals$n
    ) |>
      dplyr::bind_cols(totals[values])

    # Ancestor (more general) level columns are filled with the branch's
    # actual value -not NA- so rows can be filtered/expanded on them even
    # when the row is a subtotal (e.g. "group" on a subgroup row). Only
    # levels deeper than the current one stay NA.
    for (i in seq_len(depth)) {
      current_row[[levels[i]]] <- if (i == depth) key else sub[[levels[i]]][1]
    }

    if (is_last) {
      current_row
    } else {
      children <- build_hierarchy_node(
        sub,
        levels,
        values,
        level_names,
        depth + 1L
      )
      # The subtotal row has no value at deeper levels.
      for (col_name in levels[(depth + 1):length(levels)]) {
        current_row[[col_name]] <- NA
      }
      dplyr::bind_rows(current_row, children)
    }
  })
}
