#' Shiny bindings for [tree_table()]
#'
#' `treetableOutput()`/`renderTreetable()` are the standard `htmlwidgets`
#' Shiny bindings for [tree_table()], generated via
#' `htmlwidgets::shinyWidgetOutput()`/`htmlwidgets::shinyRenderWidget()`, so
#' the widget gets sizing, re-rendering and error handling for free like any
#' other `htmlwidgets`-based output.
#'
#' A leaf's detail button (see the `detail` argument of [tree_table()]) sets
#' the Shiny input `input[[paste0(outputId, "_detail")]]` to
#' `list(id = <leaf id>, nonce = <random number>)`; pair it with
#' [show_detail_modal()] in an `observeEvent()`.
#'
#' @param output_id Output variable to read the widget from.
#' @param width,height Widget width/height, as a CSS unit string (e.g.
#'   `"100%"`) or `"auto"` (default) to size the table to its content with no
#'   internal scroll. Passing a fixed height like `"620px"` limits the table
#'   to that height with an internal vertical scrollbar.
#' @return `treetableOutput()` returns an HTML tag object; `renderTreetable()`
#'   returns a `shiny.render.function`.
#' @examples
#' if (interactive() && rlang::is_installed("shiny")) {
#'   library(shiny)
#'
#'   data <- tibble::tibble(
#'     area = c("Sales", "Sales", "Costs"),
#'     category = c("Product A", "Product B", "Payroll"),
#'     id = c("v-a", "v-b", "c-payroll"),
#'     Q1_budget = c(1000, 500, 300), Q1_actual = c(950, 600, 280)
#'   )
#'
#'   ui <- fluidPage(treetableOutput("tbl"))
#'   server <- function(input, output) {
#'     output$tbl <- renderTreetable(
#'       tree_table(
#'         data,
#'         levels = c("area", "category"),
#'         columns = list(
#'           col_spec(
#'             "Q1",
#'             col_field("Q1_budget", label = "Budget"),
#'             col_field("Q1_actual", label = "Actual")
#'           )
#'         )
#'       )
#'     )
#'   }
#'   shinyApp(ui, server)
#' }
#' @name treetable-shiny
NULL

#' @rdname treetable-shiny
#' @export
treetableOutput <- function(output_id, width = "100%", height = "auto") {
  htmlwidgets::shinyWidgetOutput(output_id, "treetable", width, height, package = "treetable")
}

#' @rdname treetable-shiny
#' @param expr An expression that returns a [tree_table()] widget.
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Is `expr` a quoted expression (with `quote()`)? This is
#'   useful if you want to save an expression in a variable.
#' @export
renderTreetable <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, treetableOutput, env, quoted = TRUE)
}
