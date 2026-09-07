# treetable 0.1.0

* Initial release.
* `hierarchical_table()`: aggregate a flat data frame into an arbitrary-depth
  hierarchy with subtotals at every level.
* `tree_table()`: interactive, expandable/collapsible tree table
  `htmlwidgets` widget, with one or two metrics per column and automatic
  percentage variance.
* `treetable_theme()` and `column_format()`: configure row colors, fonts,
  column widths and number formats without touching CSS/JS.
* `detail_table()` and `show_detail_modal()`: optional per-leaf detail modal,
  either an automatic table view or a fully custom UI.
* `treetableOutput()` / `renderTreetable()`: Shiny bindings built on top of
  `htmlwidgets::shinyWidgetOutput()` / `htmlwidgets::shinyRenderWidget()`.
