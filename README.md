
# treetable

<!-- badges: start -->

[![R-CMD-check](https://github.com/AngelCasasbl/treeTable/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AngelCasasbl/treeTable/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

treetable aggregates any flat data frame into an arbitrary-depth
hierarchy with subtotals at every level, and renders it as an
interactive, expandable/collapsible tree table built on `htmlwidgets` —
usable the same way in the console, R Markdown/Quarto, and Shiny apps.
Row colors, fonts, column widths, number formats, and the per-leaf
detail modal are all configured from R, with no CSS or JavaScript to
touch.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("AngelCasasbl/treeTable")
```

## Example

Aggregate a flat data frame into a hierarchy with subtotals:

``` r
library(treetable)

sales <- tibble::tibble(
  area     = c("Sales", "Sales", "Sales", "Costs", "Costs"),
  category = c("Product A", "Product A", "Product B", "Payroll", "Rent"),
  amount   = c(1000, 750, 500, -1200, -300)
)

hierarchical_table(sales, levels = c("area", "category"), values = "amount")
#> # A tibble: 6 × 7
#>   level   label         n amount area  category  row_order
#>   <chr>   <chr>     <int>  <dbl> <chr> <chr>         <int>
#> 1 level_1 Costs         2  -1500 Costs <NA>              1
#> 2 leaf    Payroll       1  -1200 Costs Payroll           2
#> 3 leaf    Rent          1   -300 Costs Rent              3
#> 4 level_1 Sales         3   2250 Sales <NA>              4
#> 5 leaf    Product A     2   1750 Sales Product A         5
#> 6 leaf    Product B     1    500 Sales Product B         6
```

Render an interactive tree table comparing two metrics per column, with
a custom theme and currency formatting:

``` r
budget_vs_actual <- tibble::tibble(
  area = c("Sales", "Sales", "Costs"),
  category = c("Product A", "Product B", "Payroll"),
  id = c("v-a", "v-b", "c-payroll"),
  Q1_budget = c(1000, 500, 300), Q1_actual = c(950, 600, 280),
  Q2_budget = c(1100, 550, 300), Q2_actual = c(1200, 500, 310)
)

tree_table(
  budget_vs_actual,
  levels = c("area", "category"),
  columns = c("Q1", "Q2"),
  suffix_a = "_budget", suffix_b = "_actual",
  label_a = "Budget", label_b = "Actual",
  theme = treetable_theme(header_bg = "#102a43"),
  format_a = column_format(type = "currency", decimals = 0),
  format_b = column_format(type = "currency", decimals = 0)
)
```

In Shiny, swap `tree_table()` for
`renderTreetable()`/`treetableOutput()`; see
`vignette("introduction", package = "treetable")` for the full
walkthrough, including the per-leaf detail modal.
