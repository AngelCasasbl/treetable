sample_data <- function() {
  tibble::tibble(
    area = c("Sales", "Sales", "Costs"),
    category = c("Product A", "Product B", "Payroll"),
    id = c("v-a", "v-b", "c-payroll"),
    Q1_budget = c(1000, 500, 300), Q1_actual = c(950, 600, 280),
    Q2_budget = c(1100, 550, 300), Q2_actual = c(1200, 500, 310)
  )
}

test_that("tree_table() returns an htmlwidgets widget with the expected payload", {
  widget <- tree_table(
    sample_data(),
    levels = c("area", "category"),
    columns = c("Q1", "Q2"),
    suffix_a = "_budget", suffix_b = "_actual",
    label_a = "Budget", label_b = "Actual"
  )

  expect_s3_class(widget, "treetable")
  expect_s3_class(widget, "htmlwidget")
  expect_equal(widget$x$columns, list("Q1", "Q2"))
  expect_equal(widget$x$metrics, list("Budget", "Actual"))
  expect_length(widget$x$tree, 2) # Sales, Costs
  expect_false(widget$x$detail_enabled)
})

test_that("build_tree_payload() nests two levels with two metrics per column", {
  payload <- build_tree_payload(
    sample_data(),
    levels = c("area", "category"),
    columns = c("Q1", "Q2"),
    suffix_a = "_budget", suffix_b = "_actual",
    label_a = "Budget", label_b = "Actual"
  )

  expect_equal(payload$columns, list("Q1", "Q2"))
  expect_equal(payload$metrics, list("Budget", "Actual"))

  sales <- payload$tree[[which(vapply(payload$tree, `[[`, character(1), "label") == "Sales")]]
  expect_length(sales$children, 2)
  expect_null(sales$id)

  product_a <- sales$children[[1]]
  expect_equal(product_a$id, "v-a")
  expect_equal(product_a$series[[1]]$a, 1000)
  expect_equal(product_a$series[[1]]$b, 950)
})

test_that("build_tree_payload() supports a single metric (suffix_b = NULL)", {
  data <- sample_data()
  payload <- build_tree_payload(
    data,
    levels = c("area", "category"),
    columns = c("Q1", "Q2"),
    suffix_a = "_budget",
    label_a = "Budget"
  )

  expect_equal(payload$metrics, list("Budget"))
  sales <- payload$tree[[which(vapply(payload$tree, `[[`, character(1), "label") == "Sales")]]
  expect_null(sales$children[[1]]$series[[1]]$b)
})

test_that("build_tree_payload() respects skip_level", {
  data <- sample_data()
  payload <- build_tree_payload(
    data,
    levels = c("area", "category"),
    columns = "Q1",
    suffix_a = "_budget", suffix_b = "_actual",
    skip_level = function(level, sub_data) {
      level == "category" && identical(unique(sub_data$area), "Costs")
    }
  )

  costs <- payload$tree[[which(vapply(payload$tree, `[[`, character(1), "label") == "Costs")]]
  expect_null(costs$children)
  expect_false(is.null(costs$id))
})

test_that("tree_table() validates theme and format arguments", {
  data <- sample_data()

  expect_error(
    tree_table(data, levels = "area", columns = "Q1", suffix_a = "_budget", theme = list()),
    class = "rlang_error"
  )
  expect_error(
    tree_table(data, levels = "area", columns = "Q1", suffix_a = "_budget", format_a = list()),
    class = "rlang_error"
  )
})

test_that("tree_table() errors clearly when a level column is missing", {
  data <- sample_data()

  expect_error(
    tree_table(data, levels = "missing_level", columns = "Q1", suffix_a = "_budget"),
    class = "rlang_error"
  )
})

test_that("tree_table() carries theme and format specs into the widget payload", {
  data <- sample_data()
  widget <- tree_table(
    data,
    levels = "area",
    columns = "Q1",
    suffix_a = "_budget", suffix_b = "_actual",
    theme = treetable_theme(header_bg = "#123456"),
    format_a = column_format(type = "currency", decimals = 0),
    format_b = column_format(type = "currency", decimals = 0),
    detail = detail_table(data, id_col = "id")
  )

  expect_equal(widget$x$theme$header_bg, "#123456")
  expect_equal(widget$x$format_a$type, "currency")
  expect_equal(widget$x$format_b$type, "currency")
  expect_true(widget$x$detail_enabled)
})
