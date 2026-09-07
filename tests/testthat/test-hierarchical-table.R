test_that("aggregates a single level", {
  data <- tibble::tibble(
    area = c("Sales", "Sales", "Costs"),
    amount = c(1000, 500, -300)
  )

  result <- hierarchical_table(data, levels = "area", values = "amount")

  expect_equal(nrow(result), 2)
  expect_equal(result$level, c("leaf", "leaf"))
  expect_equal(sort(result$label), c("Costs", "Sales"))
  expect_equal(result$amount[result$label == "Sales"], 1500)
  expect_equal(result$amount[result$label == "Costs"], -300)
  expect_equal(result$n[result$label == "Sales"], 2)
})

test_that("aggregates two levels with subtotal rows", {
  data <- tibble::tibble(
    area = c("Sales", "Sales", "Costs"),
    category = c("Product A", "Product B", "Payroll"),
    amount = c(1000, 500, -300)
  )

  result <- hierarchical_table(data, levels = c("area", "category"), values = "amount")

  expect_equal(nrow(result), 5) # 2 level_1 + 3 leaf
  sales_subtotal <- result[result$level == "level_1" & result$label == "Sales", ]
  expect_equal(sales_subtotal$amount, 1500)
  expect_equal(sales_subtotal$n, 2)
  expect_true(is.na(sales_subtotal$category))

  leaf_a <- result[result$level == "leaf" & result$label == "Product A", ]
  expect_equal(leaf_a$amount, 1000)
  expect_equal(leaf_a$area, "Sales")
})

test_that("reproduces group/subgroup/item totals for a cash-flow-like hierarchy", {
  # Regression-style check for the acceptance criterion in section 8 of the
  # design prompt: hierarchical_table() must reproduce, for a
  # group/subgroup/item dataset shaped like a cash-flow report, the same
  # subtotals a hand-rolled aggregation would produce.
  movements <- tibble::tibble(
    group = c("Income", "Income", "Income", "Expenses", "Expenses"),
    subgroup = c("Sales", "Sales", "Other income", "Payroll", "Rent"),
    item = c("Product A", "Product B", "Interest", "Salaries", "Office"),
    net = c(1000, 500, 50, -700, -200)
  )

  result <- hierarchical_table(
    movements,
    levels = c("group", "subgroup", "item"),
    values = "net"
  )

  expected_group_totals <- movements |>
    dplyr::summarise(net = sum(net), .by = group)
  expected_subgroup_totals <- movements |>
    dplyr::summarise(net = sum(net), .by = c(group, subgroup))

  group_rows <- result[result$level == "level_1", ]
  for (i in seq_len(nrow(expected_group_totals))) {
    row <- group_rows[group_rows$label == expected_group_totals$group[i], ]
    expect_equal(row$net, expected_group_totals$net[i])
  }

  subgroup_rows <- result[result$level == "level_2", ]
  for (i in seq_len(nrow(expected_subgroup_totals))) {
    row <- subgroup_rows[subgroup_rows$label == expected_subgroup_totals$subgroup[i], ]
    expect_equal(row$net, expected_subgroup_totals$net[i])
  }

  expect_equal(sum(result[result$level == "leaf", ]$net), sum(movements$net))
})

test_that("treats NA as its own category", {
  data <- tibble::tibble(
    area = c("Sales", NA, "Sales"),
    amount = c(100, 50, 25)
  )

  result <- hierarchical_table(data, levels = "area", values = "amount")

  expect_equal(nrow(result), 2)
  expect_equal(result$amount[is.na(result$label)], 50)
  expect_equal(result$amount[result$label == "Sales" & !is.na(result$label)], 125)
})

test_that("orders nodes using order_col", {
  data <- tibble::tibble(
    area = c("Z-area", "A-area"),
    priority = c(1, 2),
    amount = c(10, 20)
  )

  result <- hierarchical_table(data, levels = "area", values = "amount", order_col = "priority")

  expect_equal(result$label, c("Z-area", "A-area"))
})

test_that("supports custom level_names", {
  data <- tibble::tibble(area = "Sales", category = "Product A", amount = 10)

  result <- hierarchical_table(
    data,
    levels = c("area", "category"),
    values = "amount",
    level_names = c("group", "concept")
  )

  expect_equal(unique(result$level), c("group", "leaf"))
})

test_that("returns an empty tibble for an empty data frame", {
  data <- tibble::tibble(area = character(0), amount = numeric(0))

  expect_no_warning(
    result <- hierarchical_table(data, levels = "area", values = "amount")
  )

  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("level", "label", "n", "amount", "area", "row_order"))
  expect_type(result$n, "integer")
  expect_type(result$amount, "double")
})

test_that("errors clearly when a value column is missing", {
  data <- tibble::tibble(area = "Sales", amount = 10)

  expect_error(
    hierarchical_table(data, levels = "area", values = "missing_column"),
    class = "rlang_error"
  )
})

test_that("errors clearly when a level column is missing", {
  data <- tibble::tibble(area = "Sales", amount = 10)

  expect_error(
    hierarchical_table(data, levels = "missing_level", values = "amount"),
    class = "rlang_error"
  )
})
