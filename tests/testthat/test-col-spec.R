test_that("col_field() builds a treetable_col_field with sensible defaults", {
  field <- col_field("enero_nota")

  expect_s3_class(field, "treetable_col_field")
  expect_equal(field$value, "enero_nota")
  expect_equal(field$aggregate, "sum")
  expect_s3_class(field$format, "treetable_format")
  expect_equal(field$format$type, "number")
  expect_null(field$label)
  expect_null(field$cell)
})

test_that("col_field() carries aggregate, format, label and cell overrides", {
  cell_fn <- function(value, row) NULL
  field <- col_field(
    "q1_actual",
    aggregate = "mean",
    format = column_format(type = "currency", decimals = 0),
    label = "Actual",
    cell = cell_fn
  )

  expect_equal(field$aggregate, "mean")
  expect_equal(field$format$type, "currency")
  expect_equal(field$label, "Actual")
  expect_identical(field$cell, cell_fn)
})

test_that("col_field() validates its arguments", {
  expect_error(col_field(1), class = "rlang_error")
  expect_error(col_field("x", aggregate = "median"), class = "rlang_error")
  expect_error(col_field("x", format = list()), class = "rlang_error")
  expect_error(col_field("x", label = 1), class = "rlang_error")
  expect_error(col_field("x", cell = "not a function"), class = "rlang_error")
})

test_that("col_spec() bundles one or more col_field()s under a group name", {
  spec <- col_spec("Enero", col_field("enero_nota", aggregate = "mean"))

  expect_s3_class(spec, "treetable_col_spec")
  expect_equal(spec$name, "Enero")
  expect_length(spec$fields, 1)
  expect_equal(spec$fields[[1]]$value, "enero_nota")

  spec2 <- col_spec(
    "Q1",
    col_field("q1_budget", label = "Budget"),
    col_field("q1_actual", label = "Actual")
  )
  expect_length(spec2$fields, 2)
  expect_equal(spec2$fields[[2]]$label, "Actual")
})

test_that("col_spec() validates its arguments", {
  expect_error(col_spec(1, col_field("x")), class = "rlang_error")
  expect_error(col_spec("A"), class = "rlang_error") # needs at least one field
  expect_error(col_spec("A", "not a col_field"), class = "rlang_error")
  expect_error(col_spec("A", col_field("x"), "not a col_field"), class = "rlang_error")
})

test_that("col_spec() strips argument names from ... so fields stay a positional list", {
  # A named argument in `...` must not turn `fields` into a named list -
  # build_tree_payload() relies on it staying purely positional so the
  # flattened series/renders arrays serialize as JSON arrays, not objects.
  spec <- col_spec("Q1", first = col_field("q1_budget"), second = col_field("q1_actual"))

  expect_null(names(spec$fields))
  expect_length(spec$fields, 2)
})
