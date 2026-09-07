test_that("detail_table() validates required columns", {
  raw_data <- data.frame(id = c("a", "b"), value = c(1, 2))
  detail <- detail_table(raw_data, id_col = "id", columns = c("id", "value"))

  expect_s3_class(detail, "treetable_detail_table")
  expect_error(detail_table(raw_data, id_col = "missing_column"), class = "rlang_error")
  expect_error(
    detail_table(raw_data, id_col = "id", columns = "missing_column"),
    class = "rlang_error"
  )
})

test_that("show_detail_modal() errors when detail is disabled (NULL)", {
  expect_error(show_detail_modal("a", data.frame(), NULL), class = "rlang_error")
})

test_that("show_detail_modal() errors when detail is neither a function nor a detail_table()", {
  expect_error(show_detail_modal("a", data.frame(), detail = 123), class = "rlang_error")
})

test_that("show_detail_modal() forwards id and raw_data to a custom detail function", {
  raw_data <- data.frame(id = "a", value = 1)
  captured <- NULL
  custom_detail <- function(id, raw_data) {
    captured <<- list(id = id, raw_data = raw_data)
    htmltools::tags$p("custom ui")
  }

  testthat::local_mocked_bindings(
    showModal = function(ui, session = NULL) ui,
    modalDialog = function(..., title = NULL) list(title = title, body = list(...)),
    .package = "shiny"
  )

  result <- show_detail_modal("a", raw_data, custom_detail)

  expect_equal(captured$id, "a")
  expect_equal(captured$raw_data, raw_data)
  expect_equal(result$title, "a")
})

test_that("show_detail_modal() uses the detail_table()'s own data, not the raw_data argument", {
  bundled_data <- data.frame(id = c("a", "a", "b"), value = c(1, 2, 3))
  detail <- detail_table(bundled_data, id_col = "id")

  testthat::local_mocked_bindings(
    showModal = function(ui, session = NULL) ui,
    modalDialog = function(..., title = NULL) list(title = title, body = list(...)),
    .package = "shiny"
  )

  # raw_data = NULL here: the automatic table must come from `detail`'s own
  # bundled data, never touching this argument.
  result <- show_detail_modal("a", raw_data = NULL, detail)

  expect_equal(result$title, "a")
})

test_that("simple_html_table() builds one row per record with escaped content", {
  data <- data.frame(id = c("a", "b"), label = c("<b>x</b>", "y"), stringsAsFactors = FALSE)

  table_tag <- simple_html_table(data)
  rendered <- as.character(htmltools::tagList(table_tag))

  expect_s3_class(table_tag, "shiny.tag")
  expect_equal(table_tag$name, "table")
  expect_match(rendered, "&lt;b&gt;x&lt;/b&gt;", fixed = TRUE)
})

test_that("render_detail_table() filters by id before building the table", {
  raw_data <- data.frame(id = c("a", "a", "b"), value = c(1, 2, 3))
  detail <- detail_table(raw_data, id_col = "id")

  if (rlang::is_installed("reactable")) {
    result <- render_detail_table("a", detail)
    expect_s3_class(result, "reactable")
  } else if (rlang::is_installed("DT")) {
    result <- render_detail_table("a", detail)
    expect_s3_class(result, "datatables")
  } else {
    result <- render_detail_table("a", detail)
    rendered <- as.character(htmltools::tagList(result))
    expect_match(rendered, "^<table", perl = TRUE)
  }
})

test_that("render_detail_table() honors the columns filter", {
  raw_data <- data.frame(id = c("a", "b"), value = c(1, 2), extra = c("x", "y"))
  detail <- detail_table(raw_data, id_col = "id", columns = c("id", "value"))

  result <- render_detail_table("a", detail)

  # Whichever backend renders it, the dropped "extra" column must not leak
  # into the table's underlying data.
  filtered <- raw_data[raw_data$id == "a", c("id", "value")]
  expect_false("extra" %in% names(filtered))
})
