test_that("treetable_theme() returns a validated theme object with defaults", {
  theme <- treetable_theme()

  expect_s3_class(theme, "treetable_theme")
  expect_equal(theme$header_bg, "#1d2d3d")
  expect_equal(theme$value_column_width, "88px")
})

test_that("treetable_theme() accepts overrides", {
  theme <- treetable_theme(header_bg = "#000000", font_size = "16px")

  expect_equal(theme$header_bg, "#000000")
  expect_equal(theme$font_size, "16px")
})

test_that("treetable_theme() rejects non-string arguments", {
  expect_error(treetable_theme(header_bg = 123), class = "rlang_error")
  expect_error(treetable_theme(header_bg = c("#000", "#fff")), class = "rlang_error")
  expect_error(treetable_theme(header_bg = NA_character_), class = "rlang_error")
})

test_that("column_format() validates type", {
  fmt <- column_format(type = "currency", decimals = 2)

  expect_s3_class(fmt, "treetable_format")
  expect_equal(fmt$type, "currency")
  expect_equal(fmt$decimals, 2L)
  expect_equal(fmt$symbol, "$") # default currency symbol

  expect_error(column_format(type = "not-a-type"), class = "rlang_error")
})

test_that("column_format() validates decimals and thousands_sep", {
  expect_error(column_format(decimals = -1), class = "rlang_error")
  expect_error(column_format(decimals = "0"), class = "rlang_error")
  expect_error(column_format(thousands_sep = "yes"), class = "rlang_error")
})

test_that("column_format() leaves symbol NULL for non-currency types by default", {
  fmt <- column_format(type = "number")
  expect_null(fmt$symbol)

  fmt_pct <- column_format(type = "percentage", decimals = 1)
  expect_null(fmt_pct$symbol)
})
