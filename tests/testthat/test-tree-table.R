sample_data <- function() {
  tibble::tibble(
    area = c("Sales", "Sales", "Costs"),
    category = c("Product A", "Product B", "Payroll"),
    id = c("v-a", "v-b", "c-payroll"),
    Q1_budget = c(1000, 500, 300), Q1_actual = c(950, 600, 280),
    Q2_budget = c(1100, 550, 300), Q2_actual = c(1200, 500, 310)
  )
}

sample_columns <- function() {
  list(
    col_spec("Q1", col_field("Q1_budget", label = "Budget"), col_field("Q1_actual", label = "Actual")),
    col_spec("Q2", col_field("Q2_budget", label = "Budget"), col_field("Q2_actual", label = "Actual"))
  )
}

test_that("tree_table() returns an htmlwidgets widget with the expected payload", {
  widget <- tree_table(sample_data(), levels = c("area", "category"), columns = sample_columns())

  expect_s3_class(widget, "treetable")
  expect_s3_class(widget, "htmlwidget")
  expect_equal(vapply(widget$x$column_groups, `[[`, character(1), "name"), c("Q1", "Q2"))
  expect_equal(vapply(widget$x$column_groups, `[[`, numeric(1), "span"), c(2, 2))
  expect_length(widget$x$subcolumns, 4) # Q1 budget/actual, Q2 budget/actual
  expect_length(widget$x$tree, 2) # Sales, Costs
  expect_false(widget$x$detail_enabled)
})

test_that("build_tree_payload() nests two levels with a two-field column group", {
  payload <- build_tree_payload(sample_data(), levels = c("area", "category"), columns = sample_columns())

  expect_equal(vapply(payload$column_groups, `[[`, character(1), "name"), c("Q1", "Q2"))
  expect_equal(vapply(payload$subcolumns, `[[`, character(1), "label"), c("Budget", "Actual", "Budget", "Actual"))

  sales <- payload$tree[[which(vapply(payload$tree, `[[`, character(1), "label") == "Sales")]]
  expect_length(sales$children, 2)
  expect_null(sales$id)

  product_a <- sales$children[[1]]
  expect_equal(product_a$id, "v-a")
  expect_equal(product_a$series[[1]], 1000) # Q1 budget
  expect_equal(product_a$series[[2]], 950) # Q1 actual
})

test_that("build_tree_payload() supports a single field per group (no sub-header needed)", {
  data <- sample_data()
  payload <- build_tree_payload(
    data,
    levels = c("area", "category"),
    columns = list(col_spec("Q1", col_field("Q1_budget")), col_spec("Q2", col_field("Q2_budget")))
  )

  expect_equal(vapply(payload$column_groups, `[[`, numeric(1), "span"), c(1, 1))
  sales <- payload$tree[[which(vapply(payload$tree, `[[`, character(1), "label") == "Sales")]]
  expect_length(sales$children[[1]]$series, 2) # one value per group, still flat
})

test_that("build_tree_payload() respects skip_level", {
  data <- sample_data()
  payload <- build_tree_payload(
    data,
    levels = c("area", "category"),
    columns = list(col_spec("Q1", col_field("Q1_budget"), col_field("Q1_actual"))),
    skip_level = function(level, sub_data) {
      level == "category" && identical(unique(sub_data$area), "Costs")
    }
  )

  costs <- payload$tree[[which(vapply(payload$tree, `[[`, character(1), "label") == "Costs")]]
  expect_null(costs$children)
  expect_false(is.null(costs$id))
})

test_that("tree_table() validates theme and columns arguments", {
  data <- sample_data()

  expect_error(
    tree_table(data, levels = "area", columns = sample_columns(), theme = list()),
    class = "rlang_error"
  )
  expect_error(
    tree_table(data, levels = "area", columns = "Q1"), # no longer a valid columns form
    class = "rlang_error"
  )
  expect_error(
    tree_table(data, levels = "area", columns = list()),
    class = "rlang_error"
  )
})

test_that("tree_table() errors clearly when a level column is missing", {
  data <- sample_data()

  expect_error(
    tree_table(data, levels = "missing_level", columns = sample_columns()),
    class = "rlang_error"
  )
})

test_that("build_tree_payload() supports aggregate = 'mean' for a parent node", {
  grades <- tibble::tibble(
    student = c("Ana", "Ana", "Leo", "Leo"),
    subject = c("Math", "Art", "Math", "Art"),
    id = c("ana-math", "ana-art", "leo-math", "leo-art"),
    T1_grade = c(90, 100, 80, NA)
  )

  payload <- build_tree_payload(
    grades,
    levels = c("student", "subject"),
    columns = list(col_spec("Grade", col_field("T1_grade", aggregate = "mean")))
  )

  ana <- payload$tree[[which(vapply(payload$tree, `[[`, character(1), "label") == "Ana")]]
  expect_equal(ana$series[[1]], 95) # mean(90, 100), not sum (190)

  # Leo's Art grade is NA: the parent mean must ignore it (denominator 1, not
  # 2), matching mean(x, na.rm = TRUE) rather than an external sum / n hack.
  leo <- payload$tree[[which(vapply(payload$tree, `[[`, character(1), "label") == "Leo")]]
  expect_equal(leo$series[[1]], 80)

  math_a <- ana$children[[which(vapply(ana$children, `[[`, character(1), "label") == "Math")]]
  expect_equal(math_a$series[[1]], 90) # a single leaf: mean == its own value
})

test_that("build_tree_payload() gives NA (not NaN) for an aggregate = 'mean' node with no non-NA leaves", {
  data <- tibble::tibble(
    student = c("Ana", "Ana"),
    subject = c("Math", "Art"),
    id = c("ana-math", "ana-art"),
    T1_grade = c(NA_real_, NA_real_)
  )

  payload <- build_tree_payload(
    data,
    levels = c("student", "subject"),
    columns = list(col_spec("Grade", col_field("T1_grade", aggregate = "mean")))
  )

  ana <- payload$tree[[1]]
  expect_true(is.na(ana$series[[1]]))
  expect_false(is.nan(ana$series[[1]]))
})

test_that("tree_table() carries theme into the widget payload", {
  data <- sample_data()
  widget <- tree_table(
    data,
    levels = "area",
    columns = list(
      col_spec(
        "Q1",
        col_field("Q1_budget", format = column_format(type = "currency", decimals = 0)),
        col_field("Q1_actual", format = column_format(type = "currency", decimals = 0))
      )
    ),
    theme = treetable_theme(header_bg = "#123456"),
    detail = detail_table(data, id_col = "id")
  )

  expect_equal(widget$x$theme$header_bg, "#123456")
  expect_equal(widget$x$subcolumns[[1]]$format$type, "currency")
  expect_equal(widget$x$subcolumns[[2]]$format$type, "currency")
  expect_true(widget$x$detail_enabled)
})

test_that("tree_table() defaults lang to NULL (browser auto-detect) and forwards an explicit choice", {
  data <- sample_data()
  columns <- list(col_spec("Q1", col_field("Q1_budget")))

  default_widget <- tree_table(data, levels = "area", columns = columns)
  expect_null(default_widget$x$lang)

  es_widget <- tree_table(data, levels = "area", columns = columns, lang = "es")
  expect_equal(es_widget$x$lang, "es")
})

test_that("tree_table() validates lang", {
  data <- sample_data()

  expect_error(
    tree_table(data, levels = "area", columns = list(col_spec("Q1", col_field("Q1_budget"))), lang = "fr"),
    class = "rlang_error"
  )
})

# reactable-style columns: each col_spec() group can pick its own name,
# fields (source column, aggregation, format, custom cell render),
# independently of the others - no shared naming pattern required.
mixed_data <- function() {
  tibble::tibble(
    student = c("Ana", "Ana", "Leo", "Leo"),
    subject = c("Math", "Art", "Math", "Art"),
    id = c("ana-math", "ana-art", "leo-math", "leo-art"),
    Grade_T1 = c(90, 100, 80, 95),
    attendance_days = c(18, 20, 15, 19)
  )
}

test_that("build_tree_payload() supports independent single-field groups with different names/sources/aggregates", {
  payload <- build_tree_payload(
    mixed_data(),
    levels = c("student", "subject"),
    columns = list(
      col_spec("Grade", col_field("Grade_T1", aggregate = "mean")),
      col_spec("Attendance", col_field("attendance_days", aggregate = "sum"))
    )
  )

  expect_equal(vapply(payload$column_groups, `[[`, character(1), "name"), c("Grade", "Attendance"))

  ana <- payload$tree[[which(vapply(payload$tree, `[[`, character(1), "label") == "Ana")]]
  expect_equal(ana$series[[1]], 95) # mean(90, 100)
  expect_equal(ana$series[[2]], 38) # sum(18, 20)
})

test_that("build_tree_payload() does not require a shared naming pattern between column display name and source column", {
  # The exact friction that motivated col_spec()/col_field(): the group's
  # display name ("Calificacion") need not match the source data column
  # ("Grade_T1") in any way, and different groups can pull from source
  # columns with completely unrelated names.
  payload <- build_tree_payload(
    mixed_data(),
    levels = c("student", "subject"),
    columns = list(
      col_spec("Calificacion", col_field("Grade_T1")),
      col_spec("Dias de asistencia", col_field("attendance_days"))
    )
  )

  expect_equal(
    vapply(payload$column_groups, `[[`, character(1), "name"),
    c("Calificacion", "Dias de asistencia")
  )
})

test_that("build_tree_payload() supports more than two fields per group, each independently formatted", {
  data <- mixed_data()
  data$extra_credit <- c(5, 0, 2, 3)

  payload <- build_tree_payload(
    data,
    levels = "student",
    columns = list(
      col_spec(
        "Grade",
        col_field("Grade_T1", label = "T1", aggregate = "mean"),
        col_field("attendance_days", label = "Days", format = column_format(type = "number", decimals = 0)),
        col_field("extra_credit", label = "Extra", aggregate = "sum")
      )
    )
  )

  expect_equal(payload$column_groups[[1]]$span, 3)
  expect_length(payload$subcolumns, 3)
  expect_equal(vapply(payload$subcolumns, `[[`, character(1), "label"), c("T1", "Days", "Extra"))

  ana <- payload$tree[[which(vapply(payload$tree, `[[`, character(1), "label") == "Ana")]]
  expect_length(ana$series, 3)
  expect_equal(ana$series[[1]], 95) # mean grade
  expect_equal(ana$series[[3]], 5) # sum extra credit
})

test_that("build_tree_payload() errors clearly when a col_field() source column is missing", {
  expect_error(
    build_tree_payload(
      mixed_data(),
      levels = "student",
      columns = list(col_spec("Grade", col_field("missing_col")))
    ),
    class = "rlang_error"
  )
})

test_that("build_tree_payload() errors clearly when columns is not a list of col_spec()", {
  expect_error(
    build_tree_payload(mixed_data(), levels = "student", columns = "Grade_T1"),
    class = "rlang_error"
  )
})

test_that("tree_table() applies a col_field()'s custom cell render, falling back to format otherwise", {
  data <- mixed_data()
  widget <- tree_table(
    data,
    levels = c("student", "subject"),
    id_col = "id",
    columns = list(
      col_spec(
        "Grade",
        col_field(
          "Grade_T1",
          aggregate = "mean",
          cell = function(value, row) {
            if (is.na(value) || value < 95) return(NULL) # only override the top grade
            htmltools::tags$span(class = "top-grade", sprintf("%.0f!", value))
          }
        )
      )
    )
  )

  ana <- widget$x$tree[[which(vapply(widget$x$tree, `[[`, character(1), "label") == "Ana")]]
  leo <- widget$x$tree[[which(vapply(widget$x$tree, `[[`, character(1), "label") == "Leo")]]

  # Ana's mean grade is 95: the cell renderer kicks in with custom HTML.
  expect_false(is.null(ana$renders))
  expect_match(ana$renders[[1]]$html, "top-grade")
  expect_match(ana$renders[[1]]$html, "95!")

  # Leo's mean grade is 87.5 (< 95): the renderer returns NULL, so there is
  # no override for Leo's node at all (falls back to normal formatting).
  expect_null(leo$renders)
})

test_that("a col_field()'s cell render can override just the text without any HTML", {
  data <- mixed_data()
  widget <- tree_table(
    data,
    levels = "student",
    columns = list(
      col_spec("Grade", col_field("Grade_T1", aggregate = "mean", cell = function(value, row) "N/A"))
    )
  )

  ana <- widget$x$tree[[1]]
  expect_equal(ana$renders[[1]]$text, "N/A")
  expect_null(ana$renders[[1]]$html)
})

test_that("col_spec()'s field order stays positional even with named ... arguments (no JSON array/object bug)", {
  data <- mixed_data()
  widget <- tree_table(
    data,
    levels = "student",
    columns = list(
      col_spec("Q1", a = col_field("Grade_T1"), b = col_field("attendance_days"))
    )
  )

  expect_null(names(widget$x$tree[[1]]$series))
  expect_length(widget$x$tree[[1]]$series, 2)
})
