`%||%` <- function(x, y) if (is.null(x)) y else x

# Internal: returns a function(x) that aggregates a numeric vector using
# `method` ("sum" or "mean"), ignoring NA. An empty/all-NA `x` yields
# `NA_real_` for "mean" (instead of the `NaN` that `mean(..., na.rm = TRUE)`
# would give), so it round-trips through JSON as a display blank rather than
# a non-finite value. "sum" keeps base R's own convention (0 for an
# empty/all-NA input).
aggregate_fun <- function(method) {
  if (method == "mean") {
    function(x) {
      value <- mean(x, na.rm = TRUE)
      if (is.nan(value)) NA_real_ else value
    }
  } else {
    function(x) sum(x, na.rm = TRUE)
  }
}

abort_missing_columns <- function(data, columns, call = rlang::caller_env()) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg data} is missing column{?s} {.field {missing}}.",
      call = call
    )
  }
  invisible(NULL)
}
