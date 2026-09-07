`%||%` <- function(x, y) if (is.null(x)) y else x

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
