`%||%` <- function(x, y) if (is.null(x)) y else x

# Short label for an argument, used as the check target in reports.
label_of <- function(expr) {
  lab <- paste(deparse(expr, width.cutoff = 60L), collapse = " ")
  if (nchar(lab) > 40) lab <- paste0(substr(lab, 1, 37), "...")
  lab
}

# Integer group index for the combinations of `by` in `df`, plus one row per
# group with the group labels. NA is its own group.
group_index <- function(df, by) {
  if (!length(by)) {
    return(list(index = rep(1L, nrow(df)), groups = data.frame(.all = "all")[, 0, drop = FALSE], n = 1L))
  }
  parts <- lapply(df[by], function(v) {
    v <- as.character(v)
    v[is.na(v)] <- "<NA>"
    v
  })
  lab <- do.call(paste, c(unname(parts), sep = "\u001f"))
  u <- unique(lab)
  groups <- df[!duplicated(lab), by, drop = FALSE]
  rownames(groups) <- NULL
  list(index = match(lab, u), groups = groups, n = length(u))
}

# The year ("2015") or month ("2015-03") of dates, as format() writes them.
# Worked out for the distinct dates only, since registers repeat dates, and
# written from the year and month: format() goes through strftime() for
# every date, which is slow on the birth dates of a population. Years
# outside 1000 to 9999 are left to format(), which pads them differently
# between platforms.
date_period <- function(d, unit) {
  u <- unique(d)
  lt <- as.POSIXlt(u)
  y <- lt$year + 1900L
  ok <- !is.na(y)
  if (any(y[ok] < 1000 | y[ok] > 9999)) {
    lab <- format(u, if (unit == "year") "%Y" else "%Y-%m")
  } else {
    lab <- rep(NA_character_, length(u))
    lab[ok] <- if (unit == "year") sprintf("%04d", y[ok]) else sprintf("%04d-%02d", y[ok], lt$mon[ok] + 1L)
  }
  lab[match(d, u)]
}

# Number of distinct persons per group. Without ids every row is a person.
persons_per <- function(id, index, n_groups) {
  if (is.null(id)) return(tabulate(index, n_groups))
  # one row per pair of group and person, on integer codes
  pair <- index + (match(id, unique(id)) - 1) * as.numeric(n_groups)
  first <- !is.na(pair) & !duplicated(pair)
  as.numeric(tabulate(index[first], n_groups))
}

# Normalise one data frame or a named list of data frames.
as_table_list <- function(tables, label = "data") {
  if (is.data.frame(tables)) {
    nm <- if (identical(make.names(label), label)) label else "data"
    out <- list(as.data.frame(tables, stringsAsFactors = FALSE))
    names(out) <- nm
    return(out)
  }
  if (!is.list(tables) || !length(tables)) {
    cli::cli_abort("{.arg tables} must be a data frame or a named list of data frames.")
  }
  nms <- names(tables)
  if (is.null(nms) || any(!nzchar(nms)) || anyDuplicated(nms)) {
    cli::cli_abort("{.arg tables} must be a list with unique, non-empty names.")
  }
  not_df <- nms[!vapply(tables, is.data.frame, logical(1))]
  if (length(not_df)) {
    cli::cli_abort("{.arg tables} element{?s} {.val {not_df}} {?is/are} not a data frame.")
  }
  lapply(tables, as.data.frame, stringsAsFactors = FALSE)
}

# Print a data frame through cli so it stays in order with other messages.
cli_table <- function(d, max_rows = 10) {
  cli::cli_verbatim(utils::capture.output(print(utils::head(d, max_rows), row.names = FALSE)))
  if (nrow(d) > max_rows) cli::cli_text("... and {nrow(d) - max_rows} more")
}

# Names from the user's data must never be read as cli markup.
cli_escape <- function(x) gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)

check_columns <- function(df, cols, arg) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    cli::cli_abort("{.arg {arg}} column{?s} {.field {missing}} not found in the data.")
  }
  invisible(cols)
}
