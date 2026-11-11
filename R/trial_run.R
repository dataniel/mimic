#' Try a script on the synthetic copy
#'
#' Runs the researcher's script on the copy from [mimic()] in a fresh, isolated R session and reports whether it runs
#' through, which packages it uses, which files it writes, and whether it
#' tries to save data at row level. Run it before sending a script for
#' [real_run()]: a script that fails here will fail on the real data too, and
#' a script that saves rows will be stopped at output checking.
#'
#' @section How the script sees the data:
#' Each table is available as an object named after it (`persons`,
#' `contacts`, ...); a single data frame is called `data`. The working
#' directory is the output directory, also available as `output_dir`. Write
#' all output there with relative paths, exactly as on the real run.
#'
#' @section How row-level output is detected:
#' * a write call (`write.csv()`, `saveRDS()`, `fwrite()`, ...) receives a
#'   table with many rows: at least 100 and at least half the rows of the
#'   smallest input table,
#' * a written file contains identifier values from the copy. Text files,
#'   `.rds` and `.RData` files are searched; other formats are listed as not
#'   scanned.
#'
#' @section The output check in advance:
#' With `check_output = TRUE` the files the script writes are also run
#' through [inspect()], as staff will do with the output from the real run.
#' The researcher sees from outside that a table shows a minimum, keeps a
#' cell that is not suppressed or saves a model with everyone's residuals,
#' and can fix the script before a run on the real data is spent on it.
#'
#' The check reads only the output, not the copy: the rules that compare
#' with the microdata (dominance, values held by few persons) need the real
#' data and are left for [inspect()] inside. The counts come from the copy,
#' so a cell can be small here and large in the real data, or the other way
#' round. The mess [mimic()] adds (odd codes such as `"?"` and `"999"`,
#' empty strings) gives small cells in a table that tabulates the raw
#' variable; that is a hint to clean the codes in the script, and a copy
#' made with `mess = FALSE` shows whether anything else is wrong. A file
#' that needs changes because of how it is built (a minimum
#' column, a model object, a suppressed cell the totals give back) will
#' need changes inside too; a file approved here is not yet approved.
#'
#' @param script Path to an R script, or R code as text.
#' @param fake_data The synthetic copy: a data frame or a named list of data
#'   frames, normally from [mimic()].
#' @param output_dir Directory the script writes to. By default a new
#'   temporary directory, returned in the result so the output can be
#'   looked at.
#' @param timeout Maximum run time in seconds.
#' @param config A [mimic_config()]; `NULL` uses the defaults. The
#'   output check uses the same rules, so give the configuration staff use.
#'   The script's [safe_count()], [safe_summary()] and [safe_model()]
#'   follow it too.
#' @param check_output Run the written files through [inspect()]; see the
#'   section above.
#' @return An object of class `mimic_trial` with `status` (`"ok"`,
#'   `"warn"` or `"error"`), `error` (message and script line), `packages`,
#'   `files`, `writes`, `row_level` (findings), `inspection` (the
#'   [inspect()] result for the written files, or `NULL`), `warnings`, `log`
#'   (the console output) and `output_dir`. A file that needs changes at the
#'   output check makes the status `"warn"`.
#' @export
#' @examples
#' \donttest{
#' m <- profile_data(list(persons = fake_persons, contacts = fake_contacts),
#'                   keys = "person_id",
#'                   config = mimic_config(geo_vars = "settlement", text_vars = "note"))
#' copy <- mimic(m, seed = 1)
#' fit <- trial_run(c(
#'   "tab <- table(persons$sex)",
#'   "write.csv(as.data.frame(tab), 'sex.csv')"
#' ), copy)
#' fit
#' }
trial_run <- function(script, fake_data, output_dir = NULL, timeout = 600, config = NULL,
                      check_output = TRUE) {
  if (!is.logical(check_output) || length(check_output) != 1 || is.na(check_output)) {
    cli::cli_abort("{.arg check_output} must be {.code TRUE} or {.code FALSE}.")
  }
  cfg <- as_config(config)
  sc <- as_script(script)
  if (sc$temp) on.exit(unlink(sc$path), add = TRUE)
  data <- as_script_data(fake_data, "fake_data")
  if (is.null(output_dir)) output_dir <- tempfile("trial_run_")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/")

  log_path <- tempfile("fit_log_", fileext = ".txt")
  on.exit(unlink(log_path), add = TRUE)
  res <- run_script(sc$path, data, output_dir, guard = FALSE, timeout = timeout, log_path = log_path,
                    config = cfg)
  log <- if (file.exists(log_path)) readLines(log_path, warn = FALSE) else character()

  pk <- script_packages(sc$path)
  packages <- data.frame(package = pk,
                         installed = vapply(pk, function(p) nzchar(system.file(package = p)), logical(1)),
                         stringsAsFactors = FALSE, row.names = NULL)

  writes <- writes_df(res$writes)
  ids <- id_values(fake_data, data, cfg)
  min_rows <- min(vapply(data, nrow, integer(1)))
  threshold <- max(100, 0.5 * min_rows)
  row_level <- list()
  flag <- function(what, reason) {
    row_level[[length(row_level) + 1L]] <<- data.frame(what = what, reason = reason,
                                                       stringsAsFactors = FALSE)
  }
  for (i in seq_len(nrow(writes))) {
    w <- writes[i, ]
    if (!is.na(w$rows) && w$rows >= threshold) {
      flag(paste0(w$fun, "(): ", w$path), sprintf("writes a table with %d rows", w$rows))
    }
    if (!w$inside) flag(paste0(w$fun, "(): ", w$path), "writes outside the output directory")
  }
  files <- res$files
  files$scanned <- logical(nrow(files))
  for (i in seq_len(nrow(files))) {
    sc_res <- scan_file(file.path(output_dir, files$file[i]), ids)
    files$scanned[i] <- sc_res$scanned
    if (sc_res$hits > 0) {
      flag(files$file[i], sprintf("contains %d identifier value(s) from the data", sc_res$hits))
    }
    if (!is.na(sc_res$rows) && sc_res$rows >= threshold) {
      flag(files$file[i], sprintf("holds a table with %d rows", sc_res$rows))
    }
  }
  row_level <- if (length(row_level)) unique(do.call(rbind, row_level)) else
    data.frame(what = character(), reason = character())

  # the same check staff run inside, on output from the copy; without the
  # copy as real_data, since its values say nothing about the real ones
  inspection <- if (check_output && nrow(files)) inspect(output_dir, config = cfg) else NULL

  error <- res$error
  status <- "ok"
  if (isTRUE(res$timeout)) {
    error <- list(message = sprintf("The script did not finish within %d seconds.", timeout),
                  class = "timeout", fun = NA_character_, line = NA_integer_)
  } else if (isTRUE(res$crash)) {
    error <- list(message = "The R session running the script crashed.",
                  class = "crash", fun = NA_character_, line = NA_integer_)
  }
  if (!is.null(error)) {
    status <- "error"
  } else if (nrow(row_level) || any(!packages$installed) ||
             (!is.null(inspection) && any(inspection$files$verdict == "NEEDS CHANGES"))) {
    status <- "warn"
  }

  structure(list(
    status = status,
    script = if (sc$temp) "(code)" else sc$path,
    error = error,
    packages = packages,
    files = files[, c("file", "size", "scanned")],
    writes = writes,
    row_level = row_level,
    inspection = inspection,
    copy_mess = any(attr(fake_data, "mimic")$mess$type %in% c("invalid codes", "empty strings")),
    warnings = res$warnings %||% character(),
    seconds = round(res$seconds %||% NA_real_, 1),
    log = log,
    output_dir = output_dir
  ), class = "mimic_trial")
}

#' @export
print.mimic_trial <- function(x, ...) {
  cli::cli_text("{.strong <mimic_trial>} {x$script}")
  if (x$status == "error") {
    where <- if (!is.na(x$error$line)) paste0(" (line ", x$error$line, ")") else ""
    cli::cli_alert_danger("The script failed{where}: {x$error$message}")
  } else {
    cli::cli_alert_success("The script ran through in {x$seconds} seconds.")
  }
  if (nrow(x$packages)) {
    missing <- x$packages$package[!x$packages$installed]
    cli::cli_alert_info("Packages: {x$packages$package}")
    if (length(missing)) cli::cli_alert_warning("Not installed here: {.pkg {missing}}")
  }
  if (nrow(x$files)) {
    cli::cli_alert_info("Files written to {.path {x$output_dir}}: {x$files$file}")
  } else {
    cli::cli_alert_info("No files written.")
  }
  if (nrow(x$row_level)) {
    cli::cli_alert_danger("Row-level data or writes outside the output directory:")
    cli_table(x$row_level)
  }
  if (!is.null(x$inspection)) print_output_check(x$inspection, isTRUE(x$copy_mess))
  if (length(x$warnings)) {
    cli::cli_alert_warning("{length(x$warnings)} warning{?s}, see {.code $warnings}.")
  }
  invisible(x)
}

# The output check in short: one line when nothing needs changes, the
# reasons for the files that do. Files for review by hand are only counted;
# images and spreadsheets always end there.
print_output_check <- function(ins, copy_mess = FALSE) {
  f <- ins$files
  bad <- f[f$verdict == "NEEDS CHANGES", , drop = FALSE]
  n_hand <- sum(f$verdict == "REVIEW BY HAND")
  hand <- if (n_hand) sprintf(", %d to review by hand", n_hand) else ""
  if (!nrow(bad)) {
    cli::cli_alert_success("Output check on the copy: nothing needs changes{hand}.")
    return(invisible(ins))
  }
  cli::cli_alert_danger(paste0("Output check on the copy: ", nrow(bad),
                               " file", if (nrow(bad) > 1) "s" else "",
                               " would be sent back", hand, ":"))
  for (i in seq_len(nrow(bad))) {
    cli::cli_text(cli_escape(bad$file[i]))
    cli::cli_ul(cli_escape(strsplit(bad$reasons[i], " | ", fixed = TRUE)[[1]]))
  }
  if (copy_mess) {
    cli::cli_alert_info(paste("The copy has made-up mess (odd codes, empty strings): small cells may",
                              "come from it. Clean the codes in the script, or try a copy made with",
                              "{.code mimic(mess = FALSE)}."))
  }
  cli::cli_text("See {.code $inspection} for every check.")
  invisible(ins)
}

# Identifier values in the fake data: columns mimic() created as ids, or,
# without that record, columns that look like ids.
id_values <- function(original, data, cfg) {
  cols <- attr(original, "mimic")$id_columns
  out <- character()
  for (tn in names(data)) {
    df <- data[[tn]]
    use <- if (!is.null(cols)) {
      cols[[if (is.data.frame(original)) 1 else tn]] %||% character()
    } else {
      names(df)[vapply(names(df), function(v) {
        variable_role(v, df[[v]], NA_character_, cfg)$role == "id"
      }, logical(1))]
    }
    for (v in intersect(use, names(df))) {
      x <- df[[v]]
      if (is.character(x) || is.factor(x)) out <- c(out, as.character(x[!is.na(x)]))
    }
  }
  unique(out[nchar(out) >= 4])
}

text_extensions <- c("csv", "tsv", "txt", "log", "md", "html", "htm", "json", "tex", "xml", "rmd", "qmd", "dcf")

# Look into a written file for identifier values and row counts.
scan_file <- function(path, ids) {
  ext <- tolower(tools::file_ext(path))
  res <- list(scanned = FALSE, hits = 0L, rows = NA_integer_)
  if (ext %in% text_extensions) {
    if (file.size(path) > 100e6) return(res)
    lines <- readLines(path, warn = FALSE)
    toks <- unlist(regmatches(lines, gregexpr("[A-Za-z0-9_.-]+", lines)), use.names = FALSE)
    res$scanned <- TRUE
    res$hits <- length(unique(toks[toks %in% ids]))
    if (ext %in% c("csv", "tsv")) res$rows <- max(0L, length(lines) - 1L)
  } else if (ext == "rds") {
    obj <- tryCatch(readRDS(path), error = function(e) NULL)
    res$scanned <- !is.null(obj)
    res$hits <- length(intersect(strings_of(obj), ids))
    res$rows <- max_rows_of(obj)
  } else if (ext %in% c("rdata", "rda")) {
    env <- new.env()
    ok <- !is.null(tryCatch(load(path, envir = env), error = function(e) NULL))
    res$scanned <- ok
    if (ok) {
      objs <- as.list(env)
      res$hits <- length(intersect(strings_of(objs), ids))
      res$rows <- max_rows_of(objs)
    }
  }
  res
}

strings_of <- function(x, depth = 0) {
  if (depth > 20 || is.null(x) || is.function(x) || is.environment(x)) return(character())
  out <- character()
  if (is.character(x) || is.factor(x)) out <- as.character(x)
  if (is.list(x)) out <- c(out, unlist(lapply(unclass(x), strings_of, depth = depth + 1), use.names = FALSE))
  unique(c(out, names(x)))
}

max_rows_of <- function(x, depth = 0) {
  if (depth > 20 || is.null(x) || is.function(x) || is.environment(x)) return(NA_integer_)
  if (!is.null(dim(x))) return(as.integer(dim(x)[1]))
  if (is.list(x) && length(x)) {
    r <- vapply(unclass(x), max_rows_of, integer(1), depth = depth + 1)
    if (all(is.na(r))) return(NA_integer_)
    return(max(r, na.rm = TRUE))
  }
  NA_integer_
}
