#' Run a script on the real data
#'
#' Runs the researcher's script on the real data in a
#' fresh, isolated R session inside the secure environment. Output is kept
#' in `output_dir`, and a log of the code (optionally with its printed output,
#' cleaned of data values) is placed next to the output. Check the output with
#' [inspect()] before anything is released.
#'
#' @section Safe error report:
#' When the script fails, the researcher cannot see the data to find out
#' why. [real_run()] returns what helps without revealing anything:
#' * the error class and the function that raised it,
#' * the line in the script,
#' * the error message with data values removed: quoted parts that are not
#'   words from the script or names in the data become `<value>`, every
#'   number becomes `<n>`, and identifiers, free text and rare values are
#'   removed,
#' * for every variable mentioned in the script or the message: class, share
#'   missing (rounded), number of distinct values, whether there are empty
#'   strings, and whether it is entirely missing.
#'
#' The full call is never kept, because a call can carry a whole data frame.
#'
#' @section What the log contains:
#' The script is run with `source(echo = TRUE)`. By default
#' (`log = "code_only"`) the log keeps only the code, and each stretch of
#' printed output becomes `<output not kept>`. The researcher sees where the
#' script got to, but no output; results must come as files that
#' [inspect()] checks.
#'
#' With `log = "cleaned"` the printed output is kept too, with values that
#' could point to a person replaced by `<value>`: identifiers, free text,
#' and any value (string, date or number with at least three digits, or a
#' rare age or other small number in a quasi-identifier) held by fewer than
#' `min_count` persons. Persons are counted on the identifier columns of
#' each table, so a code one person has on many contacts is still rare.
#' Numbers are compared at the precision they are printed with, so the
#' minimum shown by `summary()` as `38390` is found even when the value is
#' `38389.6`, and also when written with thousands separators
#' (`9.800.000`, `38,390`). Dates are found in the common ways of writing
#' them (`1959-01-31`, `31.01.1959`, `31/01/1959`, `31011959`), and free
#' text also when only its first part is printed.
#' A line with two or more such values is removed entirely: it is most
#' likely a printed row, and the common values left in it (sex, settlement,
#' occupation) could still single out a person in a small place. Common
#' values elsewhere are left, so the log stays readable.
#'
#' Cleaning is a search for known values, so it cannot find what a script
#' has transformed: a name in capitals, a date with the month spelled out, a
#' piece cut from the middle of a note, a value divided by 1000. A line with
#' a single rare value also keeps its common values, and those can still be
#' a rare combination when a script prints rows. That is why the code-only
#' log is the default; choose the cleaned log for scripts staff have read.
#'
#' The names of files the script wrote, and of writes that were blocked, are
#' cleaned the same way, because a script can name a file after a person.
#'
#' `real_run()` does not know the keys of the tables. Persons are found
#' through identifier columns: names like `person_id`, `id`, `cpr...` or
#' `pnr...`, and the `id_vars` of the configuration. When the key has
#' another name (`lbnr`, `person`), add it to `id_vars`; otherwise rare
#' values are counted in rows, and a code one person has on twelve contacts
#' looks common.
#'
#' @section Limits:
#' Write calls in base R, utils, grDevices and common packages (data.table,
#' readr, writexl, openxlsx, haven, arrow, ggplot2, qs, fst, ...) are
#' blocked when they target a path outside `output_dir`. This is a safety
#' net, not a sandbox: a determined script can write elsewhere through
#' other functions. File system permissions in the secure environment must
#' enforce what may be written where.
#'
#' The data are passed to the child session through a temporary file that
#' callr deletes afterwards; it lies in the session's temporary directory
#' inside the secure environment.
#'
#' @param script Path to an R script, or R code as text.
#' @param real_data The real data: a data frame (available to the script as
#'   `data`) or a named list of data frames (available under their names).
#' @param output_dir Directory for the output. Created if needed; must be
#'   empty, so only output from this run can be released.
#' @param timeout Maximum run time in seconds.
#' @param config A [mimic_config()]; `NULL` uses the defaults. The
#'   script's [safe_count()], [safe_summary()] and [safe_model()] follow it.
#' @param raw_log Optional path where the uncleaned log is saved for staff.
#'   It must lie outside `output_dir` and must not be released.
#' @param log `"code_only"` (the default) keeps only the code; `"cleaned"`
#'   also keeps its printed output with data values removed. See What the
#'   log contains.
#' @return An object of class `mimic_run` that is safe to pass on to
#'   the researcher: `status` (`"ok"` or `"error"`), `error` (the safe
#'   error report, see below), `files` written, `blocked` writes, `log_file`
#'   (the cleaned log in `output_dir`), `redactions` and `warnings`
#'   (cleaned).
#' @export
#' @examples
#' \donttest{
#' out <- file.path(tempdir(), "run1")
#' r <- real_run(c(
#'   "tab <- table(persons$sex)",
#'   "write.csv(as.data.frame(tab), 'sex.csv', row.names = FALSE)"
#' ), list(persons = fake_persons), out)
#' r
#'
#' # a failing script gets a safe error report
#' r2 <- real_run("fit <- lm(income ~ settlment, data = persons)",
#'                list(persons = fake_persons), file.path(tempdir(), "run2"))
#' r2
#' }
real_run <- function(script, real_data, output_dir, timeout = 3600, config = NULL,
                     raw_log = NULL, log = c("code_only", "cleaned")) {
  cfg <- as_config(config)
  log <- match.arg(log)
  sc <- as_script(script)
  if (sc$temp) on.exit(unlink(sc$path), add = TRUE)
  data <- as_script_data(real_data, "real_data")

  if (!is.character(output_dir) || length(output_dir) != 1) {
    cli::cli_abort("{.arg output_dir} must be a single path.")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/")
  if (length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) {
    cli::cli_abort(c("{.arg output_dir} must be empty, so only output from this run can be released.",
                     "i" = "Use a new directory for each run."))
  }
  if (!is.null(raw_log)) {
    rl <- normalizePath(dirname(raw_log), winslash = "/", mustWork = FALSE)
    if (is_inside(rl, output_dir)) {
      cli::cli_abort("{.arg raw_log} must lie outside {.arg output_dir}; the raw log must not be released.")
    }
  }

  log_tmp <- tempfile("run_raw_", fileext = ".txt")
  on.exit(unlink(log_tmp), add = TRUE)
  # the deny list is built while the script runs
  res <- run_script(sc$path, data, output_dir, guard = TRUE, timeout = timeout, log_path = log_tmp,
                    meanwhile = function() sensitive_values(data, cfg), config = cfg)
  raw <- if (file.exists(log_tmp)) readLines(log_tmp, warn = FALSE) else character()
  if (!is.null(raw_log)) writeLines(raw, raw_log)

  sens <- res$meanwhile
  cleaned <- redact(raw, sens, drop_rows = TRUE)
  lines <- if (log == "code_only") code_lines(raw, cleaned$lines) else cleaned$lines
  log_file <- file.path(output_dir, "run_log.txt")
  writeLines(c(sprintf("# mimic run log%s, cleaned: %d data value(s) found, %d line(s) removed",
                       if (log == "code_only") " (code only)" else "", cleaned$n, cleaned$dropped),
               lines), log_file)

  words <- unique(c(script_words(sc$path), names(data), unlist(lapply(data, names))))
  warnings <- vapply(res$warnings %||% character(), sanitize_message, character(1),
                     sens = sens, known = words, USE.NAMES = FALSE)

  writes <- writes_df(res$writes)
  blocked <- writes[!writes$inside, c("fun", "path"), drop = FALSE]
  # file names are made by the script and can carry values: paste0(id, ".csv")
  blocked$path <- redact(blocked$path, sens)$lines
  rownames(blocked) <- NULL

  error <- NULL
  if (isTRUE(res$timeout)) {
    error <- list(class = "timeout", fun = NA_character_, line = NA_integer_,
                  message = sprintf("The script did not finish within %d seconds.", timeout),
                  variables = variable_report(data, character(), cfg))
  } else if (isTRUE(res$crash)) {
    error <- list(class = "crash", fun = NA_character_, line = NA_integer_,
                  message = "The R session running the script crashed.",
                  variables = variable_report(data, character(), cfg))
  } else if (!is.null(res$error)) {
    msg <- sanitize_message(res$error$message, sens, words)
    cols <- unlist(lapply(data, names))
    msg_words <- unlist(regmatches(msg, gregexpr("[A-Za-z0-9_.]+", msg)))
    involved <- intersect(cols, unique(c(script_words(sc$path), msg_words)))
    error <- list(class = setdiff(res$error$class, c("simpleError", "error", "condition"))[1] %||% "error",
                  fun = res$error$fun, line = res$error$line, message = msg,
                  variables = variable_report(data, involved, cfg))
    if (is.na(error$class)) error$class <- "error"
  }

  files <- redact(setdiff(res$files$file, "run_log.txt"), sens)$lines
  structure(list(
    status = if (is.null(error)) "ok" else "error",
    script = if (sc$temp) "(code)" else basename(sc$path),
    error = error,
    files = files,
    blocked = blocked,
    log_file = log_file,
    redactions = cleaned$n,
    lines_removed = cleaned$dropped,
    warnings = unique(warnings),
    seconds = round(res$seconds %||% NA_real_)
  ), class = "mimic_run")
}

# The echoed code of a log ("> " and "+ " lines, decided on the raw log),
# with every stretch of printed output replaced by one marker line.
code_lines <- function(raw, cleaned) {
  code <- grepl("^[>+]( |$)", raw)
  if (!length(code)) return(character())
  start <- !code & c(TRUE, code[-length(code)])
  out <- ifelse(code, cleaned, NA_character_)
  out[start] <- "<output not kept>"
  out[!is.na(out)]
}

#' @export
print.mimic_run <- function(x, ...) {
  cli::cli_text("{.strong <mimic_run>} {x$script}")
  if (x$status == "ok") {
    took <- if (is.na(x$seconds) || x$seconds < 1) "under a second" else paste("about", x$seconds, "seconds")
    cli::cli_alert_success("The script ran through on the real data in {took}.")
  } else {
    e <- x$error
    where <- if (!is.na(e$line)) paste0(" at line ", e$line) else ""
    fun <- if (!is.na(e$fun)) paste0(" in ", e$fun, "()") else ""
    cli::cli_alert_danger("The script failed{where}{fun}.")
    cli::cli_text("Message (data values removed): {e$message}")
    if (nrow(e$variables)) {
      cli::cli_text("Variables involved:")
      cli_table(e$variables, 20)
    }
  }
  if (length(x$files)) cli::cli_alert_info("Output files: {x$files}") else cli::cli_alert_info("No output files.")
  if (nrow(x$blocked)) {
    cli::cli_alert_danger("Blocked {nrow(x$blocked)} write{?s} outside the output directory.")
    cli_table(x$blocked)
  }
  if (x$redactions) {
    cli::cli_alert_warning("{x$redactions} data value{?s} found in the log; {x$lines_removed} line{?s} removed.")
  }
  if (length(x$warnings)) cli::cli_alert_warning("{length(x$warnings)} warning{?s}, see {.code $warnings}.")
  cli::cli_alert_info("Check the output with {.fn inspect} before release.")
  invisible(x)
}
