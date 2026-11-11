#' Record released output in a register
#'
#' Once output is released it can be combined with everything released
#' after it. The classic case is two runs of the same script: a table for
#' all persons, and later one for all persons except a settlement. Each is
#' safe; subtracted, they reveal the settlement. `record_release()` keeps the
#' released tables in a register, and [inspect()] with the same `register`
#' compares every new table with them.
#'
#' Call it after the approver has decided, with the files that actually
#' left the secure environment. Files that need changes cannot be recorded.
#' Files sent for review by hand can, once a person has approved them.
#'
#' @section The register:
#' A directory in the secure environment with one `.rds` file per release:
#' its id, date, output directory, file names and the tables read from them.
#' Only the tables are kept, so figures and text are recorded by name only.
#' The register holds released output, but in full and in one place, so it
#' stays inside like the real data.
#'
#' @param x A `mimic_inspection` from [inspect()].
#' @param register Directory of the register. Created if needed; it must lie
#'   outside the output directory.
#' @param files Files that were released. By default those that were
#'   approved; files for review by hand are then named in a warning, because
#'   a released file missing from the register is not compared with later
#'   output.
#' @return The path of the recorded release, invisibly.
#' @export
#' @examples
#' reg <- file.path(tempdir(), "register")
#' out1 <- file.path(tempdir(), "release_1")
#' dir.create(out1, showWarnings = FALSE)
#' write.csv(data.frame(sex = c("F", "M"), n = c(985, 1015)),
#'           file.path(out1, "by_sex.csv"), row.names = FALSE)
#' record_release(inspect(out1, register = reg), reg)
#'
#' # a month later, the same table without one small settlement
#' out2 <- file.path(tempdir(), "release_2")
#' dir.create(out2, showWarnings = FALSE)
#' write.csv(data.frame(sex = c("F", "M"), n = c(983, 1014)),
#'           file.path(out2, "by_sex.csv"), row.names = FALSE)
#' inspect(out2, register = reg)
record_release <- function(x, register, files = NULL) {
  if (!inherits(x, "mimic_inspection")) {
    cli::cli_abort("{.arg x} must be created with {.fn inspect}.")
  }
  if (!is.character(register) || length(register) != 1 || is.na(register)) {
    cli::cli_abort("{.arg register} must be a single path.")
  }
  f <- x$files
  by_hand <- character()
  if (is.null(files)) {
    files <- f$file[f$verdict == "APPROVED"]
    by_hand <- f$file[f$verdict == "REVIEW BY HAND"]
  }
  unknown <- setdiff(files, f$file)
  if (length(unknown)) {
    cli::cli_abort(c("{.arg files} not in the inspection: {.file {unknown}}.",
                     "i" = "Record the files of {.path {x$output_dir}} as {.fn inspect} saw them."))
  }
  bad <- files[f$verdict[match(files, f$file)] == "NEEDS CHANGES"]
  if (length(bad)) {
    cli::cli_abort(c("{cli::qty(length(bad))}{?This file needs/These files need} changes and must not be released: {.file {bad}}.",
                     "i" = "Fix the output, run {.fn inspect} again and record the new inspection."))
  }
  reg <- normalizePath(register, winslash = "/", mustWork = FALSE)
  if (is_inside(reg, x$output_dir)) {
    cli::cli_abort("{.arg register} must lie outside the output directory, so it is not released with the output.")
  }
  # a hand-approved table left out of the register is not compared with
  # later releases, and nothing would say so. Warned after the approved
  # files are recorded, so a warning turned into an error does not stop that.
  warn_by_hand <- function() {
    if (!length(by_hand)) return(invisible())
    nh <- length(by_hand)
    cli::cli_warn(paste0(
      "Not recorded: {nh} file{?s} for review by hand ({.file {by_hand}}). ",
      "If {cli::qty(nh)}{?it was/they were} released, record {cli::qty(nh)}{?it/them} with ",
      "{.code files = c(...)} together with the approved files."
    ))
  }
  if (!length(files)) {
    cli::cli_alert_info("No files to record.")
    warn_by_hand()
    return(invisible(NULL))
  }

  dir.create(register, recursive = TRUE, showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  id <- stamp
  i <- 1L
  while (file.exists(file.path(register, paste0("release_", id, ".rds")))) {
    i <- i + 1L
    id <- paste0(stamp, "-", i)
  }
  tables <- x$tables[table_file(names(x$tables)) %in% files]
  rec <- list(id = id, created = format(Sys.time(), "%Y-%m-%d %H:%M"),
              output_dir = x$output_dir, files = files, tables = tables,
              version = as.character(utils::packageVersion("mimic")))
  path <- file.path(register, paste0("release_", id, ".rds"))
  saveRDS(rec, path)
  cli::cli_alert_success("Recorded {length(files)} file{?s} with {length(tables)} table{?s} as release {id}.")
  warn_by_hand()
  invisible(path)
}

# All releases in a register, oldest first. A register that does not exist
# yet holds none.
read_register <- function(register) {
  if (!is.character(register) || length(register) != 1 || is.na(register)) {
    cli::cli_abort("{.arg register} must be a single path.")
  }
  if (!dir.exists(register)) return(list())
  fs <- list.files(register, pattern = "^release_.*\\.rds$", full.names = TRUE)
  # release_<stamp>.rds, release_<stamp>-2.rds, ...: sorted as text, "-" comes
  # before "." and "-10" before "-2", so order by stamp and number
  b <- sub("^release_(.*)\\.rds$", "\\1", basename(fs))
  stamp <- sub("^([0-9]{8}-[0-9]{6}).*$", "\\1", b)
  num <- suppressWarnings(as.integer(sub("^[0-9]{8}-[0-9]{6}-?", "", b)))
  num[is.na(num)] <- 1L
  fs <- fs[order(stamp, num)]
  lapply(fs, function(p) {
    rec <- tryCatch(readRDS(p), error = function(e) NULL)
    if (is.null(rec) || is.null(rec$tables)) {
      cli::cli_abort("{.file {p}} in the register is not a release recorded by {.fn record_release}.")
    }
    rec
  })
}

# Is `path` the directory `dir` or inside it? Compared as text, with a
# separator, so "out2" is not inside "out".
is_inside <- function(path, dir) {
  p <- tolower(gsub("/+$", "", path))
  d <- tolower(gsub("/+$", "", dir))
  p == d || startsWith(p, paste0(d, "/"))
}
