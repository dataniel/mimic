#' Describe an output file for the checker
#'
#' Called in the researcher's script after a file is written. Records what
#' the file shows, which column holds the number of persons, which columns
#' group the table, where the numbers behind a figure are, and what changed
#' since the last request. [inspect()] reads the descriptions: it checks a
#' table by the columns named here instead of guessing them from the
#' column names, checks the numbers behind a figure, and shows the
#' descriptions to the approver in [write_report()].
#'
#' @section Why it exists:
#' A checker cannot release what they do not understand. OpenSAFELY reports
#' that about 25 % of rejected requests lack context for the output and
#' 10 % lack the data behind a figure
#' (<https://docs.opensafely.org/output-checking/>). [inspect()] also has to
#' guess: a count column is found by its name (`n`, `count`, ...), and a
#' figure cannot be read at all. A description removes the guesswork.
#'
#' @section Where descriptions are kept:
#' All descriptions go into one text file, `output_descriptions.dcf`, in
#' `dir`: the working directory, which is the output directory when the
#' script runs in [trial_run()] or [real_run()]. Describing a file again
#' replaces its description, so the script can be run again. The file is
#' output too: it is checked and released with the rest, so the note must
#' not mention values of persons.
#'
#' @section Figures:
#' A figure cannot be checked automatically, but the numbers behind it can.
#' Give them as `data`, a data frame such as the one the figure was drawn
#' from: it is written next to the figure as `<figure>_data.csv` and checked
#' like any table. A figure whose numbers need changes needs changes too; a
#' figure is never approved automatically, because only a person can see
#' whether it shows more than its numbers (single points in a scatter plot,
#' a small bar). Give the counts behind the figure, not row-level data.
#'
#' @param file Path of the file written, relative to `dir`.
#' @param note What the file shows, in a sentence or two.
#' @param count For a table: the column with the number of persons per row.
#' @param by For a table: the columns that group it (one row per
#'   combination).
#' @param data For a figure: the numbers behind it, as a data frame (written
#'   to `<figure>_data.csv`) or the path of a file in `dir` that holds them.
#' @param changes What changed since the output was last requested.
#' @param dir The output directory. In [trial_run()] and [real_run()] the
#'   working directory is the output directory.
#' @return The path of the description file, invisibly.
#' @family safe output
#' @export
#' @examples
#' out <- file.path(tempdir(), "describe_example")
#' dir.create(out, showWarnings = FALSE)
#' tab <- safe_count(fake_persons, c("sex", "occupation"))
#' write.csv(tab, file.path(out, "sex_occupation.csv"), row.names = FALSE)
#' describe_output("sex_occupation.csv", "Persons by sex and occupation, 2024.",
#'                 count = "n", by = c("sex", "occupation"), dir = out)
#' inspect(out)
describe_output <- function(file, note, count = NULL, by = NULL, data = NULL,
                            changes = NULL, dir = ".") {
  one_string <- function(x, arg, null_ok = TRUE) {
    if (is.null(x) && null_ok) return(NULL)
    if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(trimws(x))) {
      cli::cli_abort("{.arg {arg}} must be a single non-empty string.")
    }
    # one line per field keeps the file simple to read and to check
    trimws(gsub("[\r\n]+", " ", x))
  }
  file <- one_string(file, "file", null_ok = FALSE)
  note <- one_string(note, "note", null_ok = FALSE)
  count <- one_string(count, "count")
  changes <- one_string(changes, "changes")
  if (!is.null(by) && (!is.character(by) || !length(by) || anyNA(by) || any(!nzchar(by)))) {
    cli::cli_abort("{.arg by} must name one or more columns.")
  }
  if (!is.character(dir) || length(dir) != 1 || !dir.exists(dir)) {
    cli::cli_abort("{.arg dir} must be an existing directory.")
  }
  file <- gsub("\\\\", "/", file)
  if (file == description_file) cli::cli_abort("{.file {file}} is the description file itself.")
  path <- file.path(dir, file)
  if (!file.exists(path)) {
    cli::cli_abort(c("{.file {file}} does not exist in the output directory.",
                     "i" = "Write the file first, then describe it."))
  }

  # the columns named must be in a table the checker will read
  ext <- tolower(tools::file_ext(file))
  if (length(c(count, by)) && ext %in% c("csv", "tsv")) {
    df <- read_table_file(path, ext)
    missing <- setdiff(c(count, by), names(df))
    if (length(missing)) {
      cli::cli_abort("{.file {file}} has no column{?s} {.field {missing}}.")
    }
  }

  data_file <- NULL
  if (is.data.frame(data)) {
    data_file <- paste0(tools::file_path_sans_ext(file), "_data.csv")
    utils::write.csv(data, file.path(dir, data_file), row.names = FALSE)
  } else if (!is.null(data)) {
    data_file <- gsub("\\\\", "/", one_string(data, "data"))
    if (!file.exists(file.path(dir, data_file))) {
      cli::cli_abort(c("{.file {data_file}} does not exist in the output directory.",
                       "i" = "Write the numbers behind the figure first, or give them as a data frame."))
    }
  }

  rec <- c(File = file, Note = note, Count = count,
           By = if (length(by)) paste(by, collapse = ", "), Data = data_file, Changes = changes)
  desc_path <- file.path(dir, description_file)
  old <- read_description_records(desc_path)
  old <- Filter(function(r) !identical(r[["File"]], file), old)
  write_description_records(c(old, list(rec)), desc_path)
  invisible(desc_path)
}

description_file <- "output_descriptions.dcf"
description_fields <- c("File", "Note", "Count", "By", "Data", "Changes")

read_description_records <- function(path) {
  if (!file.exists(path)) return(list())
  m <- tryCatch(read.dcf(path, fields = description_fields), error = function(e) NULL)
  if (is.null(m) || !nrow(m)) return(list())
  lapply(seq_len(nrow(m)), function(i) {
    r <- m[i, ]
    r[!is.na(r)]
  })
}

write_description_records <- function(recs, path) {
  m <- matrix(NA_character_, nrow = length(recs), ncol = length(description_fields),
              dimnames = list(NULL, description_fields))
  for (i in seq_along(recs)) m[i, names(recs[[i]])] <- recs[[i]]
  # columns nobody used would be written as empty fields
  m <- m[, colSums(!is.na(m)) > 0, drop = FALSE]
  write.dcf(m, path, keep.white = description_fields)
}

# Descriptions for inspect(): a list named by file, each with note, count,
# by, data and changes (NULL when not given).
read_descriptions <- function(output_dir) {
  recs <- read_description_records(file.path(output_dir, description_file))
  out <- list()
  for (r in recs) {
    if (is.na(r["File"])) next
    get <- function(f) if (f %in% names(r)) unname(r[[f]]) else NULL
    by <- get("By")
    out[[r[["File"]]]] <- list(note = get("Note"), count = get("Count"),
                               by = if (is.null(by)) NULL else trimws(strsplit(by, ",", fixed = TRUE)[[1]]),
                               data = get("Data"), changes = get("Changes"))
  }
  out
}
