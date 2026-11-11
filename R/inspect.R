#' Check output before release
#'
#' The final inspection before anything leaves the secure environment.
#' `inspect()` reads every file in `output_dir`, runs the relevant
#' [checks] and gives each file one verdict for the person who approves the
#' release:
#'
#' * **APPROVED**: every check passed.
#' * **NEEDS CHANGES**: a rule is broken; the reasons say which and where.
#' * **REVIEW BY HAND**: the file could not be checked automatically
#'   (images, spreadsheets, unknown objects) or something needs a human
#'   judgement. When in doubt, mimic does not approve.
#'
#' @section What is checked:
#' * **Tables** (`.csv`, `.tsv`, data frames in `.rds`/`.RData`):
#'   - cells below `min_count` with [check_min_count()]: a count column
#'     named `n`, `count`, `freq` or similar, or a cross table whose numeric
#'     cells are all whole numbers,
#'   - minimum, maximum and range values with [check_extremes()],
#'   - tables of magnitudes (sums, means, medians) without a count column
#'     are sent for review by hand, because the cell sizes behind them are
#'     unknown,
#'   - row-level data: identifier columns, rows that repeat a combination
#'     of the grouping columns (an aggregated table has one row per
#'     combination), or (with `real_data`) many rows or identifier values
#'     from the real data.
#'   - suppressed cells (`NA`, `"<5"`, `"."`, ...) that can be recovered from
#'     totals in the table with [check_suppression()],
#'   - groups whose persons all share one value with
#'     [check_group_disclosure()],
#'   - rows with a suppressed count that still show a share, sum, count of
#'     part of the group or other number (`n = "<5"` next to `pct = 0.15`
#'     or `n_female = 2`),
#'   - counts of part of the group (`n_female` next to `n`) checked for
#'     small cells like the count, and a suppressed one that is the count
#'     minus the other parts (`n_female = "<5"` next to `n` and `n_male`),
#'   - tables with neither counts nor numbers (a list of dates, places or
#'     notes) are sent for review by hand: they list values rather than
#'     aggregate them,
#' * **Models** in `.rds`/`.RData` with [check_model()].
#' * **Pairs of tables** with [check_differencing()], when they have the
#'   same grouping columns or one grouping is part of the other (the finer
#'   table is summed up; rows labelled `"Total"` are left out).
#' * **Text** (the run log, `.txt`, `.md`, `.html`): with `real_data`,
#'   searched for values that could point to a person.
#'
#' @section Descriptions:
#' When the researcher described the files with [describe_output()],
#' `inspect()`
#' * checks a table by the count and grouping columns named there instead
#'   of guessing them, and sends it back when a named column is missing,
#' * checks the numbers behind a figure, and sends the figure back when
#'   they need changes or are missing (the figure itself is still reviewed
#'   by hand),
#' * checks the description file like other text, because the researcher
#'   wrote it,
#' * shows the descriptions in [write_report()].
#'
#' With `require_description = TRUE` in [mimic_config()], every output
#' file without a description is sent back.
#'
#' A table of model coefficients is approved when it carries
#' `df_residual` (and `observations`) at or above the limits, as tables from
#' [safe_model()] do; otherwise it is reviewed by hand.
#'
#' @section With the real data:
#' Some rules can only be checked against the microdata. Give `real_data`
#' and `inspect()` also
#' * applies the dominance rules with [check_dominance()] to tables of sums
#'   and means: a column such as `mean_income` or `sum_income` is matched to
#'   the variable `income`, and the table's grouping columns to the
#'   variables of the same name,
#' * flags numbers in tables that equal a value held by fewer than
#'   `min_count` persons (often a median or extreme of a small group),
#' * searches text files, the text cells of tables and file names for
#'   identifiers, free text and rare values (a day-precise date, a code or a
#'   settlement held by fewer than `min_count` persons).
#'
#' Persons are found through identifier columns in the real data: names
#' like `person_id`, `cpr...` or `pnr...`, and the `id_vars` of the
#' configuration. Add a key with another name (`lbnr`) to `id_vars`, or
#' rare values are counted in rows.
#'
#' @section Counts are taken as persons:
#' A count column is read as a number of persons, because that is what
#' `min_count` is about. A table of contacts per settlement can pass with 12
#' contacts that all belong to one person. Without the real data `inspect()`
#' cannot tell; with `real_data` a label held by fewer than `min_count`
#' persons is found whatever the count next to it says. Ask for counts of
#' persons, not of rows, when the script works on a linked table.
#'
#' @section Earlier releases:
#' The most realistic differencing attack does not use two tables from one
#' run but two runs: the same table for all persons, and a month later for
#' all persons except one settlement. Each run has its own empty output
#' directory, so `inspect()` only sees it with a register. Record what is
#' released with [record_release()] and give the same `register` every
#' time: each table is then also compared with every table released before.
#'
#' @param output_dir Directory with the output, normally from [real_run()].
#' @param config A [mimic_config()]; `NULL` uses the defaults.
#' @param real_data Optional real data (a data frame or a named list of data
#'   frames) for the checks that need microdata.
#' @param register Optional directory with the register of earlier releases
#'   kept by [record_release()]. A directory that does not exist yet holds
#'   no releases.
#' @return An object of class `mimic_inspection`: `files`, a data frame
#'   with one row per file (`file`, `type`, `verdict`, `reasons`,
#'   `description`),
#'   `checks`, the full checks per file, and `tables`, the tables read from
#'   the files. It holds output that is not yet approved and stays in the
#'   secure environment. Write it for the approver with [write_report()].
#' @export
#' @examples
#' out <- file.path(tempdir(), "inspect_example")
#' dir.create(out, showWarnings = FALSE)
#' tab <- as.data.frame(table(settlement = fake_persons$settlement))
#' names(tab)[2] <- "n"
#' write.csv(tab, file.path(out, "by_settlement.csv"), row.names = FALSE)
#' write.csv(data.frame(sex = c("F", "M"), n = c(985, 1015)),
#'           file.path(out, "by_sex.csv"), row.names = FALSE)
#' res <- inspect(out, real_data = list(persons = fake_persons))
#' res
inspect <- function(output_dir, config = NULL, real_data = NULL, register = NULL) {
  cfg <- as_config(config)
  if (!is.character(output_dir) || length(output_dir) != 1 || !dir.exists(output_dir)) {
    cli::cli_abort("{.arg output_dir} must be an existing directory.")
  }
  releases <- if (is.null(register)) NULL else read_register(register)
  real <- if (is.null(real_data)) NULL else as_script_data(real_data, "real_data")
  ctx <- inspect_context(real, cfg)

  files <- list.files(output_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  desc <- read_descriptions(output_dir)
  checks <- list()
  types <- character()
  tables <- list()
  for (f in files) {
    res <- inspect_file(file.path(output_dir, f), f, cfg, ctx, hint = desc[[f]])
    checks[[f]] <- res$checks
    # a script can name a file after a person: paste0(id, ".csv")
    if (!is.null(ctx) && redact(f, ctx$sens)$n > 0) {
      checks[[f]] <- c(checks[[f]], list(new_check("row_level", f, "fail",
                                                   "The file name contains a value that could point to a person.")))
    }
    types[[f]] <- res$type
    for (i in seq_along(res$tables)) {
      nm <- names(res$tables)[i]
      tables[[paste0(f, if (nzchar(nm)) paste0(" [", nm, "]"))]] <- res$tables[[i]]
    }
  }

  # differencing between tables with the same grouping and a count column
  # and with every table released before
  diff <- c(differencing_pairs(tables, cfg), register_pairs(tables, releases, cfg))
  for (d in diff) {
    for (f in d$files) checks[[f]] <- c(checks[[f]], list(d$check))
  }
  checks <- description_checks(files, types, checks, desc, cfg)

  summary <- data.frame(
    file = files,
    type = unname(types[files]),
    verdict = vapply(files, function(f) verdict_of(checks[[f]]), character(1), USE.NAMES = FALSE),
    reasons = vapply(files, function(f) reasons_of(checks[[f]]), character(1), USE.NAMES = FALSE),
    description = vapply(files, function(f) desc[[f]]$note %||% "", character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
  structure(list(files = summary, checks = checks, output_dir = normalizePath(output_dir, winslash = "/"),
                 config = cfg, with_real_data = !is.null(real),
                 register = if (is.null(register)) NULL else normalizePath(register, winslash = "/", mustWork = FALSE),
                 n_releases = length(releases), tables = tables, descriptions = desc,
                 created = format(Sys.time(), "%Y-%m-%d %H:%M")),
            class = "mimic_inspection")
}

# What the descriptions add once every file has been checked: the numbers
# behind a figure decide for the figure too, described files that are not
# in the output are noted, and with require_description every output
# without a description is sent back.
description_checks <- function(files, types, checks, desc, cfg) {
  verdicts <- vapply(files, function(f) verdict_of(checks[[f]]), character(1))
  data_files <- unlist(lapply(desc, `[[`, "data"), use.names = FALSE)
  for (f in files) {
    d <- desc[[f]]
    if (identical(types[[f]], "image") && !is.null(d$data)) {
      ch <- if (!d$data %in% files) {
        new_check("description", f, "fail",
                  sprintf("The numbers behind the figure (%s) are not in the output.", d$data))
      } else if (verdicts[[d$data]] == "NEEDS CHANGES") {
        new_check("description", f, "fail",
                  sprintf("The numbers behind the figure (%s) need changes, so the figure does too.", d$data))
      } else {
        new_check("description", f, "pass", sprintf("The numbers behind the figure are in %s.", d$data))
      }
      checks[[f]] <- c(checks[[f]], list(ch))
    }
    exempt <- f %in% c(description_file, "run_log.txt", data_files)
    if (isTRUE(cfg$require_description) && is.null(d) && !exempt) {
      checks[[f]] <- c(checks[[f]], list(new_check("description", f, "fail",
                                                   "No description; say what the file shows with describe_output().")))
    }
  }
  gone <- setdiff(names(desc), files)
  if (length(gone) && description_file %in% files) {
    checks[[description_file]] <- c(checks[[description_file]], list(new_check(
      "description", description_file, "warn",
      sprintf("Describes %s, which %s not in the output.", paste(gone, collapse = ", "),
              if (length(gone) == 1) "is" else "are"))))
  }
  checks
}

verdict_of <- function(checks) {
  st <- vapply(checks, `[[`, character(1), "status")
  if (any(st == "fail")) "NEEDS CHANGES" else if (any(st == "warn") || !length(st)) "REVIEW BY HAND" else "APPROVED"
}

reasons_of <- function(checks) {
  bad <- Filter(function(ch) ch$status != "pass", checks)
  if (!length(bad)) return("")
  paste(unique(vapply(bad, `[[`, character(1), "message")), collapse = " | ")
}

# What inspect() knows about the real data, if given.
inspect_context <- function(real, cfg) {
  if (is.null(real)) return(NULL)
  sens <- sensitive_values(real, cfg)
  ids <- character()
  for (df in real) {
    for (v in names(df)) {
      if (variable_role(v, df[[v]], NA_character_, cfg)$role == "id") {
        ids <- c(ids, as.character(df[[v]][!is.na(df[[v]])]))
      }
    }
  }
  list(real = real, sens = sens, ids = unique(ids),
       threshold = max(100, 0.5 * min(vapply(real, nrow, integer(1)))))
}

inspect_file <- function(path, label, cfg, ctx, hint = NULL) {
  ext <- tolower(tools::file_ext(path))
  base <- tolower(basename(path))
  manual <- function(type, why) {
    list(type = type, tables = list(),
         checks = list(new_check("manual", label, "warn", why)))
  }

  if (base == description_file) {
    # written by the researcher: prose can name a person or a value
    return(inspect_text(path, label, cfg, ctx, "description"))
  }
  if (base == "run_log.txt" || ext %in% c("txt", "log", "md", "html", "htm", "tex")) {
    return(inspect_text(path, label, cfg, ctx, if (base == "run_log.txt") "log" else "text"))
  }
  if (ext %in% c("csv", "tsv")) {
    df <- read_table_file(path, ext)
    if (is.null(df)) return(manual("table", "Could not be read as a table; review by hand."))
    df <- tidy_table(df)
    df <- with_hint(df, hint)
    return(list(type = "table", tables = stats::setNames(list(df), ""),
                checks = inspect_table(df, label, cfg, ctx, hint)))
  }
  if (ext %in% c("rds", "rdata", "rda")) {
    objs <- if (ext == "rds") {
      o <- tryCatch(readRDS(path), error = function(e) NULL)
      if (is.null(o)) NULL else list(o)
    } else {
      env <- new.env()
      ok <- tryCatch({
        load(path, envir = env)
        TRUE
      }, error = function(e) FALSE)
      if (ok) as.list(env) else NULL
    }
    if (is.null(objs)) return(manual("R object", "Could not be read; review by hand."))
    return(inspect_objects(objs, label, cfg, ctx, hint))
  }
  if (ext %in% c("png", "pdf", "jpg", "jpeg", "svg", "tif", "tiff", "bmp", "gif")) {
    why <- if (is.null(hint$data)) {
      "Figures cannot be checked automatically; check for small groups and single points by hand. Give the numbers behind it with describe_output(data = ) to have them checked."
    } else {
      "Figures cannot be checked automatically; check by hand that it shows no more than the numbers behind it (single points, small bars)."
    }
    return(manual("image", why))
  }
  if (ext %in% c("xlsx", "xls", "docx", "pptx", "odt", "ods")) {
    return(manual("document", "Office files are not read; export tables as csv to have them checked."))
  }
  manual("other", sprintf("Unknown file type '%s'; review by hand.", ext))
}

read_table_file <- function(path, ext) {
  first <- readLines(path, n = 1, warn = FALSE)
  if (!length(first)) return(NULL)
  sep <- if (ext == "tsv" || grepl("\t", first)) "\t" else if (lengths(regmatches(first, gregexpr(";", first))) >
                                                               lengths(regmatches(first, gregexpr(",", first)))) ";" else ","
  dec <- if (sep == ";") "," else "."
  tryCatch(utils::read.table(path, header = TRUE, sep = sep, dec = dec, quote = "\"",
                             check.names = FALSE, stringsAsFactors = FALSE, comment.char = "",
                             na.strings = c("NA", "")),
           error = function(e) NULL)
}

inspect_objects <- function(objs, label, cfg, ctx, hint = NULL) {
  checks <- list()
  tables <- list()
  types <- character()
  # a description is about the file: it applies when the file holds one table
  single <- length(objs) == 1
  walk <- function(x, nm, depth = 0) {
    if (depth > 5) return()
    tgt <- if (nm == "") label else paste0(label, " [", nm, "]")
    if (inherits(x, "summaryDefault")) x <- stats::setNames(as.numeric(x), names(x))
    if (is.data.frame(x) || is.matrix(x) || inherits(x, "table")) {
      h <- if (single && depth == 0) hint else NULL
      df <- with_hint(tidy_table(as_check_df(x)), h)
      tables <<- c(tables, stats::setNames(list(df), nm))
      types <<- c(types, "table")
      checks <<- c(checks, inspect_table(df, tgt, cfg, ctx, h))
    } else if (is_model(x)) {
      types <<- c(types, "model")
      checks <<- c(checks, list(check_model(x, cfg, target = tgt)))
    } else if (is.list(x) && !is.object(x)) {
      nms <- names(x) %||% rep("", length(x))
      for (i in seq_along(x)) {
        walk(x[[i]], if (nzchar(nms[i])) nms[i] else paste0(nm, "[[", i, "]]"), depth + 1)
      }
    } else if (is.atomic(x) && !is.null(names(x))) {
      types <<- c(types, "values")
      checks <<- c(checks, list(check_extremes(x, cfg, target = tgt)))
      if (!is.null(ctx) && is.numeric(x)) checks <<- c(checks, single_values(as.data.frame(as.list(x)), tgt, cfg, ctx))
      if (is.null(ctx) || !is.numeric(x)) {
        checks <<- c(checks, list(new_check("manual", tgt, "warn", "Named values without counts; review by hand.")))
      }
    } else {
      types <<- c(types, "other")
      checks <<- c(checks, list(new_check("manual", tgt, "warn",
                                         sprintf("Object of class %s cannot be checked automatically; review by hand.",
                                                 class(x)[1]))))
    }
  }
  for (i in seq_along(objs)) {
    nm <- names(objs)[i] %||% ""
    walk(objs[[i]], if (length(objs) == 1 && is.null(names(objs))) "" else nm)
  }
  type <- if (length(unique(types)) == 1) types[1] else "R objects"
  list(type = type, tables = tables, checks = checks)
}

is_model <- function(x) {
  inherits(x, c("lm", "glm", "coxph", "survreg", "nls", "lmerMod", "glmerMod", "gam", "polr", "multinom")) ||
    (is.object(x) && !is.null(tryCatch(stats::nobs(x), error = function(e) NULL)))
}

magnitude_pattern <- "(^|[^a-z])(sum|total|mean|avg|average|median|amount)([^a-z]|$)"
share_pattern <- "(^|[^a-z])(pct|percent|percentage|share|prop|proportion|rate|andel|procent)([^a-z]|$)"
# counts of part of a group next to the main count: n_female, count_2020
sub_count_pattern <- "(^|[^a-z])(n|count|freq|antal)([^a-z]|$)"

# The columns that group an aggregated table: labels and whole numbers such
# as a year, not the count column, magnitudes (sums, means) or shares.
grouping_columns <- function(df, count = NULL) {
  names(df)[vapply(names(df), function(v) {
    x <- df[[v]]
    !identical(v, count) &&
      !(is.numeric(x) && grepl(magnitude_pattern, tolower(v))) &&
      !(is.numeric(x) && grepl(share_pattern, tolower(v))) &&
      (!is.numeric(x) || all(is.na(x) | x == round(x)))
  }, logical(1), USE.NAMES = FALSE)]
}

# A table as R writes it: an unnamed column of row numbers (the write.csv()
# default, also after subsetting) is dropped, because it would make every
# row look unique; other unnamed columns are named. Count columns written
# with suppression markers ("<5", ".") become numeric with NA.
tidy_table <- function(df) {
  nm <- names(df)
  drop <- logical(length(nm))
  for (j in which(is.na(nm) | !nzchar(nm))) {
    v <- as.character(df[[j]])
    if (all(grepl("^[0-9]+$", v)) && !anyDuplicated(v)) drop[j] <- TRUE
    nm[j] <- if (j == 1) "row_names" else paste0("column_", j)
  }
  names(df) <- make.unique(nm)
  df <- df[!drop]
  df[] <- lapply(df, as_suppressed_numeric)
  df
}

# The count and grouping columns a description names, kept on the table so
# differencing (also against the register) uses them. Only when they are in
# the table; inspect_table() reports names that are not.
with_hint <- function(df, hint) {
  if (is.null(hint) || !length(c(hint$count, hint$by)) || !all(c(hint$count, hint$by) %in% names(df))) return(df)
  if (!is.null(hint$count) && is.numeric(df[[hint$count]])) attr(df, "mimic_count") <- hint$count
  if (!is.null(hint$by)) attr(df, "mimic_by") <- hint$by
  df
}

inspect_table <- function(df, label, cfg, ctx, hint = NULL) {
  out <- list()
  num <- names(df)[vapply(df, is.numeric, logical(1))]
  lab <- setdiff(names(df), num)
  # a description names the count and grouping columns; without one they
  # are guessed from the names
  if (!is.null(hint)) {
    missing <- setdiff(c(hint$count, hint$by), names(df))
    if (length(missing)) {
      out <- c(out, list(new_check("description", label, "fail",
                                   sprintf("The description names column(s) %s, which are not in the table.",
                                           paste(missing, collapse = ", ")))))
      hint <- NULL
    } else if (!is.null(hint$count) && !hint$count %in% num) {
      out <- c(out, list(new_check("description", label, "fail",
                                   sprintf("The description names %s as the count, but it does not hold numbers.",
                                           hint$count))))
      hint$count <- NULL
    }
  }
  count <- hint$count %||% guess_count(df)
  is_whole <- function(v) all(is.na(v) | (v >= 0 & v == round(v)))
  magnitude <- num[grepl(magnitude_pattern, tolower(num))]
  share <- num[grepl(share_pattern, tolower(num))]

  # row-level data
  # by name only: in an aggregated table every row has its own label, so
  # "all values unique" says nothing
  id_cols <- names(df)[grepl(id_name_pattern, names(df), ignore.case = TRUE)]
  if (length(id_cols)) {
    out <- c(out, list(new_check("row_level", label, "fail",
                                 sprintf("Has identifier column(s) %s; output must not contain identifiers.",
                                         paste(id_cols, collapse = ", ")))))
  }
  # An aggregated table has one row per combination of its grouping
  # columns. Repeated combinations mean rows of persons, even without
  # identifiers and below the row threshold: an extract of one town.
  grouping <- hint$by %||% grouping_columns(df, count)
  n_rep <- if (length(grouping) && nrow(df) > 1) sum(duplicated(df[grouping])) else 0L
  if (n_rep) {
    out <- c(out, list(new_check("row_level", label, "fail",
                                 sprintf("%d row(s) repeat a combination of %s; an aggregated table has one row per combination, so this looks like row-level data.",
                                         n_rep, paste(grouping, collapse = ", ")))))
  }
  if (!is.null(ctx)) {
    hits <- intersect(strings_of(df), ctx$ids)
    if (length(hits)) {
      out <- c(out, list(new_check("row_level", label, "fail",
                                   sprintf("Contains %d identifier value(s) from the real data.", length(hits)))))
    }
    if (nrow(df) >= ctx$threshold) {
      out <- c(out, list(new_check("row_level", label, "fail",
                                   sprintf("Has %d rows, which looks like row-level data.", nrow(df)))))
    }
  } else if (nrow(df) > 1000) {
    out <- c(out, list(new_check("row_level", label, "warn",
                                 sprintf("Has %d rows; make sure this is not row-level data.", nrow(df)))))
  }

  # cell sizes, suppressed cells and groups that share a value
  cell_checks <- function(d, by, cnt, note = "") {
    rename <- function(ch) {
      if (!is.null(ch$details)) names(ch$details)[names(ch$details) == ".column"] <- "column"
      if (nzchar(note)) ch$message <- paste(ch$message, note)
      ch
    }
    lapply(list(check_min_count(d, count = cnt, by = by, config = cfg, target = label),
                check_suppression(d, count = cnt, by = by, config = cfg, target = label),
                check_group_disclosure(d, count = cnt, by = by, config = cfg, target = label)),
           rename)
  }
  if (!is.null(count)) {
    # Counts of part of the group (n_female next to n) are whole numbers and
    # look like grouping columns. A count-like name alone is not enough:
    # antal_boern may be a category. A count of part of the group is not
    # needed to tell the rows apart.
    if (!is.null(hint$by)) {
      # the description says what groups the table; whole-number columns
      # with a count-like name besides those are counts of part of the group
      key <- hint$by
      sub_count <- intersect(setdiff(grouping_columns(df, count), key), num)
      sub_count <- sub_count[grepl(sub_count_pattern, tolower(sub_count))]
    } else {
      key <- grouping
      for (v in grouping) {
        rest <- setdiff(key, v)
        if (v %in% num && grepl(sub_count_pattern, tolower(v)) &&
            length(rest) > 0 && !anyDuplicated(df[rest])) {
          key <- rest
        }
      }
      sub_count <- setdiff(grouping, key)
    }
    # whole-number columns such as a year group the counts too
    out <- c(out, cell_checks(df, key, count))
    # each count of part of the group is a count of its own
    for (v in sub_count) {
      out <- c(out, cell_checks(df, key, v, note = sprintf("(column %s)", v)))
    }
    # a part that is the only one suppressed in its row is the count minus
    # the other parts; two suppressed parts protect each other
    if (length(sub_count) > 1) {
      na_parts <- is.na(as.matrix(df[sub_count]))
      open <- !is.na(df[[count]]) & rowSums(na_parts) == 1
      back <- sub_count[colSums(na_parts[open, , drop = FALSE]) > 0]
      if (length(back)) {
        out <- c(out, list(new_check("suppression", label, "fail",
                                     sprintf("Rows where only %s is suppressed still show %s and the other parts, which give it back by subtraction; suppress a second part or the count.",
                                             paste(back, collapse = " or "), count))))
      }
    }
    # a suppressed count is given back by a share, a sum or a count of part
    # of the group (n_female) in the same row
    other <- c(setdiff(num, c(count, grouping)), sub_count)
    sup <- is.na(df[[count]])
    leaks <- other[vapply(other, function(v) any(sup & !is.na(df[[v]]) & df[[v]] != 0), logical(1))]
    if (any(sup) && length(leaks)) {
      out <- c(out, list(new_check("suppression", label, "fail",
                                   sprintf("Rows with a suppressed count still show %s, which can give the count back; suppress them too.",
                                           paste(leaks, collapse = ", ")))))
    }
  } else if (length(num) && all(vapply(df[num], is_whole, logical(1))) &&
             !length(magnitude) && !length(share)) {
    long <- do.call(rbind, lapply(num, function(col) {
      d <- df[lab]
      d$.column <- col
      d$.n <- df[[col]]
      d
    }))
    out <- c(out, cell_checks(long, c(lab, ".column"), ".n", "(numeric cells read as counts)"))
  } else if (length(num) && any(c("Estimate", "estimate", "coef", "Coefficient") %in% names(df))) {
    out <- c(out, list(coefficient_check(df, label, cfg)))
  } else if (length(num)) {
    out <- c(out, list(new_check("min_count", label, "warn",
                                 "Table of magnitudes without a count column; add the number of persons per cell so cell sizes can be checked.")))
  } else {
    # labels only: a list of values (dates, places, notes) is not an
    # aggregated table, and nothing tells how many persons stand behind a row
    out <- c(out, list(new_check("row_level", label, "warn",
                                 "Table without counts or numbers: it lists values rather than aggregating them and may be row-level data; review by hand.")))
  }

  out <- c(out, list(check_extremes(df, cfg, target = label)))

  if (!is.null(ctx)) {
    # text cells are searched like text files: free text, identifiers and
    # values held by fewer than min_count persons
    chr <- names(df)[vapply(df, function(v) is.character(v) || is.factor(v), logical(1))]
    cells <- unique(unlist(lapply(df[chr], function(v) as.character(v[!is.na(v)])), use.names = FALSE))
    found <- if (length(cells)) redact(cells, ctx$sens)$n else 0L
    if (found) {
      out <- c(out, list(new_check("row_level", label, "fail",
                                   sprintf("Contains %d value(s) that could point to a person (free text, or a date, code or label held by fewer than %d persons).",
                                           found, cfg$min_count))))
    }
    out <- c(out, dominance_checks(df, magnitude, lab, label, cfg, ctx))
    out <- c(out, single_values(df[setdiff(num, count)], label, cfg, ctx))
  } else if (length(magnitude)) {
    out <- c(out, list(new_check("dominance", label, "warn",
                                 "Sums or means cannot be checked for dominance without the real data; give real_data.")))
  }
  out
}

# A table of coefficients says how many observations lie behind it only
# when it carries them, as safe_model() writes: df_residual and
# observations. Without them a person must judge.
coefficient_check <- function(df, label, cfg) {
  has <- function(v) v %in% names(df) && is.numeric(df[[v]]) && !all(is.na(df[[v]]))
  if (!has("df_residual")) {
    return(new_check("model", label, "warn",
                     "Table of model coefficients: check the number of observations and degrees of freedom behind the model by hand (safe_model() adds them)."))
  }
  low_df <- any(df$df_residual < cfg$min_df, na.rm = TRUE)
  low_n <- has("observations") && any(df$observations < cfg$min_count, na.rm = TRUE)
  if (low_df || low_n) {
    return(new_check("model", label, "fail",
                     sprintf("Table of model coefficients from a model with %s.",
                             paste(c(if (low_df) sprintf("residual degrees of freedom below %d", cfg$min_df),
                                     if (low_n) sprintf("fewer than %d observations", cfg$min_count)),
                                   collapse = " and "))))
  }
  new_check("model", label, "pass",
            sprintf("Table of model coefficients with residual degrees of freedom of at least %d.", cfg$min_df))
}

# Match columns like mean_income to the variable income in the real data
# and apply the dominance rules per group.
dominance_checks <- function(df, magnitude, lab, label, cfg, ctx) {
  out <- list()
  for (col in magnitude) {
    stem <- gsub(magnitude_pattern, "", tolower(col))
    stem <- gsub("^[^a-z0-9]+|[^a-z0-9]+$", "", stem)
    found <- FALSE
    for (tn in names(ctx$real)) {
      rd <- ctx$real[[tn]]
      var <- names(rd)[tolower(names(rd)) == stem]
      if (!length(var) || !is.numeric(rd[[var[1]]])) next
      by <- intersect(lab, names(rd))
      ch <- check_dominance(rd, value = var[1], by = if (length(by)) by else NULL,
                            config = cfg, target = paste0(label, ": ", col))
      out <- c(out, list(ch))
      found <- TRUE
      break
    }
    if (!found) {
      out <- c(out, list(new_check("dominance", label, "warn",
                                   sprintf("Could not match column %s to a variable in the real data; check dominance by hand.", col))))
    }
  }
  out
}

# Numbers that equal a value held by fewer than min_count persons.
single_values <- function(df, label, cfg, ctx) {
  if (!ncol(df)) return(list())
  vals <- unlist(lapply(df, function(v) if (is.numeric(v)) v else NULL), use.names = FALSE)
  vals <- vals[!is.na(vals) & (abs(vals) >= 100 | vals != round(vals) | vals %in% ctx$sens$small)]
  hits <- unique(vals[rare_number_hits(as.character(vals), ctx$sens)])
  if (!length(hits)) {
    return(list(new_check("single_values", label, "pass", "No number equals a single person's value.")))
  }
  list(new_check("single_values", label, "warn",
                 sprintf("%d number(s) equal a value held by fewer than %d persons (often a median or extreme of a small group).",
                         length(hits), cfg$min_count)))
}

inspect_text <- function(path, label, cfg, ctx, type) {
  lines <- readLines(path, warn = FALSE)
  if (is.null(ctx)) {
    st <- if (type == "log") "pass" else "warn"
    msg <- if (type == "log") "Log cleaned by real_run(); give real_data to search it again." else
      if (type == "description") "Descriptions written by the researcher cannot be searched without the real data; read them for values of persons." else
      "Text cannot be searched without the real data; review by hand."
    return(list(type = type, tables = list(), checks = list(new_check("text", label, st, msg))))
  }
  r <- redact(lines, ctx$sens)
  ids <- unlist(regmatches(lines, gregexpr("[A-Za-z0-9_.-]+", lines)), use.names = FALSE)
  n_id <- length(unique(ids[ids %in% ctx$ids]))
  ch <- if (r$n == 0 && n_id == 0) {
    new_check("text", label, "pass", "No identifiers, free text or rare values found.")
  } else {
    new_check("text", label, "fail",
              sprintf("Found %d value(s) that could point to a person (identifiers, free text or rare values).",
                      r$n + n_id))
  }
  list(type = type, tables = list(), checks = list(ch))
}

# What differencing needs from a table of counts: the count column, the
# grouping columns, and the rows without totals, so margins are not counted
# twice when a finer table is summed up.
differencing_info <- function(df) {
  cnt <- attr(df, "mimic_count") %||% guess_count(df)
  if (is.null(cnt)) return(NULL)
  by <- sort(attr(df, "mimic_by") %||% grouping_columns(df, cnt))
  tot <- if (length(by)) Reduce(`|`, lapply(df[by], is_total)) else rep(FALSE, nrow(df))
  list(df = df[!tot, , drop = FALSE], count = cnt, by = by)
}

# Two tables can be subtracted when they have the same grouping, or one
# grouping is part of the other: "n by sex" and "n by sex and settlement"
# from two populations differ by the persons in between.
compare_tables <- function(a, b, cfg, target) {
  if (!all(a$by %in% b$by) && !all(b$by %in% a$by)) return(NULL)
  check_differencing(a$df, b$df, by = intersect(a$by, b$by), count = NULL,
                     config = cfg, target = target)
}

table_file <- function(nm) sub(" \\[.*\\]$", "", nm)

differencing_pairs <- function(tables, cfg) {
  info <- lapply(tables, differencing_info)
  keep <- names(info)[!vapply(info, is.null, logical(1))]
  out <- list()
  if (length(keep) < 2) return(out)
  for (p in utils::combn(keep, 2, simplify = FALSE)) {
    ch <- compare_tables(info[[p[1]]], info[[p[2]]], cfg, paste(p[1], "vs", p[2]))
    if (!is.null(ch) && ch$status != "pass") {
      out[[length(out) + 1L]] <- list(files = unique(table_file(p)), check = ch)
    }
  }
  out
}

# Differencing against everything released before, from the register kept
# by record_release(). The check goes to the new file only.
register_pairs <- function(tables, releases, cfg) {
  info <- lapply(tables, differencing_info)
  info <- info[!vapply(info, is.null, logical(1))]
  out <- list()
  for (rel in releases) {
    for (old_nm in names(rel$tables)) {
      old <- differencing_info(rel$tables[[old_nm]])
      if (is.null(old)) next
      for (new_nm in names(info)) {
        ch <- compare_tables(info[[new_nm]], old, cfg,
                             sprintf("%s vs %s (released %s)", new_nm, old_nm, rel$created))
        if (!is.null(ch) && ch$status != "pass") {
          ch$message <- paste(ch$message, sprintf("Compared with %s from release %s.", old_nm, rel$id))
          out[[length(out) + 1L]] <- list(files = table_file(new_nm), check = ch)
        }
      }
    }
  }
  out
}

#' @export
print.mimic_inspection <- function(x, ...) {
  f <- x$files
  cli::cli_text("{.strong <mimic_inspection>} {nrow(f)} file{?s} in {.path {x$output_dir}}")
  if (!x$with_real_data) {
    cli::cli_alert_info("Checked without the real data: dominance and text checks are limited.")
  }
  if (is.null(x$register)) {
    cli::cli_alert_info("No register: tables were not compared with earlier releases.")
  } else {
    cli::cli_alert_info("Compared with {x$n_releases} earlier release{?s}.")
  }
  for (i in seq_len(nrow(f))) {
    line <- paste0(f$verdict[i], ": ", f$file[i])
    switch(f$verdict[i],
           "APPROVED" = cli::cli_alert_success(line),
           "NEEDS CHANGES" = cli::cli_alert_danger(line),
           cli::cli_alert_warning(line))
    if (nzchar(f$reasons[i])) {
      cli::cli_ul(cli_escape(strsplit(f$reasons[i], " | ", fixed = TRUE)[[1]]))
    }
  }
  cli::cli_rule()
  cli::cli_text("{sum(f$verdict == 'APPROVED')} approved, {sum(f$verdict == 'NEEDS CHANGES')} need{?s/} changes, {sum(f$verdict == 'REVIEW BY HAND')} to review by hand.")
  invisible(x)
}

#' Write an inspection report for the approver
#'
#' Writes the result of [inspect()] as a Markdown file: a summary table with
#' one verdict per file, followed by the details of every check that did
#' not pass. Save it outside the output directory, so it is not released
#' with the output.
#'
#' @param x A `mimic_inspection` from [inspect()].
#' @param path File to write, for example `"inspection.md"`.
#' @return `path`, invisibly.
#' @export
#' @examples
#' out <- file.path(tempdir(), "report_example")
#' dir.create(out, showWarnings = FALSE)
#' write.csv(data.frame(sex = c("F", "M"), n = c(985, 3)),
#'           file.path(out, "by_sex.csv"), row.names = FALSE)
#' res <- inspect(out)
#' write_report(res, file.path(tempdir(), "inspection.md"))
write_report <- function(x, path) {
  if (!inherits(x, "mimic_inspection")) {
    cli::cli_abort("{.arg x} must be created with {.fn inspect}.")
  }
  if (is_inside(normalizePath(dirname(path), winslash = "/", mustWork = FALSE), x$output_dir)) {
    cli::cli_abort("Write the report outside {.path {x$output_dir}}, so it is not released with the output.")
  }
  f <- x$files
  md_cell <- function(s) gsub("|", "\\|", gsub("\n", " ", s), fixed = TRUE)
  md_table <- function(d) {
    d[] <- lapply(d, function(v) md_cell(as.character(v)))
    c(paste0("| ", paste(names(d), collapse = " | "), " |"),
      paste0("|", paste(rep("---", ncol(d)), collapse = "|"), "|"),
      apply(d, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
  }
  cfg <- x$config
  lines <- c(
    "# Output inspection",
    "",
    sprintf("- Directory: `%s`", x$output_dir),
    sprintf("- Inspected: %s", x$created),
    sprintf("- Rules: min_count %s, dominance (n = %s, k = %s), p %% rule %s",
            cfg$min_count, cfg$dominance$n, cfg$dominance$k, cfg$p_percent),
    sprintf("- Real data used for checking: %s", if (x$with_real_data) "yes" else "no"),
    sprintf("- Earlier releases compared: %s",
            if (is.null(x$register)) "no register" else x$n_releases %||% 0),
    sprintf("- Result: %d approved, %d need changes, %d to review by hand",
            sum(f$verdict == "APPROVED"), sum(f$verdict == "NEEDS CHANGES"), sum(f$verdict == "REVIEW BY HAND")),
    "",
    "## Verdict per file",
    "",
    md_table(f[c("file", "verdict", "reasons")]),
    ""
  )
  # what the researcher says the files show, from describe_output()
  desc <- x$descriptions %||% list()
  if (length(desc)) {
    lines <- c(lines, "## What the files show", "",
               "As described by the researcher with `describe_output()`.", "")
    for (file in names(desc)) {
      d <- desc[[file]]
      extra <- c(if (!is.null(d$count)) sprintf("count: `%s`", d$count),
                 if (!is.null(d$by)) sprintf("grouped by: %s", paste0("`", d$by, "`", collapse = ", ")),
                 if (!is.null(d$data)) sprintf("numbers behind it: `%s`", d$data))
      lines <- c(lines, sprintf("- `%s`: %s%s", file, md_cell(d$note %||% ""),
                                if (length(extra)) paste0(" (", paste(extra, collapse = "; "), ")") else ""))
      if (!is.null(d$changes)) lines <- c(lines, sprintf("  - Changes since last request: %s", md_cell(d$changes)))
    }
    lines <- c(lines, "")
  }
  for (file in f$file[f$verdict != "APPROVED"]) {
    lines <- c(lines, paste0("## ", file), "")
    for (ch in x$checks[[file]]) {
      if (ch$status == "pass") next
      lines <- c(lines, sprintf("- **%s** (%s): %s", toupper(ch$status), ch$rule, md_cell(ch$message)))
      if (!is.null(ch$details) && nrow(ch$details)) {
        lines <- c(lines, "", md_table(utils::head(ch$details, 20)),
                   if (nrow(ch$details) > 20) sprintf("\n... and %d more", nrow(ch$details) - 20), "")
      }
    }
    lines <- c(lines, "")
  }
  lines <- c(lines, "## Approval", "", "Approved by: ______________________   Date: ____________", "")
  writeLines(lines, path)
  invisible(path)
}
