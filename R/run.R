# Shared machinery for trial_run() and real_run(): run a script in a fresh R
# session with the data injected, record what it writes, and report where
# it failed.

# Writers that are traced. Base packages are traced at start; the others
# when their namespace is loaded, so `pkg::fun()` and `library(pkg)` both
# work.
write_functions <- list(
  base = c("saveRDS", "save", "writeLines", "write", "cat", "sink", "writeBin", "dput", "dump"),
  utils = c("write.table", "write.csv", "write.csv2"),
  grDevices = c("pdf", "png", "jpeg", "svg", "tiff", "bmp", "cairo_pdf")
)
write_functions_later <- list(
  data.table = "fwrite",
  readr = c("write_csv", "write_csv2", "write_tsv", "write_delim", "write_rds", "write_excel_csv", "write_lines"),
  writexl = "write_xlsx",
  openxlsx = c("write.xlsx", "saveWorkbook"),
  openxlsx2 = c("write_xlsx", "wb_save"),
  haven = c("write_dta", "write_sav", "write_sas", "write_xpt"),
  arrow = c("write_parquet", "write_feather", "write_csv_arrow"),
  ggplot2 = "ggsave",
  qs = "qsave",
  qs2 = "qs_save",
  fst = "write_fst",
  vroom = "vroom_write",
  jsonlite = "write_json",
  feather = "write_feather"
)

# Runs in the child process. Must be self-contained: callr sends it without
# its enclosing environment, so only base R and `pkg::` calls are allowed.
script_runner <- function(script, data, output_dir, guard, writers, writers_later, config = NULL) {
  # safe_count() and friends follow the rules staff gave trial_run()/real_run()
  options(mimic.config = config)
  rec <- new.env()
  rec$writes <- list()
  rec$warnings <- character()

  lexical <- function(p) {
    p <- gsub("\\\\", "/", path.expand(p))
    if (!grepl("^([A-Za-z]:)?/", p)) p <- paste0(gsub("\\\\", "/", getwd()), "/", p)
    parts <- strsplit(p, "/", fixed = TRUE)[[1]]
    out <- character()
    for (s in parts) {
      if (s == "." || (s == "" && length(out))) next
      if (s == "..") {
        if (length(out) > 1) out <- out[-length(out)]
        next
      }
      out <- c(out, s)
    }
    paste(out, collapse = "/")
  }
  out_norm <- lexical(normalizePath(output_dir, winslash = "/", mustWork = TRUE))
  windows <- .Platform$OS.type == "windows"
  inside <- function(p) {
    a <- lexical(p)
    o <- out_norm
    if (windows) {
      a <- tolower(a)
      o <- tolower(o)
    }
    a == o || startsWith(a, paste0(o, "/"))
  }

  record <- function(fun, path, d) {
    if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) return(invisible())
    ok <- inside(path)
    rec$writes[[length(rec$writes) + 1L]] <- list(
      fun = fun, path = path, inside = ok,
      rows = if (length(d) >= 1) d[1] else NA_integer_,
      cols = if (length(d) >= 2) d[2] else NA_integer_
    )
    if (guard && !ok) {
      stop(sprintf("mimic: writing outside output_dir is blocked (%s)", path), call. = FALSE)
    }
    invisible()
  }
  options(mimic.record = record)

  tracer <- function(fun) {
    bquote({
      .mq_p <- NULL
      .mq_d <- NULL
      for (.mq_a in c("file", "path", "filename", "con", "sink", "description")) {
        if (exists(.mq_a, inherits = FALSE)) {
          .mq_v <- tryCatch(get(.mq_a, inherits = FALSE), error = function(e) NULL)
          if (is.character(.mq_v)) {
            .mq_p <- .mq_v
            break
          }
        }
      }
      for (.mq_a in c("x", "object", "obj", "data", "df", "value")) {
        if (exists(.mq_a, inherits = FALSE)) {
          .mq_v <- tryCatch(get(.mq_a, inherits = FALSE), error = function(e) NULL)
          if (!is.null(dim(.mq_v))) {
            .mq_d <- dim(.mq_v)
            break
          }
        }
      }
      getOption("mimic.record")(.(fun), .mq_p, .mq_d)
    })
  }
  trace_now <- function(fun, where = NULL) {
    invisible(suppressMessages(tryCatch({
      if (is.null(where)) {
        trace(fun, tracer = tracer(fun), print = FALSE)
      } else {
        trace(fun, tracer = tracer(fun), where = where, print = FALSE)
      }
    }, error = function(e) NULL)))
  }
  for (pkg in names(writers)) for (f in writers[[pkg]]) trace_now(f)
  for (pkg in names(writers_later)) {
    fs <- writers_later[[pkg]]
    if (isNamespaceLoaded(pkg)) {
      for (f in fs) trace_now(f, asNamespace(pkg))
    } else {
      setHook(packageEvent(pkg, "onLoad"), local({
        fs <- fs
        pkg <- pkg
        function(...) for (f in fs) trace_now(f, asNamespace(pkg))
      }))
    }
  }

  setwd(output_dir)
  # This runs in the child session only: scripts expect their data and
  # output_dir in the global environment, as in an ordinary R session.
  list2env(c(data, list(output_dir = out_norm)), envir = globalenv())

  script_norm <- normalizePath(script, winslash = "/", mustWork = TRUE)
  line <- NA_integer_
  err <- NULL
  t0 <- Sys.time()
  tryCatch(
    withCallingHandlers(
      source(script, local = globalenv(), echo = TRUE, max.deparse.length = Inf,
             keep.source = TRUE, spaced = FALSE),
      warning = function(w) rec$warnings <- c(rec$warnings, conditionMessage(w)),
      error = function(e) {
        for (cl in rev(sys.calls())) {
          sr <- attr(cl, "srcref")
          if (!is.null(sr)) {
            sf <- attr(sr, "srcfile")
            fn <- tryCatch(normalizePath(sf$filename, winslash = "/", mustWork = FALSE),
                           error = function(e) "")
            if (identical(fn, script_norm)) {
              line <<- as.integer(sr[1])
              break
            }
          }
        }
      }
    ),
    error = function(e) err <<- e
  )
  seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  error <- NULL
  if (!is.null(err)) {
    cl <- conditionCall(err)
    fun <- NA_character_
    if (is.call(cl)) {
      head <- cl[[1]]
      if (is.symbol(head) || (is.call(head) && as.character(head[[1]]) %in% c("::", ":::"))) {
        fun <- paste(deparse(head), collapse = "")
      }
      # evaluation machinery says nothing about where the script went wrong
      if (fun %in% c("eval", "evalq", "eval.parent", "doTryCatch", "withCallingHandlers",
                     "source", "withVisible", "do.call", "FUN", "lapply", "vapply", "sapply")) {
        fun <- NA_character_
      }
    }
    error <- list(message = conditionMessage(err), class = class(err), fun = fun, line = line)
  }
  list(error = error, writes = rec$writes, warnings = unique(rec$warnings), seconds = seconds)
}

# Accept a path to an R script, or code as a character vector.
as_script <- function(script) {
  if (is.character(script) && length(script) == 1 && file.exists(script)) {
    return(list(path = normalizePath(script, winslash = "/"), temp = FALSE))
  }
  # "C:/x (1)/analysis.R" that does not exist is a wrong path, not code
  if (is.character(script) && length(script) == 1 && !grepl("\n", script) &&
      grepl("\\.[Rr]$", script)) {
    cli::cli_abort("Script {.file {script}} not found.")
  }
  if (is.character(script) && (length(script) > 1 || grepl("\n|\\(|<-", script))) {
    path <- tempfile("script_", fileext = ".R")
    writeLines(script, path)
    return(list(path = normalizePath(path, winslash = "/"), temp = TRUE))
  }
  cli::cli_abort("{.arg script} must be the path to an R script, or R code as text.")
}

# Data are injected under their table names; a single data frame as `data`.
as_script_data <- function(data, arg) {
  if (is.data.frame(data)) return(list(data = data))
  if (is.list(data) && length(data) && !is.null(names(data)) && all(nzchar(names(data))) &&
      all(vapply(data, is.data.frame, logical(1)))) {
    return(data)
  }
  cli::cli_abort("{.arg {arg}} must be a data frame or a named list of data frames.")
}

snapshot <- function(dir) {
  f <- list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  if (!length(f)) return(data.frame(file = character(), size = numeric(), mtime = numeric()))
  info <- file.info(file.path(dir, f))
  data.frame(file = f, size = info$size, mtime = as.numeric(info$mtime), stringsAsFactors = FALSE)
}

changed_files <- function(before, after) {
  key_b <- paste(before$file, before$size, before$mtime)
  after[!paste(after$file, after$size, after$mtime) %in% key_b, , drop = FALSE]
}

# `meanwhile` is called while the script runs (real_run() builds the deny
# list then); its value is returned as `res$meanwhile`.
run_script <- function(script, data, output_dir, guard, timeout, log_path, meanwhile = NULL,
                       config = NULL) {
  before <- snapshot(output_dir)
  t0 <- Sys.time()
  p <- tryCatch(
    callr::r_bg(script_runner,
                args = list(script = script, data = data, output_dir = output_dir, guard = guard,
                            writers = write_functions, writers_later = write_functions_later,
                            config = config),
                stdout = log_path, stderr = "2>&1",
                user_profile = FALSE, system_profile = FALSE,
                # English messages, so error reports and cleaning behave the same everywhere
                env = c(callr::rcmd_safe_env(), LANGUAGE = "en")),
    error = function(e) NULL)
  # the child must not outlive this call, also when meanwhile() fails
  if (!is.null(p)) on.exit(if (p$is_alive()) p$kill(), add = TRUE)
  extra <- if (is.null(meanwhile)) NULL else meanwhile()
  res <- if (is.null(p)) {
    list(crash = TRUE)
  } else {
    left <- timeout - as.numeric(difftime(Sys.time(), t0, units = "secs"))
    p$wait(max(0, left) * 1000)
    if (p$is_alive()) {
      p$kill()
      list(timeout = TRUE)
    } else {
      tryCatch(p$get_result(), error = function(e) list(crash = TRUE))
    }
  }
  res$files <- changed_files(before, snapshot(output_dir))
  res$meanwhile <- extra
  res
}

# Packages a script uses: library(), require(), requireNamespace(),
# pacman::p_load() and pkg:: calls.
script_packages <- function(path) {
  exprs <- tryCatch(parse(path, keep.source = TRUE), error = function(e) NULL)
  if (is.null(exprs)) return(character())
  pd <- utils::getParseData(exprs)
  pkgs <- pd$text[pd$token == "SYMBOL_PACKAGE"]
  calls <- which(pd$token == "SYMBOL_FUNCTION_CALL" &
                   pd$text %in% c("library", "require", "requireNamespace", "p_load", "loadNamespace"))
  for (i in calls) {
    # the first argument after the call's opening parenthesis
    after <- pd[pd$line1 >= pd$line1[i] & seq_len(nrow(pd)) > i, , drop = FALSE]
    after <- after[after$terminal & !after$token %in% c("'('", "SYMBOL_FUNCTION_CALL"), , drop = FALSE]
    if (nrow(after)) {
      tok <- after$text[1]
      if (after$token[1] %in% c("SYMBOL", "STR_CONST")) pkgs <- c(pkgs, gsub("^[\"']|[\"']$", "", tok))
    }
  }
  sort(unique(setdiff(pkgs, c("base", ""))))
}

# Symbols and strings in a script: the researcher's own words, safe to show.
script_words <- function(path) {
  exprs <- tryCatch(parse(path, keep.source = TRUE), error = function(e) NULL)
  if (is.null(exprs)) return(character())
  pd <- utils::getParseData(exprs)
  w <- pd$text[pd$token %in% c("SYMBOL", "SYMBOL_SUB", "SYMBOL_FUNCTION_CALL", "STR_CONST", "SYMBOL_PACKAGE")]
  unique(gsub("^[\"'`]|[\"'`]$", "", w))
}

writes_df <- function(writes) {
  if (!length(writes)) {
    return(data.frame(fun = character(), path = character(), inside = logical(),
                      rows = integer(), cols = integer()))
  }
  df <- do.call(rbind, lapply(writes, function(w) {
    data.frame(fun = w$fun, path = w$path, inside = w$inside,
               rows = as.integer(w$rows), cols = as.integer(w$cols), stringsAsFactors = FALSE)
  }))
  # write.csv calls write.table: keep one line per path, the outermost writer
  df[!duplicated(df$path), , drop = FALSE]
}
