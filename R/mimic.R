#' Build a synthetic copy from a profile
#'
#' `mimic()` builds a synthetic copy of the data from a [profile_data()] object alone, so it runs outside the secure
#' environment with no access to the real data. The copy has the same
#' tables, variable names, types and allowed values, and linked tables share
#' keys. It contains no real persons.
#'
#' @section Fidelity levels:
#' 1. **Structure** (default): correct names, types and allowed values, but
#'    random content: categories uniformly, numbers and dates uniformly
#'    between the protected bounds. Enough to make most scripts run.
#' 2. **Distributions**: every variable follows its coarse distribution from
#'    [profile_data()] (rounded frequencies and histograms), but variables are
#'    independent of each other. Within-row logic such as
#'    `discharge_date >= admit_date` is not preserved.
#' 3. **Relationships**: coarse correlations are kept with a Gaussian
#'    copula. [profile_data()] stores the correlations between the normal scores
#'    of all variables of a table, rounded to one decimal, and for the main
#'    table also with the number of rows each person has in the linked
#'    tables (older persons have more contacts). `mimic()` draws correlated
#'    normal values and maps them through the protected distributions, so
#'    the marginals are exactly those of level 2. Runs without the real
#'    data. Monotone relationships are kept best; for categorical variables
#'    the order of the categories is arbitrary, so their relationships are
#'    only approximate. Variables in linked tables are related to each other,
#'    not to the person's variables.
#' 4. **Model based**: sequential synthesis with classification and
#'    regression trees, the default method of synthpop, implemented here.
#'    Each variable is drawn from donors in the leaf of a tree fitted on the
#'    previous variables of the real data, after the same protection as in
#'    [profile_data()] (top and bottom coding, merged categories). Donors come
#'    from at least `min_count` persons, also in linked tables where a leaf
#'    of many rows can be one person's; drawn numbers and dates are smoothed with
#'    noise, and discrete numbers are drawn within their protected bin.
#'    Linked tables are modelled with the person's variables as predictors.
#'    A table with more than `train_rows` rows is modelled on a random
#'    sample of whole persons, because growing the trees takes time in
#'    proportion to the rows; 100,000 rows show the same structure as
#'    millions of contacts. **Requires `real_data` and must run in the secure environment**; the
#'    copy is output and must be checked before release.
#'
#' Most researchers only need level 1. Higher levels require a deliberate
#' choice; levels 3 and 4 require `i_understand_the_risk = TRUE`, and they
#' always come with a risk report (`attr(x, "mimic")$risk`). With
#' `real_data` the report says how many real rows and unique combinations
#' the copy reproduces.
#'
#' @section Linked tables:
#' Persons are created for the main table first, with new identifiers in
#' the stored format. The number of rows each person gets in a linked table
#' is drawn from the coarse rows-per-person distribution, so some persons
#' have no rows, just like in the real data. At level 1 too, because a
#' script that joins tables must meet persons without contacts.
#'
#' Identifiers are random values in the real format, marked with
#' `id_prefix` so they can never equal a real identifier: the constant
#' first letter of a format like `"P9999999"` is replaced (`"S1234567"`),
#' otherwise the prefix is put in front. With `id_prefix = NULL` the exact
#' format is used; an identifier may then coincide with a real one by
#' chance, but the rest of that row is still synthetic. Numeric identifiers
#' are never prefixed.
#'
#' @section Mess:
#' Real register data break scripts in predictable ways. With `mess = TRUE`
#' the copy gets
#' * missing values in the same proportion as the real data,
#' * invalid or odd codes in categorical variables (`"999"`, `"UNKNOWN"`,
#'   padded or lower-case variants),
#' * extreme values in numeric variables (sentinels such as `999999`,
#'   negative values),
#' * empty strings,
#' * odd dates (`1900-01-01`, `9999-12-31`),
#' * rows in linked tables whose key is not in the main table.
#'
#' Each type can be switched off with [mess_options()]. What was added is
#' logged in `attr(x, "mimic")$mess`.
#'
#' @param profile A `mimic_profile` object from [profile_data()].
#' @param fidelity Level 1 to 4, see below.
#' @param n Number of persons in the main table. Defaults to the (rounded)
#'   number in the real data. Tables without a key are scaled accordingly.
#' @param mess `TRUE` for all types of mess, `FALSE` for a clean copy, or a
#'   [mess_options()] object.
#' @param seed Optional seed for a reproducible copy. The random number
#'   state of the session is restored afterwards.
#' @param id_prefix Marks synthetic identifiers, see Linked tables. `NULL`
#'   keeps the exact format.
#' @param real_data The real data, required for level 4 and optional for
#'   level 3 (only used to check the copy against them). A data frame or a
#'   named list of data frames with the same names as in [profile_data()].
#' @param i_understand_the_risk Must be `TRUE` for levels 3 and 4.
#' @param train_rows Level 4 only: the trees of each table are fitted on at
#'   most this many real rows, sampled as whole persons. `NULL` uses all
#'   rows, which is slower on large registers.
#' @return A data frame if the profile describes one table, otherwise a
#'   named list of data frames. The attribute `"mimic"` records the
#'   level, the seed, the mess that was added and, for levels 3 and 4, the
#'   risk report.
#' @export
#' @examples
#' cfg <- mimic_config(geo_vars = "settlement", text_vars = "note")
#' m <- profile_data(
#'   list(persons = fake_persons, contacts = fake_contacts),
#'   keys = "person_id",
#'   config = cfg
#' )
#' copy <- mimic(m, seed = 1)
#' str(copy$persons)
#'
#' # realistic distributions, no mess
#' clean <- mimic(m, fidelity = 2, mess = FALSE, seed = 1)
#' table(clean$persons$settlement)
#'
#' # only missing values and orphaned rows
#' some <- mimic(m, mess = mess_options(invalid_codes = FALSE, extremes = FALSE,
#'                                      empty_strings = FALSE, odd_dates = FALSE))
#' attr(some, "mimic")$mess
#'
#' # level 3: coarse correlations, still without the real data
#' rel <- mimic(m, fidelity = 3, i_understand_the_risk = TRUE, seed = 1)
#'
#' \donttest{
#' # level 4: in the secure environment only
#' close <- mimic(m, fidelity = 4, i_understand_the_risk = TRUE, seed = 1,
#'                real_data = list(persons = fake_persons, contacts = fake_contacts))
#' attr(close, "mimic")$risk
#' }
mimic <- function(profile, fidelity = 1, n = NULL, mess = TRUE, seed = NULL,
                 id_prefix = "S", real_data = NULL, i_understand_the_risk = FALSE,
                 train_rows = 100000) {
  if (!inherits(profile, "mimic_profile")) {
    cli::cli_abort("{.arg profile} must be created with {.fn profile_data}.")
  }
  if (!is.numeric(fidelity) || length(fidelity) != 1 || !fidelity %in% 1:4) {
    cli::cli_abort("{.arg fidelity} must be 1, 2, 3 or 4.")
  }
  fidelity <- as.integer(fidelity)
  if (!is.null(train_rows) && (!is.numeric(train_rows) || length(train_rows) != 1 ||
                               is.na(train_rows) || train_rows < 1)) {
    cli::cli_abort("{.arg train_rows} must be a positive number of rows, or {.code NULL} for all rows.")
  }
  opts <- as_mess(mess)
  if (!is.null(id_prefix) && (!is.character(id_prefix) || length(id_prefix) != 1 ||
                              !nzchar(id_prefix) || grepl("[A9]", id_prefix))) {
    cli::cli_abort("{.arg id_prefix} must be one non-empty string without {.val A} or {.val 9}, or {.code NULL}.")
  }
  m <- profile
  cfg <- m$config
  risk <- m$risk

  if (fidelity >= 3) {
    if (!isTRUE(i_understand_the_risk)) {
      cli::cli_abort(c("Level {fidelity} keeps relationships between variables and needs a deliberate choice.",
                       "i" = "Set {.code i_understand_the_risk = TRUE} if you have read the risk report.",
                       "i" = "Most scripts run fine on level 1."))
    }
    if (risk$too_small || fidelity > risk$max_level) {
      cli::cli_warn(c("The risk report recommends at most level {risk$max_level} for these data.",
                      "i" = "Consider {.code fidelity = {risk$max_level}}."))
    }
  }
  real <- NULL
  if (!is.null(real_data)) {
    real <- if (is.data.frame(real_data)) stats::setNames(list(real_data), names(m$tables)[1]) else
      as_table_list(real_data)
    missing <- setdiff(names(m$tables), names(real))
    if (length(missing)) cli::cli_abort("{.arg real_data} lacks table{?s} {.val {missing}} from the profile.")
  }
  if (fidelity == 4) {
    if (is.null(real)) {
      cli::cli_abort(c("Level 4 is fitted on the real data; give {.arg real_data}.",
                       "i" = "Run it in the secure environment and check the copy before release."))
    }
    if (!requireNamespace("rpart", quietly = TRUE)) {
      cli::cli_abort("Level 4 needs the {.pkg rpart} package.")
    }
  }
  if (fidelity == 3 && all(vapply(m$tables, function(t) is.null(t$cor), logical(1)))) {
    cli::cli_warn(c("The profile holds no correlations (too few persons); level 3 falls back to level 2."))
    fidelity <- 2L
  }

  if (!is.null(seed)) {
    genv <- globalenv()
    old <- if (exists(".Random.seed", envir = genv, inherits = FALSE)) get(".Random.seed", envir = genv) else NULL
    on.exit({
      if (is.null(old)) {
        if (exists(".Random.seed", envir = genv, inherits = FALSE)) rm(".Random.seed", envir = genv)
      } else {
        assign(".Random.seed", old, envir = genv)
      }
    }, add = TRUE)
    set.seed(seed)
  }

  n_real <- if (is.na(m$n_persons)) cfg$min_count else m$n_persons
  if (is.null(n)) n <- max(n_real, 1)
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1) {
    cli::cli_abort("{.arg n} must be a positive number of persons.")
  }
  n <- as.integer(round(n))
  scale <- n / n_real

  main <- m$keys$main
  tabs <- m$tables
  out <- list()
  notes <- list()
  note <- function(tbl, var, type, rows) {
    notes[[length(notes) + 1L]] <<- data.frame(table = tbl, variable = var, type = type,
                                               rows = rows, stringsAsFactors = FALSE)
  }

  # persons of the main table, or of a single keyed table
  main_ids <- NULL
  if (!is.na(main)) {
    main_ids <- gen_ids(tabs[[main]]$variables[[tabs[[main]]$key]], n, unique = TRUE, prefix = id_prefix)
  }

  # protected real data for level 4
  prot <- NULL
  if (fidelity == 4) {
    prot <- lapply(names(tabs), function(tn) protect_frame(real[[tn]], tabs[[tn]]$variables, cfg))
    names(prot) <- names(tabs)
  }
  child_counts <- list()
  main_syn <- NULL
  main_prot <- NULL

  # the main table first, so linked tables can use its persons and counts
  order <- if (is.na(main)) names(tabs) else c(main, setdiff(names(tabs), main))
  for (tn in order) {
    t <- tabs[[tn]]
    key <- t$key
    key_values <- NULL
    if (!is.na(key) && identical(tn, main)) {
      key_values <- main_ids
    } else if (!is.na(key) && !is.null(t$rows_per_person)) {
      persons <- main_ids %||% gen_ids(t$variables[[key]], n, unique = TRUE, prefix = id_prefix)
      counts <- child_counts[[tn]] %||% gen_counts(t$rows_per_person, length(persons), fidelity)
      if (is.null(main_ids)) counts <- pmax(counts, 1L)
      child_counts[[tn]] <- counts
      key_values <- rep(persons, counts)
    } else if (!is.na(key)) {
      key_values <- gen_ids(t$variables[[key]], n, unique = TRUE, prefix = id_prefix)
    }
    n_rows <- if (!is.null(key_values)) {
      length(key_values)
    } else {
      stored <- if (is.na(t$n_rows)) cfg$min_count else t$n_rows
      if (length(tabs) == 1) n else max(1L, as.integer(round(stored * scale)))
    }

    # columns from the level 3 and 4 engines
    gen <- list()
    if (fidelity == 3 && !is.null(t$cor)) {
      u <- copula_uniforms(t$cor, n_rows)
      for (v in intersect(colnames(u), names(t$variables))) {
        gen[[v]] <- inverse_marginal(t$variables[[v]], u[, v], cfg)
      }
      if (identical(tn, main)) {
        for (rc in grep("^\\.rows_", colnames(u), value = TRUE)) {
          child <- sub("^\\.rows_", "", rc)
          child_counts[[child]] <- as.integer(inv_binned(tabs[[child]]$rows_per_person, u[, rc], TRUE))
        }
      }
    }
    if (fidelity == 4) {
      rp <- prot[[tn]]
      specs <- t$variables
      if (identical(tn, main)) {
        mk <- as.character(real[[main]][[key]])
        for (child in setdiff(names(tabs), main)) {
          rpp <- tabs[[child]]$rows_per_person
          if (is.null(rpp) || isTRUE(rpp$suppressed)) next
          cnt <- tabulate(match(as.character(real[[child]][[tabs[[child]]$key]]), mk), length(mk))
          rc <- paste0(".rows_", child)
          rp[[rc]] <- pmin(pmax(cnt, rpp$lo), rpp$hi)
          specs[[rc]] <- rpp
        }
        syn <- cart_table(rp, specs, n_rows, cfg, train_rows = train_rows)
        for (rc in grep("^\\.rows_", names(syn), value = TRUE)) {
          child_counts[[sub("^\\.rows_", "", rc)]] <- as.integer(syn[[rc]])
        }
        main_syn <- syn[setdiff(names(syn), grep("^\\.rows_", names(syn), value = TRUE))]
        main_prot <- rp[names(main_syn)]
      } else if (!is.null(main_syn) && !is.na(key) && ncol(main_syn)) {
        # the person's variables are predictors for the linked table
        given <- take_rows(main_syn, rep(seq_len(n), child_counts[[tn]]))
        rk <- match(as.character(real[[tn]][[key]]), as.character(real[[main]][[tabs[[main]]$key]]))
        given_real <- take_rows(main_prot, rk)
        names(given) <- names(given_real) <- paste0(".person_", names(main_syn))
        syn <- cart_table(rp, specs, n_rows, cfg, given, given_real,
                          pid = as.character(real[[tn]][[key]]), train_rows = train_rows)
        syn <- syn[setdiff(names(syn), names(given))]
      } else {
        # a keyed table other than the main one has several rows per person
        pid <- if (!is.na(key) && !identical(tn, main)) as.character(real[[tn]][[key]]) else NULL
        syn <- cart_table(rp, specs, n_rows, cfg, pid = pid, train_rows = train_rows)
      }
      for (v in intersect(names(syn), names(t$variables))) {
        gen[[v]] <- from_model_scale(syn[[v]], t$variables[[v]])
      }
    }

    cols <- list()
    for (v in names(t$variables)) {
      spec <- t$variables[[v]]
      cols[[v]] <- if (!is.na(key) && identical(v, key)) {
        restore_class(key_values, spec)
      } else if (!is.null(gen[[v]])) {
        gen[[v]]
      } else {
        val <- gen_variable(spec, n_rows, min(fidelity, 2L), cfg, id_prefix)
        if (isTRUE(spec$suppressed)) note(tn, v, "placeholder values (suppressed in profile_data)", n_rows)
        val
      }
    }
    df <- as.data.frame(cols, stringsAsFactors = FALSE, optional = TRUE)
    if (!length(cols)) df <- data.frame(row.names = seq_len(n_rows))
    names(df) <- names(t$variables)
    out[[tn]] <- df
  }
  out <- out[names(tabs)]

  copy_risk_report <- NULL
  if (fidelity >= 3) {
    copy_risk_report <- copy_risk(out, real, m, fidelity)
    print(copy_risk_report)
  }

  out <- apply_mess(out, m, opts, main_ids, note, id_prefix)

  log <- if (length(notes)) do.call(rbind, notes) else
    data.frame(table = character(), variable = character(), type = character(), rows = integer())
  id_columns <- lapply(tabs, function(t) {
    names(t$variables)[vapply(t$variables, function(s) identical(s$type, "id"), logical(1))]
  })
  info <- list(fidelity = fidelity, n = n, seed = seed, mess = log, id_columns = id_columns,
               risk = copy_risk_report,
               version = as.character(utils::packageVersion("mimic")))
  if (length(out) == 1) {
    res <- out[[1]]
  } else {
    res <- out
  }
  attr(res, "mimic") <- info
  res
}

#' Choose which kinds of mess to add
#'
#' @param missing Missing values in the same proportion as the real data.
#' @param invalid_codes Invalid or odd codes in categorical variables.
#' @param extremes Extreme values in numeric variables.
#' @param empty_strings Empty and blank strings in character variables.
#' @param odd_dates Implausible dates.
#' @param orphans Rows in linked tables whose key is not in the main table.
#' @param rate Share of rows affected per variable by each kind of mess
#'   other than missing values. At least one row is always affected, so the
#'   script meets the problem.
#' @return An object of class `mimic_mess` for [mimic()].
#' @export
#' @examples
#' mess_options(orphans = FALSE, rate = 0.05)
mess_options <- function(missing = TRUE, invalid_codes = TRUE, extremes = TRUE,
                         empty_strings = TRUE, odd_dates = TRUE, orphans = TRUE,
                         rate = 0.01) {
  flags <- list(missing = missing, invalid_codes = invalid_codes, extremes = extremes,
                empty_strings = empty_strings, odd_dates = odd_dates, orphans = orphans)
  for (nm in names(flags)) {
    if (!is.logical(flags[[nm]]) || length(flags[[nm]]) != 1 || is.na(flags[[nm]])) {
      cli::cli_abort("{.arg {nm}} must be TRUE or FALSE.")
    }
  }
  assert_number(rate, "rate", min = 0, max = 0.5)
  structure(c(flags, rate = rate), class = "mimic_mess")
}

#' @export
print.mimic_mess <- function(x, ...) {
  on <- names(x)[vapply(x, isTRUE, logical(1))]
  cli::cli_text("{.strong <mimic_mess>} {if (length(on)) paste(on, collapse = ', ') else 'none'}; rate {x$rate}")
  invisible(x)
}

as_mess <- function(mess) {
  if (inherits(mess, "mimic_mess")) return(mess)
  if (isTRUE(mess)) return(mess_options())
  if (isFALSE(mess)) {
    return(mess_options(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  }
  cli::cli_abort("{.arg mess} must be TRUE, FALSE or created with {.fn mess_options}.")
}

# --- generators ----------------------------------------------------------------

gen_variable <- function(spec, n, fidelity, cfg, prefix = NULL) {
  switch(spec$type,
         id = gen_ids(spec, n, unique = isTRUE(spec$unique), prefix = prefix),
         text = gen_text(n, spec$approx_length),
         categorical = gen_categorical(spec, n, fidelity),
         logical = gen_logical(spec, n, fidelity, cfg),
         numeric = restore_class(gen_numeric(spec, n, fidelity), spec),
         date = gen_date(spec, n, fidelity),
         rep(NA, n))
}

# Fill a template: 9 becomes a random digit, A a random capital letter.
fill_template <- function(tmpl, k) {
  chars <- strsplit(tmpl, "")[[1]]
  if (!length(chars) || k == 0) return(rep(tmpl, k))
  mat <- matrix(rep(chars, each = k), nrow = k)
  for (j in seq_along(chars)) {
    if (chars[j] == "9") mat[, j] <- sample(0:9, k, replace = TRUE)
    if (chars[j] == "A") mat[, j] <- sample(LETTERS, k, replace = TRUE)
  }
  do.call(paste0, lapply(seq_along(chars), function(j) mat[, j]))
}

gen_ids <- function(spec, n, unique = TRUE, avoid = character(), prefix = NULL) {
  fmts <- spec$formats
  if (is.null(fmts) || !nrow(fmts)) fmts <- data.frame(format = "ID99999999", share = 1)
  if (!is.null(prefix) && !identical(spec$class, "numeric") && !identical(spec$class, "integer")) {
    fmts$format <- vapply(fmts$format, apply_prefix, character(1), prefix = prefix, USE.NAMES = FALSE)
  }
  pick <- sample(seq_len(nrow(fmts)), n, replace = TRUE, prob = pmax(fmts$share, 1e-6))
  out <- character(n)
  for (i in seq_len(nrow(fmts))) {
    sel <- which(pick == i)
    out[sel] <- fill_template(fmts$format[i], length(sel))
  }
  if (unique || length(avoid)) {
    for (attempt in 1:50) {
      bad <- duplicated(out) | out %in% avoid
      if (!any(bad)) break
      out[bad] <- fill_template(fmts$format[pick[which(bad)[1]]], sum(bad))
    }
    bad <- duplicated(out) | out %in% avoid
    if (any(bad)) out[bad] <- paste0(out[bad], "_", seq_len(sum(bad)))
  }
  restore_class(out, spec)
}

# Mark a template as synthetic. A literal first character (kept because all
# real ids share it, like the "P" in "P9999999") is replaced, so the length
# stays the same and no synthetic id can equal a real one. Otherwise the
# prefix is put in front.
apply_prefix <- function(tmpl, prefix) {
  first <- substr(tmpl, 1, 1)
  if (nchar(tmpl) > 0 && !first %in% c("9", "A", prefix) && nchar(prefix) == 1) {
    paste0(prefix, substr(tmpl, 2, nchar(tmpl)))
  } else {
    paste0(prefix, tmpl)
  }
}

lorem <- c("lorem", "ipsum", "dolor", "sit", "amet", "consectetur", "adipiscing",
           "elit", "sed", "do", "eiusmod", "tempor", "incididunt", "ut", "labore",
           "et", "dolore", "magna", "aliqua", "enim", "minim", "veniam")

gen_text <- function(n, approx_length) {
  len <- if (is.null(approx_length) || is.na(approx_length)) 30 else approx_length
  if (n == 0) return(character())
  target <- pmax(1, round(len * stats::runif(n, 0.5, 1.5)))
  # cut every text from one long random text, starting at a word: one
  # paste() instead of one per row
  pool <- paste(sample(lorem, ceiling(max(target) / 2) + 200, replace = TRUE), collapse = " ")
  starts <- c(1L, as.integer(gregexpr(" ", pool, fixed = TRUE)[[1]]) + 1L)
  starts <- starts[starts + max(target) - 1L <= nchar(pool)]
  from <- starts[sample.int(length(starts), n, replace = TRUE)]
  substring(pool, from, from + target - 1L)
}

gen_categorical <- function(spec, n, fidelity) {
  lv <- spec$levels
  prob <- spec$freq
  if (fidelity < 2 || anyNA(prob) || sum(prob) == 0) prob <- rep(1, length(lv))
  x <- lv[sample.int(length(lv), n, replace = TRUE, prob = prob)]
  restore_class(x, spec)
}

gen_logical <- function(spec, n, fidelity, cfg) {
  if (fidelity < 2) return(sample(c(FALSE, TRUE), n, replace = TRUE))
  f <- spec$freq
  total <- sum(f, na.rm = TRUE) + sum(is.na(f)) * cfg$min_count
  f[is.na(f)] <- cfg$min_count
  p_true <- if (total > 0) f[2] / sum(f) else 0.5
  stats::runif(n) < p_true
}

# Draw values from the stored bins (level 2) or uniformly between the
# bounds (level 1). Shared by numeric variables, dates and row counts.
draw_binned <- function(spec, n, fidelity, discrete) {
  lo <- as.numeric(spec$lo)
  hi <- as.numeric(spec$hi)
  if (fidelity < 2) {
    from <- rep(lo, n)
    to <- rep(hi, n)
  } else {
    d <- spec$dist
    f <- d$freq
    f[is.na(f)] <- 0
    if (sum(f) == 0) f <- rep(1, nrow(d))
    b <- sample.int(nrow(d), n, replace = TRUE, prob = f)
    from <- pmax(as.numeric(d$from)[b], lo)
    to <- pmin(as.numeric(d$to)[b], hi)
    to <- pmax(to, from)
  }
  if (discrete) {
    from + floor(stats::runif(n) * (to - from + 1))
  } else {
    stats::runif(n, from, to)
  }
}

gen_numeric <- function(spec, n, fidelity) {
  if (isTRUE(spec$suppressed)) return(round(stats::runif(n, 0, 100)))
  x <- draw_binned(spec, n, fidelity, discrete = isTRUE(spec$discrete))
  if (isTRUE(spec$integer)) x <- round(x)
  x
}

gen_counts <- function(spec, n, fidelity) {
  if (isTRUE(spec$suppressed)) return(stats::rpois(n, 1))
  as.integer(draw_binned(spec, n, fidelity, discrete = TRUE))
}

gen_date <- function(spec, n, fidelity) {
  days <- if (isTRUE(spec$suppressed)) {
    stats::runif(n, as.numeric(as.Date("2000-01-01")), as.numeric(as.Date("2020-12-31")))
  } else {
    draw_binned(spec, n, fidelity, discrete = TRUE)
  }
  restore_class(as.Date(floor(days), origin = "1970-01-01"), spec)
}

# Give a generated vector the class of the original variable.
restore_class <- function(x, spec) {
  cls <- spec$class %||% "character"
  switch(cls,
         factor = , ordered = factor(as.character(x), levels = spec$levels %||% unique(x),
                                     ordered = isTRUE(spec$ordered)),
         integer = {
           v <- suppressWarnings(as.integer(x))
           if (anyNA(v[!is.na(x)])) as.character(x) else v
         },
         numeric = , double = {
           v <- suppressWarnings(as.numeric(x))
           if (anyNA(v[!is.na(x)])) as.character(x) else v
         },
         Date = as.Date(x),
         POSIXct = as.POSIXct(as.Date(x)),
         logical = as.logical(x),
         as.character(x))
}

# --- mess --------------------------------------------------------------------

n_mess <- function(n_rows, rate) {
  if (n_rows == 0) return(0L)
  as.integer(min(n_rows, max(1, round(rate * n_rows))))
}

odd_codes <- function(levels) {
  base <- if (length(levels)) levels[1] else "x"
  unique(c("999", "UNKNOWN", "?", tolower(base), paste0(" ", base), paste0(base, " ")))
}

odd_dates <- as.Date(c("1900-01-01", "1970-01-01", "2099-12-31", "9999-12-31"))

apply_mess <- function(out, m, opts, main_ids, note, prefix = NULL) {
  for (tn in names(out)) {
    df <- out[[tn]]
    t <- m$tables[[tn]]
    nr <- nrow(df)
    if (!nr) next
    for (v in names(df)) {
      spec <- t$variables[[v]]
      is_key <- !is.na(t$key) && identical(v, t$key)
      x <- df[[v]]

      if (opts$invalid_codes && spec$type == "categorical") {
        k <- n_mess(nr, opts$rate)
        idx <- sample.int(nr, k)
        codes <- odd_codes(spec$levels)
        new <- sample(codes, k, replace = TRUE)
        if (is.factor(x)) {
          levels(x) <- union(levels(x), codes)
          x[idx] <- new
        } else if (is.character(x)) {
          x[idx] <- new
        }
        note(tn, v, "invalid codes", k)
      }
      if (opts$extremes && spec$type == "numeric" && !isTRUE(spec$suppressed) && is.numeric(x)) {
        k <- n_mess(nr, opts$rate)
        hi <- max(abs(spec$hi), 1)
        vals <- c(999999, -1, -hi, hi * 10, 0)
        x[sample.int(nr, k)] <- sample(vals, k, replace = TRUE)
        if (is.integer(df[[v]])) x <- as.integer(x)
        note(tn, v, "extreme values", k)
      }
      if (opts$empty_strings && is.character(x) && !is_key && spec$type %in% c("categorical", "text")) {
        k <- n_mess(nr, opts$rate)
        x[sample.int(nr, k)] <- sample(c("", " "), k, replace = TRUE)
        note(tn, v, "empty strings", k)
      }
      if (opts$odd_dates && spec$type == "date") {
        k <- n_mess(nr, opts$rate)
        new <- sample(odd_dates, k, replace = TRUE)
        x[sample.int(nr, k)] <- if (inherits(x, "POSIXct")) as.POSIXct(new) else new
        note(tn, v, "odd dates", k)
      }
      # missing last, so the share of missing values matches the real data
      if (opts$missing && !is.null(spec$missing) && spec$missing > 0) {
        k <- as.integer(min(nr, round(spec$missing * nr)))
        if (k > 0) {
          x[sample.int(nr, k)] <- NA
          note(tn, v, "missing values", k)
        }
      }
      df[[v]] <- x
    }

    if (opts$orphans && !is.null(main_ids) && !is.na(t$key) && !identical(tn, m$keys$main)) {
      share <- max(t$orphan_share %||% 0, opts$rate)
      k <- n_mess(nr, share)
      from <- sample.int(nr, k, replace = TRUE)
      key_spec <- t$variables[[t$key]]
      new_keys <- gen_ids(key_spec, k, unique = FALSE, avoid = as.character(main_ids), prefix = prefix)
      shuffle <- sample.int(nr + k)
      # the added rows and the shuffle column by column: rbind() and row
      # subsetting of a data frame are slow on many rows. Only the keys are
      # joined with rbind(), which settles their class when they differ.
      keys <- rbind(df[t$key], stats::setNames(data.frame(new_keys, stringsAsFactors = FALSE), t$key))[[1]]
      pick <- c(seq_len(nr), from)[shuffle]
      cols <- lapply(df, function(x) x[pick])
      cols[[t$key]] <- keys[shuffle]
      df <- stats::setNames(as.data.frame(cols, stringsAsFactors = FALSE, optional = TRUE), names(df))
      note(tn, t$key, "rows with keys not in the main table", k)
    }
    rownames(df) <- NULL
    out[[tn]] <- df
  }
  out
}
