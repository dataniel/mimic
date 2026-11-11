#' Describe a dataset safely
#'
#' `profile_data()` records what is needed to make a convincing copy and
#' nothing that identifies a person. It describes one
#' table or a set of linked tables and applies the protection rules
#' **before** anything is stored, so the result can be saved with
#' [saveRDS()] and taken out of the secure environment. [mimic()] builds the
#' synthetic copy from it.
#'
#' @section What is stored:
#' For every variable: name, type, share missing, and depending on type
#' * **categorical** (character, factor): allowed values and rounded
#'   frequencies. Categories with fewer than `min_count` persons are merged
#'   into `other_label`; if the merged group itself is too small, the next
#'   smallest category is merged too. Factor levels that no one holds are
#'   dropped: they are labels of fewer than `min_count` persons as well, and
#'   may come from a list outside the data.
#' * **geographical** (`geo_vars`): as categorical, flagged so that small
#'   units (settlements) are reported.
#' * **numeric**: bottom and top coded bounds, a coarse histogram whose
#'   bins each hold at least `min_count` persons, and rounded mean, median
#'   and standard deviation computed after top and bottom coding. Integer
#'   variables with few values keep their values as bins.
#' * **dates**: as numeric, with bounds coarsened to `date_unit` and bins
#'   by year.
#' * **logical**: rounded frequencies; a value held by fewer than
#'   `min_count` persons is suppressed.
#' * **identifiers** (keys, `id_vars`, and columns that look like ids): the
#'   format only, such as `"A9999999"`. Letters or punctuation shared by
#'   every value are kept only when at least `min_persons` values share them;
#'   digits never are.
#' * **free text** (`text_vars`, and long character columns that are mostly
#'   unique): only the approximate length.
#' * **names and addresses**: a categorical variable with at least 20
#'   categories, more than half of them held by fewer than `min_count`
#'   persons, is stored as an identifier. Its few common values are
#'   households or blocks of flats, which is not a category to publish. To
#'   keep a code like this as categories, coarsen it first (the first
#'   characters of a code, a district instead of an address). This applies
#'   to `geo_vars` too: a geographical unit where most units hold fewer than
#'   `min_count` persons is an address, not a place.
#'
#' For linked tables it also stores the distribution of rows per person
#' (top coded), the share of persons with no rows, and the share of rows
#' that refer to unknown persons.
#'
#' For level 3 of [mimic()] each table also gets a correlation matrix of
#' the normal scores of its variables on the protected scale, with the rows
#' per person in linked tables added for the main table. It is rounded to one
#' decimal and only stored with at least `min_persons` persons. Correlation
#' coefficients belong to the "safe" output classes in the Eurostat
#' guidelines for output checking.
#'
#' All counting is done in **persons**, not rows: a category held by one
#' person with twenty contacts is still a category of one person.
#'
#' @section Why bounds are not the 1 % quantile:
#' With 40 rows, the 1 % quantile is one person's value. `profile_data()`
#' therefore places each bound where at least
#' `max(tail_coding * persons, min_count)` persons lie at or beyond it.
#'
#' @section What is never stored:
#' Rows, identifier values, free text, exact minima and maxima, and labels
#' of categories held by fewer than `min_count` persons. Counts below
#' `min_count` are suppressed and other counts rounded to `round_base`.
#'
#' @param tables A data frame, or a named list of linked data frames.
#' @param keys Key linking the tables: one column name used in every table,
#'   or a named character vector mapping table names to key columns. Persons
#'   are counted on this key.
#' @param config A [mimic_config()]; `NULL` uses the defaults.
#' @param main Name of the table with one row per person. By default the
#'   first table whose key is unique.
#' @return An object of class `mimic_profile` with elements `tables`
#'   (variable descriptions per table), `keys`, `n_persons` (rounded),
#'   `config` and `risk` (a risk report, see Details).
#'
#' @details
#' The risk report (`$risk`) lists every variable that protection changed
#' and how, checks combinations of quasi-identifiers with
#' [check_k_anonymity()] (totals only), checks the keys with [check_keys()],
#' and says whether the population is too small for synthesis above level 2
#' (`min_persons`). Variables that were classified as identifiers or free
#' text automatically are reported with a warning, so a wrong guess is
#' noticed.
#'
#' @export
#' @examples
#' cfg <- mimic_config(
#'   quasi_ids = c("birth_date", "sex", "settlement"),
#'   geo_vars = "settlement",
#'   text_vars = "note"
#' )
#' m <- profile_data(
#'   list(persons = fake_persons, contacts = fake_contacts,
#'        admissions = fake_admissions),
#'   keys = "person_id",
#'   config = cfg
#' )
#' m
#' m$tables$persons$variables$settlement
profile_data <- function(tables, keys = NULL, config = NULL, main = NULL) {
  cfg <- as_config(config)
  tables <- as_table_list(tables, label_of(substitute(tables)))
  k <- resolve_keys(tables, keys, main)

  named <- unique(c(cfg$quasi_ids, cfg$geo_vars, cfg$id_vars, cfg$text_vars))
  all_cols <- unique(unlist(lapply(tables, names)))
  unknown <- setdiff(named, all_cols)
  if (length(unknown)) {
    cli::cli_warn(c("Variable{?s} named in the configuration not found in any table: {.field {unknown}}.",
                    "i" = "Check the spelling; they get no special protection."))
  }

  main_keys <- if (!is.na(k$main)) unique(as.character(tables[[k$main]][[k$key[[k$main]]]])) else NULL
  keyed <- names(k$key)[!is.na(k$key)]
  n_persons <- if (!is.null(main_keys)) {
    length(main_keys)
  } else if (length(keyed)) {
    length(unique(unlist(lapply(keyed, function(t) as.character(tables[[t]][[k$key[[t]]]])))))
  } else {
    nrow(tables[[1]])
  }

  auto <- character()
  modified <- list()
  out <- list()
  child_counts <- list()

  for (tn in names(tables)) {
    df <- tables[[tn]]
    key <- k$key[[tn]]
    id <- if (is.na(key)) seq_len(nrow(df)) else person_ids(df[[key]])

    vars <- list()
    for (v in names(df)) {
      role <- variable_role(v, df[[v]], key, cfg)
      if (role$auto) auto <- c(auto, sprintf("%s$%s (%s)", tn, v, role$role))
      spec <- describe_variable(df[[v]], id, role$role, cfg, geo = v %in% cfg$geo_vars)
      # names and addresses look like categories, but most of them are held
      # by fewer than min_count persons, and the rest are households
      if (spec$type == "categorical" && mostly_rare(spec$n_categories, spec$n_rare)) {
        role <- list(role = "id", auto = TRUE)
        auto <- c(auto, sprintf("%s$%s (id: most categories held by fewer than %d persons; coarsen it to keep categories)",
                                tn, v, cfg$min_count))
        spec <- describe_id(df[[v]], id, cfg)
        spec$actions <- c("most categories held by fewer than min_count persons; coarsen it (substr(code, 1, 3), a district) to keep categories",
                          spec$actions)
      }
      spec$missing <- protected_share(sum(is.na(df[[v]])), nrow(df), cfg)
      if (is.character(df[[v]]) && spec$type %in% c("categorical", "text")) {
        spec$empty <- protected_share(sum(!is.na(df[[v]]) & !nzchar(df[[v]])), nrow(df), cfg)
      }
      if (role$auto) spec$actions <- c(paste("classified automatically as", role$role), spec$actions)
      vars[[v]] <- spec
      if (length(spec$actions)) {
        modified[[length(modified) + 1L]] <- data.frame(table = tn, variable = v,
                                                        action = spec$actions,
                                                        stringsAsFactors = FALSE)
      }
    }

    rpp <- NULL
    with_rows <- NULL
    orphans <- 0
    if (!is.na(key) && !identical(tn, k$main) &&
        (!is.null(main_keys) || anyDuplicated(df[[key]]))) {
      kv <- as.character(df[[key]])
      if (!is.null(main_keys)) {
        counts <- tabulate(match(kv, main_keys), length(main_keys))
        orphans <- protected_share(sum(!is.na(kv) & !kv %in% main_keys), nrow(df), cfg)
        with_rows <- protected_share(sum(counts > 0), length(main_keys), cfg)
      } else {
        counts <- as.vector(table(kv))
      }
      rpp <- describe_numeric(counts, seq_along(counts), cfg)
      if (!is.null(main_keys)) {
        child_counts[[paste0(".rows_", tn)]] <- list(x = counts, spec = rpp)
      }
    }

    out[[tn]] <- list(
      name = tn,
      key = key,
      is_main = identical(tn, k$main),
      n_rows = round_count(nrow(df), cfg),
      variables = vars,
      rows_per_person = rpp,
      share_with_rows = with_rows,
      orphan_share = orphans
    )
  }

  # Coarse correlations for level 3. Correlation coefficients from enough
  # persons are "safe" output in the Eurostat guidelines; they are rounded
  # to one decimal and only stored when there are at least min_persons.
  if (n_persons >= cfg$min_persons) {
    for (tn in names(out)) {
      extra <- if (identical(tn, k$main)) child_counts else list()
      out[[tn]]$cor <- latent_correlation(tables[[tn]], out[[tn]]$variables, cfg, extra)
    }
  } else {
    modified[[length(modified) + 1L]] <- data.frame(
      table = names(out)[1], variable = "(all)",
      action = sprintf("correlations not stored: fewer than %d persons", cfg$min_persons),
      stringsAsFactors = FALSE)
  }

  if (length(auto)) {
    cli::cli_warn(c("Classified {length(auto)} variable{?s} automatically to protect them:",
                    stats::setNames(cli_escape(auto), rep("*", length(auto))),
                    "i" = "Set {.arg id_vars} or {.arg text_vars} in {.fn mimic_config} to be explicit."))
  }

  risk <- build_risk(tables, k, cfg, n_persons, modified)

  structure(
    list(
      version = as.character(utils::packageVersion("mimic")),
      created = format(Sys.Date(), "%Y-%m"),
      config = cfg,
      keys = k,
      n_persons = round_count(n_persons, cfg),
      tables = out,
      risk = risk
    ),
    class = "mimic_profile"
  )
}

# Person ids for counting. Missing keys count as separate persons, which is
# the conservative choice for merging (they cannot help reach min_count).
person_ids <- function(v) {
  v <- as.character(v)
  na <- is.na(v)
  v[na] <- paste0(".na_", seq_len(sum(na)))
  v
}

id_name_pattern <- "(^id$|_id$|^id_|\\.id$|^cpr|^pnr|^ssn)"

# The ASCII letters and digits, and what describe_id() writes for each.
id_chars <- paste(c(LETTERS, letters, 0:9), collapse = "")
id_marks <- paste(rep(c("A", "9"), c(52, 10)), collapse = "")

variable_role <- function(name, x, key, cfg) {
  role <- function(r, auto = FALSE) list(role = r, auto = auto)
  if (!is.na(key) && identical(name, key)) return(role("id"))
  if (name %in% cfg$id_vars) return(role("id"))
  if (name %in% cfg$text_vars) return(role("text"))
  if (name %in% cfg$geo_vars) return(role("categorical"))
  if (inherits(x, c("Date", "POSIXt"))) return(role("date"))
  if (is.logical(x)) return(role("logical"))
  looks_like_id <- grepl(id_name_pattern, name, ignore.case = TRUE)
  if (is.numeric(x)) return(if (looks_like_id) role("id", TRUE) else role("numeric"))
  if (is.character(x) || is.factor(x)) {
    if (looks_like_id) return(role("id", TRUE))
    if (is.character(x)) {
      xs <- x[!is.na(x)]
      nu <- length(unique(xs))
      if (length(xs) >= cfg$min_count) {
        if (nu > 0.5 * length(xs) && mean(nchar(xs)) >= 20) return(role("text", TRUE))
        if (nu == length(xs)) return(role("id", TRUE))
      }
    }
    return(role("categorical"))
  }
  role("unsupported")
}

describe_variable <- function(x, id, role, cfg, geo = FALSE) {
  switch(role,
         id = describe_id(x, id, cfg),
         text = describe_text(x, cfg),
         categorical = describe_categorical(x, id, cfg, geo),
         logical = describe_logical(x, id, cfg),
         numeric = describe_numeric_var(x, id, cfg),
         date = describe_date(x, id, cfg),
         list(type = "unsupported", class = class(x)[1],
              actions = "not described: unsupported type"))
}

# Formats are counted in persons: a format held by one person with twenty
# contacts is a format of one person.
describe_id <- function(x, id, cfg) {
  ok <- !is.na(x)
  s <- if (is.numeric(x)) format(x[ok], scientific = FALSE, trim = TRUE) else as.character(x[ok])
  ido <- id[ok]
  # patterns of the distinct values: an id repeats on every contact
  us <- unique(s)
  # ASCII letters and digits character by character; the regular expressions
  # only for values with other characters (letters outside ASCII)
  upat <- chartr(id_chars, id_marks, us)
  other <- grepl("[^\\x01-\\x7F]", us, perl = TRUE, useBytes = TRUE)
  upat[other] <- gsub("[[:digit:]]", "9", gsub("[[:alpha:]]", "A", us[other]))
  pats <- sort(unique(upat))
  ucode <- match(upat, pats)
  code <- ucode[match(s, us)]
  persons <- persons_per(ido, code, length(pats))
  rows <- tabulate(code, length(pats))
  keep <- which(persons >= cfg$min_count)
  formats <- vapply(keep, function(k) {
    tmpl <- strsplit(pats[k], "")[[1]]
    if (persons[k] >= cfg$min_persons) {
      chars <- do.call(rbind, strsplit(us[ucode == k], ""))
      for (j in seq_along(tmpl)) {
        cj <- unique(chars[, j])
        if (length(cj) == 1 && !grepl("[[:digit:]]", cj)) tmpl[j] <- cj
      }
    }
    paste(tmpl, collapse = "")
  }, character(1), USE.NAMES = FALSE)
  share <- vapply(keep, function(k) protected_share(rows[k], length(s), cfg), numeric(1),
                  USE.NAMES = FALSE)
  list(type = "id", class = class(x)[1],
       formats = data.frame(format = formats, share = share, stringsAsFactors = FALSE),
       unique = !anyDuplicated(s),
       actions = "identifier: only the format is stored")
}

describe_text <- function(x, cfg) {
  xs <- as.character(x[!is.na(x)])
  len <- if (length(xs) >= cfg$min_count) signif(mean(nchar(xs)), 1) else NA_real_
  list(type = "text", class = class(x)[1], approx_length = len,
       actions = "free text: only the approximate length is stored")
}

describe_categorical <- function(x, id, cfg, geo = FALSE) {
  xc <- as.character(x)
  ok <- !is.na(xc)
  declared <- if (is.factor(x)) levels(x) else sort(unique(xc[ok]))
  idx <- match(xc[ok], declared)
  ido <- id[ok]
  persons <- persons_per(ido, idx, length(declared))
  rows <- tabulate(idx, length(declared))

  merged <- which(persons > 0 & persons < cfg$min_count)
  if (length(merged)) {
    repeat {
      if (length(unique(ido[idx %in% merged])) >= cfg$min_count) break
      rest <- setdiff(which(persons > 0), merged)
      if (!length(rest)) break
      merged <- c(merged, rest[which.min(persons[rest])])
    }
    clash <- which(declared == cfg$other_label)
    merged <- union(merged, clash)
  }
  # factor levels that no one holds are labels of fewer than min_count
  # persons too, and may come from elsewhere (a name list, a full register)
  unused <- setdiff(which(persons == 0), merged)
  kept <- setdiff(seq_along(declared), c(merged, unused))
  lv <- c(declared[kept], if (length(merged)) cfg$other_label)
  freq <- round_count(c(rows[kept], if (length(merged)) sum(rows[merged])), cfg)

  actions <- character()
  if (length(unused)) {
    actions <- sprintf("%d unused factor level%s not stored", length(unused),
                       if (length(unused) == 1) "" else "s")
  }
  if (length(merged)) {
    nm <- length(merged)
    what <- if (geo) {
      if (nm == 1) "small geographical unit" else "small geographical units"
    } else {
      if (nm == 1) "category" else "categories"
    }
    actions <- c(actions, sprintf("%d %s merged into '%s' (fewer than %d persons)",
                                  nm, what, cfg$other_label, cfg$min_count))
  }
  list(type = "categorical", class = class(x)[1], ordered = is.ordered(x), geo = geo,
       levels = lv, freq = freq, n_merged = length(merged),
       n_categories = length(kept) + length(merged),
       # categories below min_count themselves, without those merged to
       # make the merged group large enough
       n_rare = sum(persons > 0 & persons < cfg$min_count),
       actions = c(actions, "frequencies rounded"))
}

describe_logical <- function(x, id, cfg) {
  ok <- !is.na(x)
  idx <- as.integer(x[ok]) + 1L
  persons <- persons_per(id[ok], idx, 2)
  freq <- round_count(tabulate(idx, 2), cfg)
  small <- persons > 0 & persons < cfg$min_count
  freq[small] <- NA_real_
  list(type = "logical", class = "logical", levels = c(FALSE, TRUE), freq = freq,
       actions = c(if (any(small)) "a value held by few persons suppressed", "frequencies rounded"))
}

describe_numeric_var <- function(x, id, cfg) {
  spec <- describe_numeric(as.numeric(x), id, cfg)
  spec$class <- class(x)[1]
  spec
}

# Shared by numeric variables and rows-per-person distributions.
describe_numeric <- function(x, id, cfg) {
  ok <- !is.na(x)
  x <- x[ok]
  id <- id[ok]
  if (length(unique(id)) < cfg$min_count) return(suppressed_spec("numeric", x, cfg))
  b <- tail_bounds(x, id, cfg)
  if (is.null(b)) return(suppressed_spec("numeric", x, cfg))
  xw <- pmin(pmax(x, b[1]), b[2])
  is_int <- all(x == round(x))
  vals <- sort(unique(xw))
  discrete <- is_int && length(vals) <= 20

  if (discrete) {
    bin <- match(xw, vals)
    map <- merge_adjacent(bin, id, length(vals), cfg$min_count)
    dist <- data.frame(
      from = as.numeric(tapply(vals, map, min)),
      to = as.numeric(tapply(vals, map, max)),
      freq = round_count(tabulate(map[bin], max(map)), cfg)
    )
    lo <- b[1]
    hi <- b[2]
  } else {
    br <- pretty(b, n = 10)
    bin <- findInterval(xw, br, rightmost.closed = TRUE, all.inside = TRUE)
    nb <- length(br) - 1L
    map <- merge_adjacent(bin, id, nb, cfg$min_count)
    keep <- c(TRUE, map[-1] != map[-nb], TRUE)
    nbr <- br[keep]
    dist <- data.frame(from = nbr[-length(nbr)], to = nbr[-1],
                       freq = round_count(tabulate(map[bin], max(map)), cfg))
    rb <- signif_inward(b[1], b[2], cfg$signif_digits)
    lo <- rb[1]
    hi <- rb[2]
  }
  dg <- cfg$signif_digits
  list(type = "numeric", class = "numeric", integer = is_int, discrete = discrete,
       lo = lo, hi = hi, dist = dist,
       mean = signif(mean(xw), dg), median = signif(stats::median(xw), dg),
       sd = signif(stats::sd(xw), dg),
       actions = c("bottom and top coded", "binned", "summaries rounded"))
}

describe_date <- function(x, id, cfg) {
  cls <- class(x)[1]
  d <- as.Date(x)
  ok <- !is.na(d)
  days <- as.numeric(d[ok])
  id <- id[ok]
  if (length(unique(id)) < cfg$min_count) return(suppressed_spec("date", x, cfg))
  b <- tail_bounds(days, id, cfg)
  if (is.null(b)) return(suppressed_spec("date", x, cfg))
  unit <- cfg$date_unit
  lo <- floor_unit(as.Date(b[1], origin = "1970-01-01"), unit)
  hi <- ceil_unit(as.Date(b[2], origin = "1970-01-01"), unit)
  dw <- pmin(pmax(days, as.numeric(lo)), as.numeric(hi))
  br <- as.numeric(date_breaks(lo, hi))
  bin <- findInterval(dw, br, all.inside = TRUE)
  nb <- length(br) - 1L
  map <- merge_adjacent(bin, id, nb, cfg$min_count)
  keep <- c(TRUE, map[-1] != map[-nb], TRUE)
  nbr <- br[keep]
  dist <- data.frame(from = as.Date(nbr[-length(nbr)], origin = "1970-01-01"),
                     to = as.Date(nbr[-1] - 1, origin = "1970-01-01"),
                     freq = round_count(tabulate(map[bin], max(map)), cfg))
  med <- floor_unit(as.Date(stats::median(dw), origin = "1970-01-01"), unit)
  list(type = "date", class = cls, unit = unit, lo = lo, hi = hi, dist = dist, median = med,
       actions = c(paste("coarsened to", unit), "bottom and top coded", "binned by year"))
}

# Normal scores of a variable on its protected scale: numbers and dates top
# and bottom coded, categories merged as in the stored levels.
latent_scores <- function(x, spec, cfg) {
  v <- switch(spec$type,
              numeric = pmin(pmax(as.numeric(x), spec$lo), spec$hi),
              date = pmin(pmax(as.numeric(as.Date(x)), as.numeric(spec$lo)), as.numeric(spec$hi)),
              categorical = {
                xc <- as.character(x)
                code <- match(xc, spec$levels)
                code[is.na(code) & !is.na(xc)] <- match(cfg$other_label, spec$levels)
                as.numeric(code)
              },
              logical = as.numeric(x),
              NULL)
  if (is.null(v)) return(NULL)
  ok <- !is.na(v)
  z <- rep(NA_real_, length(v))
  z[ok] <- stats::qnorm((average_rank(v[ok]) - 0.5) / sum(ok))
  z
}

# rank(x, ties.method = "average") for a vector without missing values,
# from the number of rows with each distinct value: registers repeat their
# values, and rank() sorts all rows.
average_rank <- function(x) {
  u <- sort(unique(x))
  code <- match(x, u)
  n <- tabulate(code, length(u))
  last <- cumsum(n)
  ((last - n + 1 + last) / 2)[code]
}

# Correlation matrix of normal scores, rounded to one decimal. `extra` holds
# person-level columns (rows per person in linked tables) for the main table.
latent_correlation <- function(df, specs, cfg, extra = list()) {
  cols <- list()
  add <- function(nm, x, spec) {
    if (isTRUE(spec$suppressed) || !spec$type %in% c("numeric", "date", "categorical", "logical")) return()
    z <- latent_scores(x, spec, cfg)
    if (!is.null(z) && length(unique(z[!is.na(z)])) > 1) cols[[nm]] <<- z
  }
  for (v in names(specs)) add(v, df[[v]], specs[[v]])
  for (e in names(extra)) add(e, extra[[e]]$x, extra[[e]]$spec)
  if (length(cols) < 2) return(NULL)
  z <- do.call(cbind, cols)
  r <- suppressWarnings(stats::cor(z, use = "pairwise.complete.obs"))
  r[is.na(r)] <- 0
  diag(r) <- 1
  round(r, 1)
}

build_risk <- function(tables, k, cfg, n_persons, modified) {
  modified <- if (length(modified)) do.call(rbind, modified) else
    data.frame(table = character(), variable = character(), action = character())
  # routine actions are not worth a line each in the report
  routine <- c("frequencies rounded", "summaries rounded", "binned", "binned by year")
  changes <- modified[!modified$action %in% routine, , drop = FALSE]
  rownames(changes) <- NULL

  qi_table <- if (!is.na(k$main)) k$main else names(tables)[1]
  qi <- intersect(cfg$quasi_ids, names(tables[[qi_table]]))
  quasi <- NULL
  if (length(qi)) {
    key <- k$key[[qi_table]]
    quasi <- check_k_anonymity(tables[[qi_table]], qi,
                               id = if (is.na(key)) NULL else key,
                               config = cfg, target = qi_table)
  }
  keys <- if (any(!is.na(k$key))) check_keys(tables, k$key[!is.na(k$key)], main = if (is.na(k$main)) NULL else k$main,
                                             config = cfg) else NULL

  too_small <- n_persons < cfg$min_persons
  at_risk_share <- 0
  if (!is.null(quasi) && quasi$status == "fail") {
    p <- quasi$details$persons_at_risk[1]
    num <- suppressWarnings(as.numeric(gsub(",", "", p)))
    at_risk_share <- if (is.na(num)) 0 else num / n_persons
  }
  max_level <- if (too_small || at_risk_share > 0.2) 2L else 4L

  structure(list(
    changes = changes,
    quasi = quasi,
    keys = keys,
    n_persons = fmt_count(n_persons, cfg),
    too_small = too_small,
    at_risk_share = round(at_risk_share, 2),
    max_level = max_level,
    quasi_declared = length(cfg$quasi_ids) > 0
  ), class = "mimic_risk")
}

#' @export
print.mimic_risk <- function(x, ..., max_rows = 15) {
  cli::cli_h2("Risk report")
  ch <- x$changes
  if (nrow(ch)) {
    var <- paste0(ch$table, "$", ch$variable)
    var <- factor(var, levels = unique(var))
    lines <- vapply(split(ch$action, var), paste, character(1), collapse = "; ")
    n <- length(lines)
    cli::cli_text("Protection changed {n} variable{?s}:")
    show <- utils::head(lines, max_rows)
    cli::cli_ul(cli_escape(paste0(names(show), ": ", show)))
    if (n > max_rows) cli::cli_text("... and {n - max_rows} more (see {.code $risk$changes})")
  }
  if (!is.null(x$quasi)) print(x$quasi) else if (!x$quasi_declared) {
    cli::cli_alert_warning("No quasi-identifiers declared; rare combinations were not checked.")
  }
  if (!is.null(x$keys)) print(x$keys)
  if (x$too_small) {
    cli::cli_alert_danger("About {x$n_persons} persons: too few for safe synthesis above level 2.")
  }
  cli::cli_alert_info("Highest recommended fidelity level: {x$max_level}.")
  invisible(x)
}

#' @export
print.mimic_profile <- function(x, ...) {
  cli::cli_text("{.strong <mimic_profile>} {length(x$tables)} table{?s}, about {fmt_count(x$n_persons, x$config)} persons")
  for (t in x$tables) {
    types <- table(vapply(t$variables, `[[`, character(1), "type"))
    type_txt <- paste(sprintf("%d %s", as.integer(types), names(types)), collapse = ", ")
    role <- if (isTRUE(t$is_main)) "main, " else ""
    key <- if (is.na(t$key)) "no key" else paste("key", t$key)
    rows <- fmt_count(t$n_rows, x$config)
    line <- sprintf("%s (%s%s): %s rows; %s", t$name, role, key, rows, type_txt)
    if (!is.null(t$rows_per_person) && !isTRUE(t$rows_per_person$suppressed)) {
      line <- sprintf("%s; %s to %s+ rows per person", line,
                      t$rows_per_person$lo, t$rows_per_person$hi)
    }
    cli::cli_bullets(c("*" = cli_escape(line)))
  }
  print(x$risk)
  invisible(x)
}
