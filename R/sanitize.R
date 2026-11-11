# Removing data values from text that leaves the secure environment: logs,
# error messages and warnings.
#
# The approach is a deny list built from the real data: every value that
# could point to a person is collected, and any occurrence in the text is
# replaced. A value points to a person when it is an identifier, free text,
# or held by fewer than min_count persons (a rare category, an exact income,
# a day-precise date). Common values ("F", "Town_A", "2020") are left alone,
# so the log stays readable.
#
# Persons are counted as in profile_data(): a code one person has on twelve
# contacts is held by one person, not twelve rows. real_run() knows no keys,
# so every identifier column of a table is tried and the smallest count
# wins. A wrong guess only makes the list longer.

# Number of holders of each of the values `u`, where `code` gives each row's
# value as its position in `u` (NA is no value): rows, or distinct values of
# an identifier in `ids` (integer codes, see id_codes()) if fewer. Done on
# integer codes, because the tables can have millions of rows.
holders <- function(code, u, ids) {
  nu <- length(u)
  n <- tabulate(code, nu)
  for (i in ids) {
    pair <- code + (i - 1) * as.numeric(nu)
    first <- !is.na(pair) & !duplicated(pair)
    n <- pmin(n, tabulate(code[first], nu))
  }
  n
}

# Each value as its position among the distinct values; NA is a value of
# its own, as for unique().
id_codes <- function(x) match(x, unique(x))

sensitive_values <- function(data, cfg) {
  strings <- character()
  texts <- character()
  numbers <- numeric()
  small <- numeric()
  for (df in data) {
    roles <- vapply(names(df), function(v) variable_role(v, df[[v]], NA_character_, cfg)$role,
                    character(1))
    id_cols <- names(df)[roles == "id"]
    codes <- lapply(stats::setNames(id_cols, id_cols), function(i) id_codes(df[[i]]))
    for (v in names(df)) {
      x <- df[[v]]
      role <- roles[[v]]
      ids <- codes[setdiff(id_cols, v)]
      if (inherits(x, c("Date", "POSIXt"))) {
        # counted on the day number, formatted only when rare
        day <- floor(as.numeric(as.Date(x)))
        u <- unique(day[!is.na(day)])
        d <- as.Date(u[holders(match(day, u), u, ids) < cfg$min_count], origin = "1970-01-01")
        # the same dates as scripts often print them: 31.01.1959, 31011959
        strings <- c(strings, written_dates(d))
      } else if (is.numeric(x)) {
        fin <- !is.na(x) & is.finite(x)
        if (role == "id") {
          rare <- unique(x[fin])
        } else {
          # persons are counted on the exact value: rounded to 6 digits,
          # 10000001 to 10000009 would share one key and look common. The
          # values are compared at the precision they are printed with
          # later (see rare_number_hits).
          key <- signif(x, 15)
          key[!fin] <- NA
          u <- unique(key[fin])
          code <- match(key, u)
          rare_u <- holders(code, u, ids) < cfg$min_count
          rare <- unique(x[fin & rare_u[code]])
        }
        numbers <- c(numbers, rare)
        # small whole numbers are too common to remove in general, but a rare
        # age in a quasi-identifier (the one person aged 97) is not
        if (v %in% cfg$quasi_ids) small <- c(small, rare[abs(rare) < 100 & rare == round(rare)])
      } else if (is.character(x) || is.factor(x)) {
        # work on the distinct values; blank ones are no value
        u <- if (is.factor(x)) levels(x) else unique(x[!is.na(x)])
        u <- u[nzchar(trimws(u))]
        code <- if (is.factor(x)) match(levels(x), u)[as.integer(x)] else match(x, u)
        if (role == "text") {
          texts <- c(texts, u[unique(code[!is.na(code)])])
        } else if (role == "id") {
          strings <- c(strings, u[unique(code[!is.na(code)])])
        } else {
          n <- holders(code, u, ids)
          # factor levels no one holds are no values
          tab <- u[n > 0]
          rare <- u[n > 0 & n < cfg$min_count]
          # names and addresses: when most values are rare, all are, as in
          # profile_data()
          if (mostly_rare(length(tab), length(rare))) rare <- tab
          # values shorter than 3 characters ("7", "12") alone point to no
          # one, and on the list they would remove every number from the
          # code in the log. A rare one in a quasi-identifier is kept, as
          # small numbers are.
          if (!v %in% cfg$quasi_ids) rare <- rare[nchar(rare) >= 3]
          strings <- c(strings, rare)
        }
      }
    }
  }
  # numbers with fewer than 3 significant digits (counts, years, codes) are
  # too common to identify anyone and would make logs unreadable
  big <- abs(numbers) >= 100 | (numbers != round(numbers))
  numbers <- unique(numbers[big])
  # the rare numbers rounded to 3 to 15 significant digits, for matching
  # printed output: summary() shows 38389.6 as 38390
  rounded <- lapply(seq_len(15), function(k) {
    if (k < 3) character() else unique(formatC(numbers, digits = k, format = "g"))
  })
  texts <- unique(texts[nchar(texts) >= 8])
  long <- texts[nchar(texts) >= text_prefix_length + 4]
  prefixes <- unique(substr(long, 1, text_prefix_length))
  c(list(strings = unique(strings), texts = texts, prefixes = prefixes, long_texts = long,
         numbers = numbers, small = unique(small), rounded = rounded),
    deny_parts(unique(strings), texts, prefixes))
}

# The deny list as redact() searches it, prepared once: redact() runs for
# every file, table and file name. Free text and values with spaces or other
# characters that split tokens are searched for as whole substrings, longest
# first (`multi`); the rest are compared with the tokens of the text
# (`single`). `multi_index` and `prefix_index` are for search_plan().
deny_parts <- function(strings, texts, prefixes) {
  multi <- c(texts, strings[grepl("[^A-Za-z0-9_.:-]", strings)])
  multi <- multi[order(-nchar(multi))]
  list(multi = multi, single = setdiff(strings, multi),
       multi_index = search_index(multi),
       prefix_index = search_index(prefixes, text_prefix_length))
}

# grepl() once per key reads the whole log again for every free text and
# identifier of a population. search_plan() reads the lines once per length
# instead: every stretch of the lines of that length is looked up among the
# first characters of the keys, which gives the lines a key can be in. Most
# keys are in none, and the rest are searched for in a few lines only.
#
# search_index() prepares the keys. They are compared on their first
# `key_length` characters, or twice or four times that when they are long
# enough, because the first words of a free text are in many lines; shorter
# keys on all of themselves. Keys of one length form a group with their
# distinct first characters (`heads`). `all` marks the keys to search for in
# every line: those of a group too small to be worth a reading of the lines,
# and keys that hold "<" or ">" or lie within "value". redact() changes the
# lines while it searches, and such a key could come to match across an
# inserted <value>.
search_index <- function(keys, key_length = 8L) {
  n <- length(keys)
  # a few keys are searched for faster than the stretches are collected
  if (n < 20) return(list(n = n, all = rep(TRUE, n), groups = list()))
  nk <- nchar(keys)
  all <- grepl("[<>]", keys) | keys %in% marker_pieces
  width <- ifelse(nk < key_length, nk, key_length * 2^pmin(floor(log2(nk / key_length)), 2))
  groups <- list()
  for (k in unique(width[!all])) {
    sel <- which(!all & width == k)
    if (length(sel) < 20) {
      all[sel] <- TRUE
      next
    }
    head <- substr(keys[sel], 1, k)
    heads <- unique(head)
    groups[[length(groups) + 1L]] <- list(width = k, keys = sel, heads = heads, head_of = match(head, heads))
  }
  list(n = n, all = all, groups = groups)
}

# Where each key of `index` may be found in `lines`: `lines`, the numbers of
# the lines to search for each key (none: it is in no line), and `all`, TRUE
# for the keys to search for in every line.
search_plan <- function(index, lines) {
  n <- index$n
  everywhere <- list(all = rep(TRUE, n), lines = vector("list", n))
  if (!length(lines)) return(everywhere)
  tryCatch({
    nl <- nchar(lines)
    # reading from the middle of a very long line is slow when it is not ASCII
    if (anyNA(nl) || max(nl) > 10000) return(everywhere)
    where <- vector("list", n)
    # at most about a million stretches at a time
    chunk <- cumsum(as.numeric(nl)) %/% 1e6
    for (g in index$groups) {
      k <- g$width
      long <- which(nl >= k)
      if (!length(long)) next
      found <- lapply(split(long, chunk[long]), function(i) {
        n_start <- nl[i] - k + 1L
        from <- sequence(n_start)
        m <- match(substring(rep.int(lines[i], n_start), from, from + k - 1L), g$heads)
        at <- !is.na(m)
        list(head = m[at], line = rep.int(i, n_start)[at])
      })
      head <- unlist(lapply(found, `[[`, "head"), use.names = FALSE)
      line <- unlist(lapply(found, `[[`, "line"), use.names = FALSE)
      if (!length(head)) next
      once <- !duplicated(head + (line - 1) * as.numeric(length(g$heads)))
      by_head <- split(line[once], head[once])
      where[g$keys] <- unname(by_head[match(g$head_of, as.integer(names(by_head)))])
    }
    list(all = index$all, lines = where)
  }, error = function(e) everywhere)
}

# Every piece of the word in the marker <value>.
marker_pieces <- unique(unlist(lapply(1:5, function(i) substring("value", i, i:5))))

# Formats besides ISO in which rare dates are searched for. Dates written
# with "/" are matched by redact() after turning "/" into ".".
date_formats <- c("%d.%m.%Y", "%d-%m-%Y", "%Y.%m.%d", "%m.%d.%Y", "%d.%m.%y",
                  "%Y%m%d", "%d%m%Y", "%d%m%y")

# The dates `d` in ISO form and in every format of date_formats, in that
# order. Written from the year, month and day: format() goes through
# strftime() for every date and format, which is slow when most birth dates
# of a population are rare. Years outside 1000 to 9999 are left to format(),
# which pads them differently between platforms.
written_dates <- function(d) {
  if (!length(d)) return(character())
  lt <- as.POSIXlt(d)
  y <- lt$year + 1900L
  if (anyNA(y) || any(y < 1000 | y > 9999)) {
    return(c(format(d), unlist(lapply(date_formats, function(f) format(d, f)))))
  }
  m <- lt$mon + 1L
  dd <- lt$mday
  y2 <- y %% 100L
  c(sprintf("%04d-%02d-%02d", y, m, dd),
    sprintf("%02d.%02d.%04d", dd, m, y), sprintf("%02d-%02d-%04d", dd, m, y),
    sprintf("%04d.%02d.%02d", y, m, dd), sprintf("%02d.%02d.%04d", m, dd, y),
    sprintf("%02d.%02d.%02d", dd, m, y2), sprintf("%04d%02d%02d", y, m, dd),
    sprintf("%02d%02d%04d", dd, m, y), sprintf("%02d%02d%02d", dd, m, y2))
}

# Free text cut short (substr(), a tibble or str() printing the first part)
# is found from its first characters.
text_prefix_length <- 16L

# Numbers written with a thousands separator: 9.800.000,5 or 38,390 or
# 1 234 567. Matched on the whole line, because "," and " " split tokens.
grouped_number <- "(?<![0-9.,])-?[0-9]{1,3}(?:[.,' ][0-9]{3})+(?:[.,][0-9]+)?(?![0-9])"

# Read a grouped number both ways: every separator removed, and the last
# "." or "," as the decimal mark. A hit on either is a hit.
grouped_hits <- function(s, sens) {
  plain <- gsub("[^0-9-]", "", s)
  has_dec <- grepl("[.,][0-9]+$", s)
  dec <- ifelse(has_dec,
                paste0(gsub("[^0-9-]", "", sub("[.,][0-9]+$", "", s)), ".", sub("^.*[.,]", "", s)),
                plain)
  rare_number_hits(plain, sens) | rare_number_hits(dec, sens)
}

# Replace every stretch of `line` that starts with a known text prefix and
# goes on as one of the free texts. Returns the line and the number of hits.
redact_prefix <- function(line, prefix, texts) {
  cand <- texts[startsWith(texts, prefix)]
  n <- 0L
  repeat {
    pos <- regexpr(prefix, line, fixed = TRUE)
    if (pos < 0) break
    rest <- substring(line, pos)
    len <- max(vapply(cand, function(t) {
      k <- min(nchar(rest), nchar(t))
      a <- strsplit(substr(rest, 1, k), "")[[1]]
      b <- strsplit(substr(t, 1, k), "")[[1]]
      m <- which(a != b)[1]
      if (is.na(m)) k else m - 1L
    }, integer(1)))
    line <- paste0(substr(line, 1, pos - 1), "<value>", substring(line, pos + len))
    n <- n + 1L
  }
  list(line = line, n = n)
}

# Significant digits a number is written with. Trailing zeros of a whole
# number may come from rounding, so "38390" counts as 4 digits; "72.30" as 4.
written_digits <- function(s) {
  m <- sub("[eE].*$", "", sub("^[-+]", "", s))
  whole <- !grepl(".", m, fixed = TRUE)
  d <- sub("^0+", "", gsub(".", "", m, fixed = TRUE))
  d[whole] <- sub("0+$", "", d[whole])
  nchar(d)
}

# Which written numbers equal a rare value, either exactly (at least three
# digits) or at the precision they are written with (at least three
# significant digits). Comparing at full precision alone lets rounded
# minima and maxima from print(summary(x)) through.
rare_number_hits <- function(s, sens) {
  hit <- logical(length(s))
  if ((!length(sens$numbers) && !length(sens$small)) || !length(s)) return(hit)
  num <- suppressWarnings(as.numeric(s))
  ok <- !is.na(num)
  hit[ok] <- num[ok] %in% sens$numbers & nchar(gsub("[^0-9]", "", s[ok])) >= 3
  # rare small values of quasi-identifiers, written as whole numbers
  whole <- grepl("^-?[0-9]+(\\.0+)?$", s)
  hit[ok] <- hit[ok] | (num[ok] %in% sens$small & whole[ok])
  k <- pmin(written_digits(s), 15L)
  for (kk in unique(k[ok & k >= 3])) {
    sel <- ok & k == kk
    hit[sel] <- hit[sel] | formatC(num[sel], digits = kk, format = "g") %in% sens$rounded[[kk]]
  }
  hit
}

# Replace the matches of `pattern` for which `is_hit` (given all matches of
# all lines as one vector) is TRUE with <value>. Returns the lines and the
# number of replacements per line.
replace_matches <- function(lines, pattern, is_hit, perl = FALSE) {
  m <- gregexpr(pattern, lines, perl = perl)
  found <- regmatches(lines, m)
  nm <- lengths(found)
  n <- integer(length(lines))
  if (!sum(nm)) return(list(lines = lines, n = n))
  s <- unlist(found, use.names = FALSE)
  hit <- is_hit(s)
  if (!any(hit)) return(list(lines = lines, n = n))
  line_of <- rep.int(seq_along(lines), nm)
  n <- tabulate(line_of[hit], length(lines))
  s[hit] <- "<value>"
  sel <- n > 0
  regmatches(lines[sel], m[sel]) <- split(s, factor(line_of, levels = seq_along(lines)))[sel]
  list(lines = lines, n = n)
}

# Replace sensitive values with <value>. With drop_rows = TRUE a line with
# two or more sensitive values is removed entirely: it is most likely a
# printed row, and the common values left in it (sex, settlement,
# occupation) can still single out a person in a small place.
redact <- function(lines, sens, drop_rows = FALSE) {
  if (!length(lines)) return(list(lines = lines, n = 0L, dropped = 0L))
  per_line <- integer(length(lines))
  # free text and values with spaces first, as whole substrings
  parts <- if (is.null(sens$multi_index)) deny_parts(sens$strings, sens$texts, sens$prefixes) else sens
  multi <- parts$multi
  plan <- search_plan(parts$multi_index, lines)
  for (j in which(plan$all | lengths(plan$lines) > 0)) {
    s <- multi[j]
    at <- if (plan$all[j]) seq_along(lines) else plan$lines[[j]]
    hit <- at[grepl(s, lines[at], fixed = TRUE)]
    if (length(hit)) {
      per_line[hit] <- per_line[hit] + lengths(regmatches(lines[hit], gregexpr(s, lines[hit], fixed = TRUE)))
      lines[hit] <- gsub(s, "<value>", lines[hit], fixed = TRUE)
    }
  }
  # free text cut short
  plan <- search_plan(parts$prefix_index, lines)
  for (j in which(plan$all | lengths(plan$lines) > 0)) {
    p <- sens$prefixes[j]
    at <- if (plan$all[j]) seq_along(lines) else plan$lines[[j]]
    for (i in at[grepl(p, lines[at], fixed = TRUE)]) {
      r <- redact_prefix(lines[i], p, sens$long_texts)
      lines[i] <- r$line
      per_line[i] <- per_line[i] + r$n
    }
  }
  # The matches of each pattern below are checked for all lines at once:
  # %in% builds its lookup table anew on every call, and the deny list can
  # hold every identifier of a population.
  # numbers with thousands separators
  if (length(sens$numbers) || length(sens$small)) {
    r <- replace_matches(lines, grouped_number, function(s) grouped_hits(s, sens), perl = TRUE)
    lines <- r$lines
    per_line <- per_line + r$n
  }
  single <- parts$single
  # dates written with "/" are split into tokens; compare 31/01/1959 as 31.01.1959
  r <- replace_matches(lines, "(?<![0-9/])[0-9]{1,4}/[0-9]{1,2}/[0-9]{1,4}(?![0-9/])",
                       function(s) gsub("/", ".", s, fixed = TRUE) %in% single, perl = TRUE)
  lines <- r$lines
  per_line <- per_line + r$n
  r <- replace_matches(lines, "[A-Za-z0-9_.:-]+", function(tk) {
    core <- gsub("^[.:-]+|[.:-]+$", "", tk)
    hit <- core %in% single | rare_number_hits(core, sens)
    # file names and compound names: P2410400.csv, result_P2410400.csv,
    # 10000001-tab.rds, persons.P2410400. Parts shorter than 3 characters
    # are left alone: the 15 of 2020-01-15 or the 3 of table_3.csv.
    parts_of <- which(!hit & grepl("[._-]", core))
    if (length(parts_of)) {
      parts <- strsplit(core[parts_of], "[._-]")
      p <- unlist(parts, use.names = FALSE)
      owner <- rep.int(parts_of, lengths(parts))
      long <- nchar(p) >= 3
      p <- p[long]
      owner <- owner[long]
      hit[unique(owner[p %in% single | rare_number_hits(p, sens)])] <- TRUE
    }
    hit
  })
  lines <- r$lines
  per_line <- per_line + r$n
  dropped <- 0L
  if (drop_rows) {
    rows <- per_line >= 2
    dropped <- sum(rows)
    lines[rows] <- "<line removed: it contained data values>"
  }
  list(lines = lines, n = sum(per_line), dropped = dropped)
}

# Error messages get stricter treatment than logs: quoted parts that are not
# words from the script or names in the data are removed, and every number is
# replaced. Only the function name of the call is kept, because a call can
# carry whole data frames.
sanitize_message <- function(msg, sens, known) {
  if (is.null(msg) || !length(msg)) return("")
  msg <- paste(msg, collapse = " ")
  quotes <- list(c("'", "'"), c("\"", "\""), c("`", "`"),
                 c("\u2018", "\u2019"), c("\u201c", "\u201d"))
  for (q in quotes) {
    pat <- paste0(q[1], "([^", q[1], q[2], "]*)", q[2])
    m <- gregexpr(pat, msg, perl = TRUE)
    parts <- regmatches(msg, m)[[1]]
    if (length(parts)) {
      inner <- substr(parts, 2, nchar(parts) - 1)
      keep <- inner %in% known
      parts[!keep] <- paste0(q[1], "<value>", q[2])
      regmatches(msg, m) <- list(parts)
    }
  }
  msg <- redact(msg, sens)$lines
  msg <- gsub("-?[0-9]+([.,][0-9]+)?([eE][-+]?[0-9]+)?", "<n>", msg)
  if (nchar(msg) > 500) msg <- paste0(substr(msg, 1, 497), "...")
  msg
}

# Non-revealing description of the variables a failed script touched.
variable_report <- function(data, vars, cfg) {
  rows <- list()
  for (tn in names(data)) {
    df <- data[[tn]]
    for (v in intersect(vars, names(df))) {
      x <- df[[v]]
      n <- length(x)
      nd <- length(unique(x[!is.na(x)]))
      rows[[length(rows) + 1L]] <- data.frame(
        table = tn,
        variable = v,
        class = paste(class(x), collapse = "/"),
        missing = sprintf("%s %%", round(100 * protected_share(sum(is.na(x)), n, cfg), 1)),
        distinct = if (nd <= 50) as.character(nd) else paste("about", fmt_count(nd, cfg)),
        empty_strings = if (is.character(x) || is.factor(x)) any(!is.na(x) & !nzchar(trimws(as.character(x)))) else NA,
        all_missing = all(is.na(x)),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    return(data.frame(table = character(), variable = character(), class = character(),
                      missing = character(), distinct = character(), empty_strings = logical(),
                      all_missing = logical()))
  }
  do.call(rbind, rows)
}
