# Internal disclosure protection. Every number that profile_data() stores passes
# through one of these helpers. Nothing here returns a raw data value unless
# at least min_count persons share it or lie beyond it.

# Round counts to round_base. Counts between 0 and min_count become NA
# (suppressed): a rounded 3 is still a 3.
round_count <- function(n, cfg) {
  out <- cfg$round_base * round(n / cfg$round_base)
  out[!is.na(n) & n > 0 & n < cfg$min_count] <- NA_real_
  out
}

# Human-readable protected count for reports.
fmt_count <- function(n, cfg) {
  vapply(n, function(k) {
    # NA comes from round_count(), which suppresses small counts
    if (is.na(k)) return(paste("fewer than", cfg$min_count))
    if (k == 0) return("0")
    if (k < cfg$min_count) return(paste("fewer than", cfg$min_count))
    format(cfg$round_base * round(k / cfg$round_base),
           big.mark = ",", scientific = FALSE, trim = TRUE)
  }, character(1))
}

# Share k / n where small counts are reported as min_count (conservative,
# never reveals the exact small number).
protected_share <- function(k, n, cfg) {
  if (is.na(k) || k == 0 || n == 0) return(0)
  r <- if (k < cfg$min_count) cfg$min_count else cfg$round_base * round(k / cfg$round_base)
  min(1, signif(r / n, 2))
}

# Bottom and top coding bounds that never fall on fewer than min_count
# persons. With n persons, the 1 % quantile of a small table is one person's
# value; here the bound is the value at which at least
# max(ceiling(p * n), min_count) distinct persons lie at or beyond it.
tail_bounds <- function(x, id, cfg) {
  n_persons <- length(unique(id))
  t_lo <- max(ceiling(cfg$tail_coding[1] * n_persons), cfg$min_count)
  t_hi <- max(ceiling((1 - cfg$tail_coding[2]) * n_persons), cfg$min_count)

  bound <- function(o, target) {
    i <- which(cumsum(!duplicated(id[o])) >= target)[1]
    if (is.na(i)) NA_real_ else x[o][i]
  }
  lo <- bound(order(x), t_lo)
  hi <- bound(order(x, decreasing = TRUE), t_hi)
  if (is.na(lo) || is.na(hi) || lo > hi) return(NULL)
  c(lo, hi)
}

# Round bounds inwards to `digits` significant digits, so that at least as
# many persons lie beyond the stored bound as beyond the true one. When the
# values are so close that inward rounding crosses (1235.37 to 1238.91 at two
# digits), the bounds are rounded outwards instead: the unrounded bounds are
# single persons' values and must never be stored.
signif_inward <- function(lo, hi, digits) {
  mag <- function(x) if (x == 0) 1 else 10^(floor(log10(abs(x))) - digits + 1)
  r_lo <- ceiling(lo / mag(lo)) * mag(lo)
  r_hi <- floor(hi / mag(hi)) * mag(hi)
  if (r_lo > r_hi) {
    r_lo <- floor(lo / mag(lo)) * mag(lo)
    r_hi <- ceiling(hi / mag(hi)) * mag(hi)
  }
  c(r_lo, r_hi)
}

# Merge adjacent bins until every non-empty bin holds at least min_count
# distinct persons. `bin` gives the bin (1..n_bins) of each observation.
# Returns a map from old bin to new contiguous group number.
merge_adjacent <- function(bin, id, n_bins, min_count) {
  map <- seq_len(n_bins)
  repeat {
    labs <- sort(unique(map))
    if (length(labs) <= 1) break
    g <- map[bin]
    persons <- vapply(labs, function(l) length(unique(id[g == l])), numeric(1))
    small <- which(persons > 0 & persons < min_count)
    if (!length(small)) break
    i <- small[1]
    nb <- c(i - 1L, i + 1L)
    nb <- nb[nb >= 1 & nb <= length(labs)]
    j <- nb[which.min(persons[nb])]
    map[map == labs[i]] <- labs[j]
  }
  match(map, sort(unique(map)))
}

floor_unit <- function(d, unit) {
  as.Date(format(d, if (unit == "year") "%Y-01-01" else "%Y-%m-01"))
}

ceil_unit <- function(d, unit) {
  start <- floor_unit(d, unit)
  next_start <- if (unit == "year") {
    as.Date(paste0(as.integer(format(start, "%Y")) + 1L, "-01-01"))
  } else {
    as.Date(vapply(start, function(s) format(seq(s, by = "month", length.out = 2)[2]),
                   character(1)))
  }
  next_start - 1
}

# Year breaks from the year of lo to the year after hi, with a step that
# keeps the number of bins moderate.
date_breaks <- function(lo, hi) {
  y0 <- as.integer(format(lo, "%Y"))
  y1 <- as.integer(format(hi, "%Y")) + 1L
  span <- y1 - y0
  step <- if (span > 40) 10L else if (span > 20) 5L else 1L
  ys <- seq(y0, y1 + step - 1L, by = step)
  ys <- ys[seq_len(which(ys >= y1)[1])]
  as.Date(paste0(ys, "-01-01"))
}

# A variable with many categories, most of them held by fewer than min_count
# persons, is an identifier in disguise: names, addresses, household
# numbers. Its common values are households too. Codes such as diagnoses
# have small categories as well, but not most of them.
mostly_rare <- function(n_categories, n_rare) {
  n_categories >= 20 && n_rare > 0.5 * n_categories
}

suppressed_spec <- function(type, x, cfg) {
  list(type = type, class = class(x)[1], suppressed = TRUE,
       actions = paste("suppressed: fewer than", cfg$min_count, "persons with values"))
}
