# Synthesis engines for fidelity levels 3 and 4. Each engine fills the
# variables of a table; mimic() handles keys, linked tables and mess. Kept
# apart so a method can be improved without touching the rest.

# --- level 3: Gaussian copula from coarse correlations ------------------------

# Nearest valid correlation matrix: rounding can make the stored matrix
# slightly indefinite.
nearest_cor <- function(r) {
  e <- eigen(r, symmetric = TRUE)
  s <- e$vectors %*% diag(pmax(e$values, 1e-3), nrow = length(e$values)) %*% t(e$vectors)
  s <- stats::cov2cor(s)
  dimnames(s) <- dimnames(r)
  s
}

# n rows of correlated uniforms, one column per variable in r.
copula_uniforms <- function(r, n) {
  r <- nearest_cor(r)
  z <- matrix(stats::rnorm(n * ncol(r)), nrow = n) %*% chol(r)
  u <- stats::pnorm(z)
  colnames(u) <- colnames(r)
  u
}

# Inverse of the stored, protected distribution: maps uniforms to values so
# level 3 keeps exactly the marginals of level 2.
inv_binned <- function(spec, u, discrete) {
  d <- spec$dist
  lo <- as.numeric(spec$lo)
  hi <- as.numeric(spec$hi)
  from <- pmax(as.numeric(d$from), lo)
  to <- pmax(pmin(as.numeric(d$to), hi), from)
  f <- d$freq
  f[is.na(f)] <- 0
  if (sum(f) == 0) f <- rep(1, length(f))
  cum <- cumsum(f) / sum(f)
  b <- pmin(findInterval(u, cum) + 1L, length(f))
  prev <- c(0, cum)[b]
  frac <- (u - prev) / (cum[b] - prev)
  frac[!is.finite(frac)] <- 0.5
  frac <- pmin(pmax(frac, 0), 1 - 1e-9)
  if (discrete) from[b] + floor(frac * (to[b] - from[b] + 1)) else from[b] + frac * (to[b] - from[b])
}

inverse_marginal <- function(spec, u, cfg) {
  switch(spec$type,
         categorical = {
           p <- spec$freq
           if (anyNA(p) || sum(p) == 0) p <- rep(1, length(p))
           cum <- cumsum(p) / sum(p)
           restore_class(spec$levels[pmin(findInterval(u, cum) + 1L, length(p))], spec)
         },
         logical = {
           f <- spec$freq
           f[is.na(f)] <- cfg$min_count
           u > f[1] / sum(f)
         },
         numeric = {
           x <- inv_binned(spec, u, discrete = isTRUE(spec$discrete))
           if (isTRUE(spec$integer)) x <- round(x)
           restore_class(x, spec)
         },
         date = restore_class(as.Date(floor(inv_binned(spec, u, TRUE)), origin = "1970-01-01"), spec))
}

# --- level 4: sequential synthesis with classification and regression trees ----

modelled_types <- c("numeric", "date", "categorical", "logical")

# Real data on the protected scale used for modelling: numbers and dates
# top and bottom coded, categories merged as in profile_data(). Dates as days,
# categories and logicals as factors.
protect_frame <- function(df, specs, cfg) {
  out <- list()
  for (v in names(specs)) {
    s <- specs[[v]]
    if (isTRUE(s$suppressed) || !s$type %in% modelled_types || !v %in% names(df)) next
    x <- df[[v]]
    out[[v]] <- switch(s$type,
                       numeric = pmin(pmax(as.numeric(x), s$lo), s$hi),
                       date = pmin(pmax(as.numeric(as.Date(x)), as.numeric(s$lo)), as.numeric(s$hi)),
                       categorical = {
                         xc <- as.character(x)
                         xc[!is.na(xc) & !xc %in% s$levels & cfg$other_label %in% s$levels] <- cfg$other_label
                         factor(xc, levels = s$levels)
                       },
                       logical = factor(as.character(x), levels = c("FALSE", "TRUE")))
  }
  as.data.frame(out, optional = TRUE, stringsAsFactors = FALSE)
}

# Factors with many levels make tree growing exponential for multi-class
# responses; as predictors they are replaced by codes ordered by frequency.
as_predictors <- function(df, ref = df) {
  for (v in names(df)) {
    if (is.factor(ref[[v]]) && nlevels(ref[[v]]) > 10) {
      # counted and looked up on the level codes, not on the labels of every row
      lv <- levels(ref[[v]])
      ord <- lv[order(tabulate(as.integer(ref[[v]]), length(lv)), decreasing = TRUE)]
      x <- df[[v]]
      df[[v]] <- if (is.factor(x)) match(levels(x), ord)[as.integer(x)] else match(as.character(x), ord)
    }
  }
  df
}

# Draw one variable for the synthetic rows from donors in the same tree
# leaf, then protect the drawn values: numbers and dates are smoothed with
# noise and kept within the bounds, discrete numbers are drawn within their
# protected bin. `pid` gives the person of each real row in a linked table
# (NULL: one row per person). rpart's minbucket counts rows, and a leaf of
# five contacts can be one person, so leaves with fewer than min_count
# persons draw from the pooled small leaves, or from all rows if the pool is
# too small as well.
cart_draw <- function(y, real_pred, syn_pred, spec, cfg, pid = NULL) {
  n <- nrow(syn_pred)
  ok <- !is.na(y)
  marginal <- function() model_scale(inverse_marginal(spec, stats::runif(n), cfg), spec)
  pv <- if (is.null(pid)) which(ok) else pid[ok]
  if (!ncol(syn_pred) || length(unique(pv)) < 2 * cfg$min_count) return(marginal())

  # taking rows out of a data frame is slow, and most variables have no
  # missing values
  real_ok <- if (all(ok)) real_pred else real_pred[ok, , drop = FALSE]
  train <- as_predictors(real_ok)
  train$.y <- y[ok]
  method <- if (is.factor(y)) "class" else "anova"
  fit <- tryCatch(
    rpart::rpart(.y ~ ., data = train, method = method,
                 control = rpart::rpart.control(minbucket = max(cfg$min_count, 5), cp = 0.001,
                                                maxcompete = 0, xval = 0)),
    error = function(e) NULL)
  if (is.null(fit)) return(marginal())

  leaf_fit <- fit
  leaf_fit$frame$yval <- seq_len(nrow(fit$frame))
  leaf_real <- fit$where
  leaf_syn <- tryCatch(
    as.integer(stats::predict(leaf_fit, newdata = as_predictors(syn_pred, real_ok), type = "vector")),
    error = function(e) NULL)
  if (is.null(leaf_syn)) return(marginal())

  # factors are drawn as their integer codes: assigning into a factor leaf
  # by leaf is slow
  yv <- if (is.factor(y)) as.integer(y[ok]) else y[ok]
  leaves <- sort(unique(leaf_real))
  leaf_persons <- persons_per(pv, match(leaf_real, leaves), length(leaves))
  small <- leaves[leaf_persons < cfg$min_count]
  pool <- which(leaf_real %in% small)
  if (length(unique(pv[pool])) < cfg$min_count) pool <- seq_along(yv)
  # rows of each leaf, found once rather than by scanning all rows per leaf
  syn_rows <- split(seq_len(n), leaf_syn)
  real_rows <- split(seq_along(yv), leaf_real)
  out <- rep(if (is.factor(y)) NA_integer_ else NA_real_, n)
  for (leaf in unique(leaf_syn)) {
    key <- as.character(leaf)
    idx <- syn_rows[[key]]
    donors <- if (leaf %in% small) yv[pool] else yv[real_rows[[key]]]
    if (!length(donors)) donors <- yv
    draw <- donors[sample.int(length(donors), length(idx), replace = TRUE)]
    if (!is.factor(y) && !isTRUE(spec$discrete)) {
      bw <- if (length(unique(donors)) > 1) stats::bw.nrd0(donors) else 0
      draw <- draw + stats::rnorm(length(idx), 0, bw)
    }
    out[idx] <- draw
  }
  if (is.factor(y)) out <- factor(levels(y)[out], levels = levels(y))
  if (!is.factor(y)) {
    lo <- as.numeric(spec$lo)
    hi <- as.numeric(spec$hi)
    if (isTRUE(spec$discrete)) {
      # donor value -> its protected bin -> a value drawn within the bin
      d <- spec$dist
      b <- findInterval(out, as.numeric(d$from))
      b <- pmin(pmax(b, 1L), nrow(d))
      from <- pmax(as.numeric(d$from)[b], lo)
      to <- pmax(pmin(as.numeric(d$to)[b], hi), from)
      out <- from + floor(stats::runif(n) * (to - from + 1))
    }
    out <- pmin(pmax(out, lo), hi)
    if (spec$type == "date" || isTRUE(spec$integer)) out <- round(out)
  }
  out
}

# Value on the modelling scale (days, factors) from a generated vector.
model_scale <- function(x, spec) {
  switch(spec$type,
         date = as.numeric(as.Date(x)),
         categorical = factor(as.character(x), levels = spec$levels),
         logical = factor(as.character(x), levels = c("FALSE", "TRUE")),
         as.numeric(x))
}

# Back from the modelling scale to the original class.
from_model_scale <- function(x, spec) {
  switch(spec$type,
         date = restore_class(as.Date(x, origin = "1970-01-01"), spec),
         categorical = restore_class(as.character(x), spec),
         logical = as.logical(as.character(x)),
         restore_class(x, spec))
}

# The real rows the trees are fitted on: all of them, or at most
# `train_rows`. Growing a tree takes time in proportion to the rows, and a
# register of contacts can have millions; a tree on 100,000 rows finds the
# same structure. Whole persons are sampled (`pid`), so a person's rows stay
# together and the leaves still count persons correctly.
training_rows <- function(n, train_rows, pid = NULL) {
  if (is.null(train_rows) || n <= train_rows) return(seq_len(n))
  if (is.null(pid)) return(sort(sample.int(n, train_rows)))
  code <- match(pid, unique(pid))
  size <- tabulate(code)
  ord <- sample.int(length(size))
  chosen <- ord[cumsum(size[ord]) <= train_rows]
  if (!length(chosen)) chosen <- ord[1]
  which(code %in% chosen)
}

# Rows `i` of a data frame, column by column and with new row numbers.
# `df[i, ]` also makes a unique row name for every repeated row, which takes
# longer than the rows themselves when persons are repeated for each of
# their contacts.
take_rows <- function(df, i) {
  out <- lapply(df, function(x) x[i])
  attr(out, "row.names") <- .set_row_names(length(i))
  class(out) <- "data.frame"
  out
}

# Synthesise the modelled variables of one table in column order. `given`
# holds synthetic predictors fixed beforehand (the person's variables for a
# linked table) and `given_real` the matching real values.
cart_table <- function(real_prot, specs, n, cfg, given = NULL, given_real = NULL, pid = NULL,
                       train_rows = NULL) {
  # persons as whole numbers: they are counted for every variable
  if (!is.null(pid)) pid <- match(pid, unique(pid))
  keep <- training_rows(nrow(real_prot), train_rows, pid)
  if (length(keep) < nrow(real_prot)) {
    real_prot <- take_rows(real_prot, keep)
    if (!is.null(given_real)) given_real <- take_rows(given_real, keep)
    if (!is.null(pid)) pid <- pid[keep]
  }
  syn <- if (is.null(given)) data.frame(row.names = seq_len(n)) else given
  realp <- if (is.null(given_real)) data.frame(row.names = seq_len(nrow(real_prot))) else given_real
  for (v in names(real_prot)) {
    spec <- specs[[v]]
    y <- real_prot[[v]]
    val <- cart_draw(y, realp, syn, spec, cfg, pid)
    syn[[v]] <- val
    realp[[v]] <- y
  }
  syn
}

# --- risk report for levels 3 and 4 ---------------------------------------------

# One number per row of each data frame in `frames`, equal for rows with the
# same values of `cols` across all frames (numbers to 6 digits, dates to the
# day, NA a value of its own). Built column by column on integer codes:
# pasting the values into strings is slow on hundreds of thousands of rows.
row_keys <- function(frames, cols) {
  size <- vapply(frames, nrow, integer(1))
  key <- rep(1, sum(size))
  # the keys lie between 1 and `span`; they are only renumbered when another
  # column would take them beyond the whole numbers a double holds
  span <- 1
  for (cl in cols) {
    v <- unlist(lapply(frames, function(f) {
      x <- f[[cl]]
      if (inherits(x, c("Date", "POSIXt"))) x <- floor(as.numeric(as.Date(x)))
      if (is.factor(x)) x <- as.character(x)
      if (is.numeric(x)) x <- signif(x, 6)
      x
    }), use.names = FALSE)
    u <- unique(v)
    if (span * length(u) > 2^52) {
      key <- match(key, unique(key))
      span <- max(key)
    }
    key <- key + (match(v, u) - 1) * span
    span <- span * length(u)
  }
  split(key, factor(rep.int(seq_along(frames), size), levels = seq_along(frames)))
}

# How close the copy comes to the real rows: the share of synthetic rows
# identical to a real row, and replicated uniques (combinations unique in
# the copy that are also unique in the real data).
copy_risk <- function(out, real, m, fidelity) {
  cfg <- m$config
  rows <- list()
  if (!is.null(real)) {
    for (tn in names(out)) {
      t <- m$tables[[tn]]
      rd <- real[[tn]]
      if (is.null(rd)) next
      cols <- names(t$variables)[vapply(t$variables, function(s) s$type %in% modelled_types &&
                                          !isTRUE(s$suppressed), logical(1))]
      cols <- intersect(cols, names(rd))
      if (!length(cols)) next
      # chance level: the same copy with every column shuffled on its own
      shuffled <- as.data.frame(lapply(out[[tn]][cols], function(v) v[sample.int(length(v))]),
                                optional = TRUE)
      k <- row_keys(list(out[[tn]], rd, shuffled), cols)
      identical_rows <- mean(k[[1]] %in% k[[2]])
      by_chance <- mean(k[[3]] %in% k[[2]])

      qi <- intersect(cfg$quasi_ids, cols)
      if (!length(qi)) qi <- cols[vapply(t$variables[cols], function(s) s$type %in% c("categorical", "logical"), logical(1))]
      ru <- 0
      if (length(qi)) {
        cs <- group_index(coarsen_qi(out[[tn]][qi], cfg), qi)
        cr <- group_index(coarsen_qi(rd[qi], cfg), qi)
        lab_s <- do.call(paste, c(unname(lapply(cs$groups, as.character)), sep = "\u001f"))
        lab_r <- do.call(paste, c(unname(lapply(cr$groups, as.character)), sep = "\u001f"))
        uniq_s <- lab_s[tabulate(cs$index, cs$n) == 1]
        uniq_r <- lab_r[tabulate(cr$index, cr$n) == 1]
        ru <- length(intersect(uniq_s, uniq_r))
      }
      rows[[length(rows) + 1L]] <- data.frame(
        table = tn,
        compared = paste(cols, collapse = ", "),
        identical_rows = sprintf("%s %%", round(100 * identical_rows, 1)),
        by_chance = sprintf("%s %%", round(100 * by_chance, 1)),
        replicated_uniques = fmt_count(ru, cfg),
        uniques_on = paste(qi, collapse = ", "),
        .excess = identical_rows > 2 * by_chance + 0.01,
        .ru = ru,
        stringsAsFactors = FALSE
      )
    }
  }
  comparison <- if (length(rows)) do.call(rbind, rows) else NULL
  concern <- !is.null(comparison) && any(comparison$.excess | comparison$.ru > 0)
  if (!is.null(comparison)) comparison <- comparison[setdiff(names(comparison), c(".excess", ".ru"))]
  structure(list(
    fidelity = fidelity,
    method = if (fidelity == 3) "Gaussian copula from correlations rounded to one decimal" else
      "sequential classification and regression trees on protected real data",
    max_level = m$risk$max_level,
    too_small = m$risk$too_small,
    quasi = m$risk$quasi,
    comparison = comparison,
    concern = concern,
    stays_inside = fidelity == 4
  ), class = "mimic_copy_risk")
}

#' @export
print.mimic_copy_risk <- function(x, ...) {
  cli::cli_h2("Risk report for the level {x$fidelity} copy")
  cli::cli_text("Method: {x$method}.")
  if (x$fidelity > x$max_level || x$too_small) {
    cli::cli_alert_danger("The profile recommends at most level {x$max_level}.")
  }
  if (!is.null(x$quasi) && x$quasi$status == "fail") {
    cli::cli_alert_warning("The real data have rare combinations of quasi-identifiers: {x$quasi$message}")
  }
  if (is.null(x$comparison)) {
    cli::cli_alert_info("Not compared with the real data (give {.arg real_data} to check replicated rows).")
  } else {
    if (x$concern) {
      cli::cli_alert_warning("The copy reproduces real rows beyond chance, or combinations that are unique in the real data:")
    } else {
      cli::cli_alert_success("The copy matches real rows no more than chance, and reproduces no unique combinations.")
    }
    cli_table(x$comparison)
  }
  if (x$stays_inside) {
    cli::cli_alert_danger("A level 4 copy is built from the real data: treat it as output and check it before it leaves the secure environment.")
  }
  invisible(x)
}
