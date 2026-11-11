#' Safe output for the researcher's script
#'
#' Three functions for the researcher to use *in the script*, so the output
#' passes [inspect()] the first time: counts of persons with small cells
#' suppressed and the rest rounded, summaries of a numeric variable without
#' small or dominated groups, and a table of model coefficients instead of
#' the model object. They apply the same rules as [inspect()].
#'
#' @section Why they exist:
#' Output checkers send output back for the same few reasons again and
#' again. OpenSAFELY reports that about 30 % of rejected requests have
#' counts that are not rounded and 20 % have small counts that are not, or
#' not correctly, suppressed (<https://docs.opensafely.org/output-checking/>).
#' Suppression is easy to get wrong: a suppressed cell next to a total is
#' the total minus the other cells. [inspect()] finds these mistakes, but
#' only after the script has run on the real data. These functions do it
#' right in the script, and [trial_run()] shows the result on the copy.
#'
#' @section Which rules apply:
#' Without `config` the functions use the configuration that [trial_run()]
#' and [real_run()] were given: they place it in the script's session as the
#' option `mimic.config`. The script then follows the rules staff use,
#' without knowing them. Outside those functions the defaults of
#' [mimic_config()] apply.
#'
#' Where researchers work on a researcher machine in the secure
#' environment and run their scripts themselves, staff can set the option
#' for every session in the machine's `Rprofile.site`:
#'
#' ```r
#' options(mimic.config = mimic::mimic_config(min_count = 5))
#' ```
#'
#' With [describe_output()] the researcher also says what each file shows,
#' so the checker does not have to guess.
#'
#' @name safe_output
#' @keywords internal
NULL

# The rules for a helper in a script: the argument, else the configuration
# trial_run() or real_run() placed in the session, else the defaults.
script_config <- function(config) {
  as_config(config %||% getOption("mimic.config"))
}

#' Count persons with small cells suppressed
#'
#' Counts persons in every combination of `by`, suppresses cells with fewer
#' than `min_count` persons and rounds the other counts to `round_base`.
#' With `total = TRUE` it adds totals labelled `"Total"` and suppresses
#' further cells where needed, so no suppressed cell can be worked out from
#' a total and the cells next to it. The result is ready for
#' [utils::write.csv()].
#'
#' @section How cells are suppressed:
#' 1. **Primary:** a cell with between 1 and `min_count - 1` persons is
#'    suppressed. Empty combinations are not listed; a cell of 0 is not a
#'    disclosure.
#' 2. **Secondary** (with totals): in every row or column of the table that
#'    has a total, one suppressed cell alone would be the total minus the
#'    others. The smallest other cell in that line is then suppressed too,
#'    or the total when there is no other cell. This repeats until no line
#'    gives a cell back, and the result is confirmed with
#'    [check_suppression()].
#' 3. **Rounding:** the remaining counts, totals included, are rounded to
#'    `round_base`, after suppression, so a rounded count of 5 is never a
#'    rounded 3.
#'
#' Totals count distinct persons. When one person can be in several cells
#' (contacts by diagnosis), a total is smaller than the sum of its cells;
#' secondary suppression still treats the cells as adding up, which
#' suppresses more than needed but never too little.
#'
#' Suppressed cells are `NA`, which [inspect()] reads as suppressed.
#'
#' @param data A data frame, for example one of the tables in the script.
#' @param by Names of the columns to count by.
#' @param id Person identifier. Persons, not rows, are counted, so give it
#'   for a table with several rows per person (contacts, admissions).
#'   Without `id` each row is one person.
#' @param total Add totals: for one variable a `"Total"` row, for several the
#'   totals over each variable and their combinations.
#' @param config A [mimic_config()]; `NULL` uses the rules staff gave
#'   [trial_run()] or [real_run()], see [safe_output].
#' @return A data frame with the `by` columns (as text when `total = TRUE`)
#'   and `n`, the rounded number of persons, `NA` where suppressed. A
#'   message says how many cells were suppressed.
#' @family safe output
#' @export
#' @examples
#' safe_count(fake_persons, "sex")
#' safe_count(fake_persons, c("settlement", "sex"))
#'
#' # persons, not contacts
#' safe_count(fake_contacts, "contact_type", id = "person_id")
safe_count <- function(data, by, id = NULL, total = TRUE, config = NULL) {
  cfg <- script_config(config)
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame, not {.obj_type_friendly {data}}.")
  }
  if (!is.character(by) || !length(by)) cli::cli_abort("{.arg by} must name one or more columns.")
  check_columns(data, by, "by")
  if (!is.null(id)) check_columns(data, id, "id")
  if (!is.logical(total) || length(total) != 1 || is.na(total)) {
    cli::cli_abort("{.arg total} must be {.code TRUE} or {.code FALSE}.")
  }
  ids <- if (is.null(id)) NULL else data[[id]]

  # cells, in the order of the by columns (factor levels respected)
  gi <- group_index(data, by)
  cells <- gi$groups
  cells$.n <- persons_per(ids, gi$index, gi$n)
  cells <- cells[do.call(order, unname(as.list(cells[by]))), , drop = FALSE]

  if (total) {
    cells[by] <- lapply(cells[by], as.character)
    taken <- by[vapply(by, function(b) any(is_total(cells[[b]])), logical(1))]
    if (length(taken)) {
      cli::cli_abort(c("{.field {taken}} already {?has/have} a value that reads as a total ({.val Total}, {.val All}, ...).",
                       "i" = "Rename it, or use {.code total = FALSE}."))
    }
    margins <- list()
    # every non-empty set of by variables to total over; the full set last
    for (k in seq_along(by)) {
      for (over in utils::combn(by, k, simplify = FALSE)) {
        keep <- setdiff(by, over)
        mi <- group_index(data, keep)
        m <- mi$groups
        m[keep] <- lapply(m[keep], as.character)
        for (b in over) m[[b]] <- rep("Total", mi$n)
        m$.n <- persons_per(ids, mi$index, mi$n)
        if (length(keep)) m <- m[do.call(order, unname(as.list(mi$groups[keep]))), , drop = FALSE]
        margins[[length(margins) + 1L]] <- m[c(by, ".n")]
      }
    }
    cells <- rbind(cells[c(by, ".n")], do.call(rbind, margins))
  }
  rownames(cells) <- NULL

  n <- cells$.n
  primary <- n > 0 & n < cfg$min_count
  sup <- primary
  if (total) sup <- secondary_suppression(cells, by, sup)

  out <- cells[by]
  out$n <- cfg$round_base * round(n / cfg$round_base)
  out$n[sup] <- NA_real_

  if (total) {
    # the promise of this function: no suppressed cell comes back
    ch <- check_suppression(out, by = by, count = "n", config = cfg)
    if (ch$status != "pass") {
      cli::cli_abort("Internal error: suppression in {.fn safe_count} left a cell that can be recovered.")
    }
  }
  report_suppressed(sum(primary), sum(sup & !primary), cfg)
  out
}

# Suppress further cells until no line with a total gives a suppressed cell
# back. A line: all cells that share the values of every by column but one,
# plus the total of that column. `sup` marks the suppressed cells.
secondary_suppression <- function(cells, by, sup) {
  n <- cells$.n
  tot <- lapply(cells[by], is_total)
  repeat {
    changed <- FALSE
    for (b in by) {
      gi <- group_index(cells, setdiff(by, b))
      for (g in seq_len(gi$n)) {
        rows <- which(gi$index == g)
        t_row <- rows[tot[[b]][rows]]
        c_rows <- rows[!tot[[b]][rows]]
        if (length(t_row) != 1 || !length(c_rows)) next
        open <- sum(sup[c_rows])
        # one hidden cell next to a known total, or a hidden total next to
        # known cells: hide one more, the smallest cell, else the total
        if ((open == 1 && !sup[t_row]) || (open == 0 && sup[t_row])) {
          cand <- c_rows[!sup[c_rows] & n[c_rows] > 0]
          pick <- if (length(cand)) cand[which.min(n[cand])] else t_row
          if (!sup[pick]) {
            sup[pick] <- TRUE
            changed <- TRUE
          }
        }
      }
    }
    if (!changed) break
  }
  sup
}

report_suppressed <- function(k_primary, k_secondary, cfg) {
  if (!k_primary && !k_secondary) return(invisible())
  msg <- sprintf("Suppressed %d cell%s with fewer than %d persons", k_primary,
                 if (k_primary == 1) "" else "s", cfg$min_count)
  if (k_secondary) {
    msg <- sprintf("%s, and %d more so %s cannot be worked out from the totals", msg, k_secondary,
                   if (k_primary == 1) "it" else "they")
  }
  cli::cli_inform(paste0(msg, "."))
}

#' Summarise a numeric variable without small or dominated groups
#'
#' Gives the number of persons and chosen statistics of `value` for every
#' group of `by`. A group with fewer than `min_count` persons has all its
#' statistics suppressed. Means, sums and standard deviations are also
#' suppressed where one or two persons dominate the group by the rules in
#' [check_dominance()]: the mean income of a settlement where one person
#' earns most of the money is close to that person's income.
#'
#' Minimum and maximum are not offered: they almost always belong to one
#' person. Use the quartiles (`p25`, `p75`) or the median instead.
#'
#' Statistics are not rounded; the number of persons is rounded to
#' `round_base`. Column names are `mean_<value>`, `median_<value>` and so on,
#' which [inspect()] matches to the variable when it checks dominance on the
#' real data.
#'
#' @inheritParams safe_count
#' @param value Name of the numeric column to summarise. Missing values are
#'   left out, and persons are counted among the rows with a value.
#' @param by Names of the columns to group by; `NULL` for one row for all.
#' @param id Person identifier. Persons are counted, and for the dominance
#'   rules each person's values are added up first. Without `id` each row is
#'   one person.
#' @param stats Statistics to give: any of `"mean"`, `"sd"`, `"median"`,
#'   `"p25"`, `"p75"` and `"sum"`.
#' @return A data frame with the `by` columns, `n` (rounded number of
#'   persons, `NA` when the group is too small) and one column per statistic,
#'   `NA` where suppressed. A message says how many groups were suppressed.
#' @family safe output
#' @export
#' @examples
#' safe_summary(fake_persons, "income", by = "sex")
#' safe_summary(fake_persons, "income", by = "settlement", stats = c("median", "p25", "p75"))
safe_summary <- function(data, value, by = NULL, id = NULL,
                         stats = c("mean", "sd", "median"), config = NULL) {
  cfg <- script_config(config)
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame, not {.obj_type_friendly {data}}.")
  }
  if (!is.character(value) || length(value) != 1) cli::cli_abort("{.arg value} must name one column.")
  check_columns(data, value, "value")
  if (!is.numeric(data[[value]])) cli::cli_abort("{.arg value} must name a numeric column.")
  if (!is.null(by)) check_columns(data, by, "by")
  if (!is.null(id)) check_columns(data, id, "id")
  allowed <- c("mean", "sd", "median", "p25", "p75", "sum")
  if (!is.character(stats) || !length(stats)) cli::cli_abort("{.arg stats} must name one or more statistics.")
  extreme <- intersect(stats, c("min", "max", "range"))
  if (length(extreme)) {
    cli::cli_abort(c("{.val {extreme}} {?is/are} not offered: a minimum or maximum is almost always one person's value.",
                     "i" = "Use {.val p25}, {.val median} or {.val p75} instead."))
  }
  bad <- setdiff(stats, allowed)
  if (length(bad)) cli::cli_abort("{.arg stats} must be among {.val {allowed}}, not {.val {bad}}.")
  stats <- unique(stats)

  v <- data[[value]]
  ok <- !is.na(v)
  d <- data[ok, , drop = FALSE]
  v <- v[ok]
  ids <- if (is.null(id)) seq_len(nrow(d)) else d[[id]]
  gi <- group_index(d, by %||% character())
  persons <- persons_per(if (is.null(id)) NULL else ids, gi$index, gi$n)
  small <- persons < cfg$min_count

  out <- if (is.null(by)) data.frame(row.names = seq_len(max(gi$n, 1L))) else gi$groups
  if (!gi$n) out <- out[0, , drop = FALSE]
  out$n <- cfg$round_base * round(persons / cfg$round_base)
  out$n[small] <- NA_real_

  funs <- list(mean = mean, sd = stats::sd, median = stats::median,
               p25 = function(x) stats::quantile(x, 0.25, names = FALSE),
               p75 = function(x) stats::quantile(x, 0.75, names = FALSE), sum = sum)
  # the rows of each group, found once rather than by scanning all rows per
  # group and statistic
  rows <- split(seq_along(v), factor(gi$index, levels = seq_len(gi$n)))
  dominated <- logical(gi$n)
  if (any(c("mean", "sd", "sum") %in% stats)) {
    dominated <- vapply(rows, function(r) nzchar(dominance_broken(v[r], ids[r], cfg)), logical(1),
                        USE.NAMES = FALSE)
  }
  for (s in stats) {
    val <- vapply(rows, function(r) funs[[s]](v[r]), numeric(1), USE.NAMES = FALSE)
    val[small] <- NA_real_
    if (s %in% c("mean", "sd", "sum")) val[dominated] <- NA_real_
    out[[paste0(s, "_", value)]] <- val
  }
  if (!is.null(by)) out <- out[do.call(order, unname(as.list(out[by]))), , drop = FALSE]
  rownames(out) <- NULL

  k_small <- sum(small)
  k_dom <- sum(dominated & !small)
  if (k_small || k_dom) {
    parts <- c(if (k_small) sprintf("%d group%s with fewer than %d persons", k_small,
                                    if (k_small == 1) "" else "s", cfg$min_count),
               if (k_dom) sprintf("means, sums and standard deviations in %d group%s dominated by few persons",
                                  k_dom, if (k_dom == 1) "" else "s"))
    cli::cli_inform(paste0("Suppressed ", paste(parts, collapse = "; "), "."))
  }
  out
}

#' A table of model coefficients that is safe to release
#'
#' A fitted model object carries its data: an `lm` or `glm` object holds
#' the model frame, residuals and fitted values of every person, so it is
#' never released. `safe_model()` returns the coefficient table instead,
#' after checking with [check_model()] that the model rests on enough
#' observations: residual degrees of freedom of at least `min_df`, every
#' factor level and, for a binary outcome, both outcome classes with at
#' least `min_count` observations.
#'
#' The function stops when a rule is broken, so nothing is written that
#' would be sent back. A binary outcome with a small cell within a factor
#' level gives a warning: the coefficient is then estimated from very few
#' persons, and the checker will look at it.
#'
#' @param fit A fitted model with a coefficient table in `summary(fit)`,
#'   for example from [stats::lm()], [stats::glm()] or `survival::coxph()`.
#' @inheritParams safe_count
#' @return A data frame with one row per coefficient: `term`, the columns of
#'   the coefficient table (`estimate`, `std_error`, `statistic`, `p_value`
#'   for `lm` and `glm`), `observations` (rounded number of observations)
#'   and `df_residual`.
#' @family safe output
#' @export
#' @examples
#' fit <- lm(income ~ sex + occupation, data = fake_persons)
#' safe_model(fit)
safe_model <- function(fit, config = NULL) {
  cfg <- script_config(config)
  ch <- check_model(fit, config = cfg)
  issues <- ch$details
  if (!is.null(issues)) {
    # the coefficient table leaves the row-level parts behind
    issues <- issues[!grepl("row-level data", issues$issue, fixed = TRUE), , drop = FALSE]
  }
  if (ch$status == "warn" && is.null(ch$details)) {
    cli::cli_abort(c("Cannot check a model of class {.cls {class(fit)[1]}} automatically.",
                     "i" = "Write the coefficients yourself; the checker will review them by hand."))
  }
  if (!is.null(issues) && any(issues$severity == "fail")) {
    cli::cli_abort(c("The model rests on too few observations to be released:",
                     stats::setNames(cli_escape(issues$issue[issues$severity == "fail"]), rep("x", sum(issues$severity == "fail")))))
  }
  if (!is.null(issues) && nrow(issues)) {
    cli::cli_warn(c("The checker will look closely at this model:",
                    stats::setNames(cli_escape(issues$issue), rep("!", nrow(issues)))))
  }

  co <- tryCatch(stats::coef(summary(fit)), error = function(e) NULL)
  if (!is.matrix(co) || !is.numeric(co) || is.null(rownames(co))) {
    cli::cli_abort(c("Found no coefficient table in {.code summary(fit)} for a model of class {.cls {class(fit)[1]}}.",
                     "i" = "Write the coefficients yourself; the checker will review them by hand."))
  }
  out <- data.frame(term = rownames(co), stringsAsFactors = FALSE)
  for (j in seq_len(ncol(co))) out[[coef_name(colnames(co)[j])]] <- unname(co[, j])
  n_obs <- tryCatch(stats::nobs(fit), error = function(e) NA_real_)
  out$observations <- cfg$round_base * round(n_obs / cfg$round_base)
  out$df_residual <- tryCatch(stats::df.residual(fit), error = function(e) NA_real_) %||% NA_real_
  rownames(out) <- NULL
  out
}

# Plain column names for a coefficient table.
coef_name <- function(x) {
  known <- c("Estimate" = "estimate", "Std. Error" = "std_error", "t value" = "statistic",
             "z value" = "statistic", "Pr(>|t|)" = "p_value", "Pr(>|z|)" = "p_value")
  if (x %in% names(known)) return(unname(known[x]))
  x <- gsub("[^a-z0-9]+", "_", tolower(x))
  gsub("^_|_$", "", x)
}
