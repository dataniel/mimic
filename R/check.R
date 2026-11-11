#' Disclosure and data checks
#'
#' The `check_*()` functions are the building blocks of output control.
#' Each tests one rule and returns a `mimic_check` object with the same
#' fields, so results can be printed one by one or collected with
#' [check_all()]. [profile_data()] and `inspect()` are built on them.
#'
#' | Function | Rule |
#' |---|---|
#' | [check_min_count()] | every cell or group has at least `min_count` persons |
#' | [check_k_anonymity()] | combinations of quasi-identifiers are shared by at least `min_count` persons |
#' | [check_dominance()] | no few contributors dominate a sum ((n, k) rule and p % rule) |
#' | [check_extremes()] | no minimum, maximum or other single-person values |
#' | [check_model()] | models have enough observations and degrees of freedom |
#' | [check_differencing()] | two tables cannot be subtracted to reveal a small group |
#' | [check_suppression()] | suppressed cells cannot be recovered from totals |
#' | [check_group_disclosure()] | no group shares one value of a variable |
#' | [check_keys()] | keys are complete and linked tables fit together |
#'
#' @section What a check reveals:
#' A check never prints counts below `min_count`; counts are rounded and
#' small ones are shown as "fewer than 5". It does show *which* groups fail,
#' using the labels in the table being checked, because the person approving
#' output needs that to ask for a fix. Results from [check_k_anonymity()]
#' only contain totals, never the rare combinations themselves. Results
#' computed on real data belong in the secure environment unless reviewed.
#'
#' @section Result fields:
#' * `rule`: name of the rule.
#' * `target`: what was checked.
#' * `status`: `"pass"`, `"warn"` or `"fail"`.
#' * `message`: one sentence for a human.
#' * `details`: a data frame with failing groups or issues, or `NULL`.
#'
#' @name checks
#' @keywords internal
NULL

new_check <- function(rule, target, status, message, details = NULL) {
  stopifnot(status %in% c("pass", "warn", "fail"))
  if (!is.null(details)) rownames(details) <- NULL
  structure(list(rule = rule, target = target, status = status,
                 message = message, details = details),
            class = "mimic_check")
}

#' @export
print.mimic_check <- function(x, ..., max_rows = 10) {
  line <- "{x$rule} ({x$target}): {x$message}"
  switch(x$status,
         pass = cli::cli_alert_success(line),
         warn = cli::cli_alert_warning(line),
         fail = cli::cli_alert_danger(line))
  d <- x$details
  if (!is.null(d) && nrow(d) > 0 && x$status != "pass") {
    cli_table(d, max_rows)
  }
  invisible(x)
}

#' Collect checks into one report
#'
#' @param ... `mimic_check` objects or lists of them.
#' @return A data frame of class `mimic_report` with one row per check
#'   (`rule`, `target`, `status`, `message`). The full checks are kept in the
#'   attribute `"checks"`.
#' @export
#' @examples
#' check_all(
#'   check_min_count(fake_persons, by = "settlement", id = "person_id"),
#'   check_keys(list(persons = fake_persons, contacts = fake_contacts),
#'              keys = "person_id")
#' )
check_all <- function(...) {
  flat <- list()
  add <- function(x) {
    if (inherits(x, "mimic_check")) {
      flat[[length(flat) + 1L]] <<- x
    } else if (is.list(x)) {
      for (el in x) add(el)
    } else {
      cli::cli_abort("{.fn check_all} takes {.cls mimic_check} objects or lists of them.")
    }
  }
  for (el in list(...)) add(el)

  out <- data.frame(
    rule = vapply(flat, `[[`, character(1), "rule"),
    target = vapply(flat, `[[`, character(1), "target"),
    status = vapply(flat, `[[`, character(1), "status"),
    message = vapply(flat, `[[`, character(1), "message"),
    stringsAsFactors = FALSE
  )
  attr(out, "checks") <- flat
  class(out) <- c("mimic_report", "data.frame")
  out
}

#' @export
print.mimic_report <- function(x, ...) {
  checks <- attr(x, "checks")
  for (ch in checks) print(ch)
  st <- x$status
  cli::cli_rule()
  cli::cli_text("{sum(st == 'pass')} passed, {sum(st == 'warn')} warning{?s}, {sum(st == 'fail')} failed.")
  invisible(x)
}

count_names <- c("n", "N", "count", "Count", "freq", "Freq", "frequency", "antal")

# Find the count column of an aggregated table, or NULL.
guess_count <- function(df) {
  hit <- intersect(count_names, names(df))
  hit <- hit[vapply(df[hit], is.numeric, logical(1))]
  if (length(hit)) hit[1] else NULL
}

as_check_df <- function(x, arg = "x") {
  if (inherits(x, "table")) return(as.data.frame(x, stringsAsFactors = FALSE))
  if (is.matrix(x)) return(as.data.frame(as.table(x), stringsAsFactors = FALSE))
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg {arg}} must be a data frame or a table, not {.obj_type_friendly {x}}.")
  }
  as.data.frame(x, stringsAsFactors = FALSE)
}

#' Check that every cell has enough persons
#'
#' Works on microdata (one row per person or observation, give `by`) and on
#' aggregated tables (one row per cell, with a count column). Empty cells
#' (0) are allowed; cells with between 1 and `min_count - 1` persons fail.
#'
#' @param x A data frame, or a `table`/matrix of counts.
#' @param by Grouping variables. For an aggregated table the default is all
#'   non-numeric columns.
#' @param count Name of the count column in an aggregated table. Detected
#'   automatically when a column is called `n`, `count`, `freq` or similar.
#'   Leave `NULL` with `by` for microdata.
#' @param id Person identifier in microdata. Persons, not rows, are counted.
#'   Without `id` each row is one person.
#' @param config A [mimic_config()]; `NULL` uses the defaults.
#' @param target Label used in reports.
#' @return A `mimic_check`. `details` lists the failing groups by label,
#'   never their counts.
#' @family checks
#' @export
#' @examples
#' check_min_count(fake_persons, by = "settlement", id = "person_id")
#'
#' tab <- as.data.frame(table(sex = fake_persons$sex))
#' check_min_count(tab)
check_min_count <- function(x, by = NULL, count = NULL, id = NULL,
                            config = NULL, target = NULL) {
  target <- target %||% label_of(substitute(x))
  cfg <- as_config(config)
  df <- as_check_df(x)
  aggregated <- !is.null(count) || (is.null(by) && !is.null(guess_count(df))) ||
    inherits(x, c("table", "matrix"))

  if (aggregated) {
    count <- count %||% guess_count(df)
    if (is.null(count)) {
      cli::cli_abort("Could not find a count column; give {.arg count}.")
    }
    check_columns(df, count, "count")
    if (is.null(by)) by <- names(df)[!vapply(df, is.numeric, logical(1))]
    check_columns(df, by, "by")
    gi <- group_index(df, by)
    n <- as.numeric(tapply(df[[count]], factor(gi$index, levels = seq_len(gi$n)), sum, na.rm = TRUE))
    n[is.na(n)] <- 0
  } else {
    if (is.null(by)) {
      cli::cli_abort(c("Give {.arg by} for microdata or {.arg count} for an aggregated table.",
                       "i" = "No count column was found in {.arg x}."))
    }
    check_columns(df, by, "by")
    if (!is.null(id)) check_columns(df, id, "id")
    gi <- group_index(df, by)
    n <- persons_per(if (is.null(id)) NULL else df[[id]], gi$index, gi$n)
  }

  fail <- n > 0 & n < cfg$min_count
  k <- sum(fail)
  if (k == 0) {
    new_check("min_count", target, "pass",
              sprintf("All %d cells have 0 or at least %d persons.", length(n), cfg$min_count))
  } else {
    new_check("min_count", target, "fail",
              sprintf("%d of %d cells have fewer than %d persons.", k, length(n), cfg$min_count),
              details = gi$groups[fail, , drop = FALSE])
  }
}

# Coarsen quasi-identifiers to the granularity at which they are compared.
coarsen_qi <- function(df, cfg) {
  df[] <- lapply(df, function(v) {
    if (inherits(v, c("Date", "POSIXt"))) return(date_period(as.Date(v), cfg$date_unit))
    if (is.numeric(v) && length(unique(v[!is.na(v)])) > 20) {
      br <- unique(stats::quantile(v, seq(0, 1, 0.1), na.rm = TRUE, names = FALSE))
      return(paste0("decile_", findInterval(v, br, rightmost.closed = TRUE)))
    }
    as.character(v)
  })
  df
}

#' Check combinations of quasi-identifiers (k-anonymity)
#'
#' Counts how many persons share each combination of the quasi-identifiers.
#' Persons in a combination shared by fewer than `min_count` persons are at
#' risk of re-identification. Dates are compared at `date_unit`
#' granularity and numeric variables with many values in deciles; group
#' them yourself if you need another granularity.
#'
#' The result also shows how many persons would be at risk if each
#' quasi-identifier were dropped, which points to the variable to coarsen.
#'
#' @inheritParams check_min_count
#' @param quasi_ids Quasi-identifiers. Defaults to `config$quasi_ids`.
#' @return A `mimic_check`. `details` contains totals only, never the
#'   rare combinations.
#' @family checks
#' @export
#' @examples
#' check_k_anonymity(fake_persons, c("birth_date", "sex", "settlement"),
#'                   id = "person_id")
check_k_anonymity <- function(x, quasi_ids = NULL, id = NULL, config = NULL,
                              target = NULL) {
  target <- target %||% label_of(substitute(x))
  cfg <- as_config(config)
  df <- as_check_df(x)
  qi <- quasi_ids %||% cfg$quasi_ids
  if (!length(qi)) {
    cli::cli_abort("No quasi-identifiers given. Use {.arg quasi_ids} or {.code mimic_config(quasi_ids = )}.")
  }
  check_columns(df, qi, "quasi_ids")
  if (!is.null(id)) check_columns(df, id, "id")
  ids <- if (is.null(id)) seq_len(nrow(df)) else df[[id]]
  cq <- coarsen_qi(df[qi], cfg)

  at_risk <- function(vars) {
    gi <- group_index(cq, vars)
    p <- persons_per(ids, gi$index, gi$n)
    rare <- p < cfg$min_count
    c(combos = gi$n, rare = sum(rare),
      persons = length(unique(ids[gi$index %in% which(rare)])))
  }

  all_q <- at_risk(qi)
  n_persons <- length(unique(ids))
  rows <- list(c(variables = paste(qi, collapse = " + "), all_q))
  if (length(qi) > 1) {
    for (q in qi) {
      rows[[length(rows) + 1L]] <- c(variables = paste("without", q), at_risk(setdiff(qi, q)))
    }
  }
  details <- data.frame(
    variables = vapply(rows, `[[`, character(1), "variables"),
    # numbers of combinations are not counts of persons and need no rounding
    combinations = format(as.numeric(vapply(rows, `[[`, character(1), "combos")), big.mark = ",", trim = TRUE),
    rare_combinations = format(as.numeric(vapply(rows, `[[`, character(1), "rare")), big.mark = ",", trim = TRUE),
    persons_at_risk = fmt_count(as.numeric(vapply(rows, `[[`, character(1), "persons")), cfg),
    stringsAsFactors = FALSE
  )
  k <- all_q[["persons"]]
  if (k == 0) {
    new_check("k_anonymity", target, "pass",
              sprintf("Every combination of %s is shared by at least %d persons.",
                      paste(qi, collapse = ", "), cfg$min_count),
              details = details)
  } else {
    share <- round(100 * protected_share(k, n_persons, cfg), 1)
    new_check("k_anonymity", target, "fail",
              sprintf("%s persons (about %s %%) are in combinations of %s shared by fewer than %d persons.",
                      fmt_count(k, cfg), share, paste(qi, collapse = ", "), cfg$min_count),
              details = details)
  }
}

#' Check that no few contributors dominate a sum
#'
#' Applies two rules from the Eurostat guidelines for output checking to
#' every group:
#' * **(n, k) rule:** the `n` largest contributors may not make up more than
#'   `k` of the total (default: 2 contributors, 85 %).
#' * **p % rule:** the total minus the two largest contributions must be at
#'   least `p` of the largest, so the second largest contributor cannot
#'   estimate the largest within `p`. Groups with fewer than 3 contributors
#'   always fail this rule.
#'
#' Use it before releasing sums, means or totals of skewed variables such as
#' income, costs or catch.
#'
#' @inheritParams check_min_count
#' @param value Numeric variable that is summed.
#' @param id Contributor identifier. Contributions are summed per
#'   contributor first. Without `id` each row is one contributor.
#' @return A `mimic_check`. `details` lists failing groups and the rule
#'   they break.
#' @family checks
#' @export
#' @examples
#' check_dominance(fake_persons, value = "income", by = "settlement",
#'                 id = "person_id")
check_dominance <- function(x, value, by = NULL, id = NULL, config = NULL,
                            target = NULL) {
  target <- target %||% label_of(substitute(x))
  cfg <- as_config(config)
  df <- as_check_df(x)
  check_columns(df, value, "value")
  if (!is.numeric(df[[value]])) cli::cli_abort("{.arg value} must name a numeric column.")
  if (!is.null(by)) check_columns(df, by, "by")
  if (!is.null(id)) check_columns(df, id, "id")

  gi <- group_index(df, by %||% character())
  ids <- if (is.null(id)) seq_len(nrow(df)) else df[[id]]
  v <- df[[value]]
  ok <- !is.na(v)

  # the rows of each group, found once rather than by scanning all rows per group
  rows <- split(which(ok), factor(gi$index[ok], levels = seq_len(gi$n)))
  rule_broken <- vapply(rows, function(r) dominance_broken(v[r], ids[r], cfg), character(1),
                        USE.NAMES = FALSE)

  fail <- nzchar(rule_broken)
  rule_txt <- sprintf("(n = %s, k = %s) and p = %s", cfg$dominance$n, cfg$dominance$k, cfg$p_percent)
  if (!any(fail)) {
    new_check("dominance", target, "pass",
              sprintf("No group of %s is dominated by few contributors %s.", value, rule_txt))
  } else {
    details <- gi$groups[fail, , drop = FALSE]
    details$rule_broken <- rule_broken[fail]
    new_check("dominance", target, "fail",
              sprintf("%d of %d groups of %s are dominated by few contributors %s.",
                      sum(fail), gi$n, value, rule_txt),
              details = details)
  }
}

# Which dominance rule one group breaks: "" when none. Values are added up
# per contributor first.
dominance_broken <- function(v, ids, cfg) {
  ok <- !is.na(v)
  contr <- abs(as.numeric(rowsum(v[ok], ids[ok], reorder = FALSE)))
  contr <- sort(contr[contr > 0], decreasing = TRUE)
  total <- sum(contr)
  if (total == 0) return("")
  nk <- sum(utils::head(contr, cfg$dominance$n)) / total > cfg$dominance$k
  pp <- length(contr) < 3 || (total - contr[1] - contr[2]) < cfg$p_percent * contr[1]
  if (nk && pp) "(n, k) and p %" else if (nk) "(n, k)" else if (pp) "p %" else ""
}

extreme_pattern <- "(^|[^a-z])(min|max|minimum|maximum|range|lowest|highest|smallest|largest)([^a-z]|$)"

#' Check for minimum, maximum and other single-person values
#'
#' Minimum and maximum values almost always belong to one person. This
#' check looks for them by name: column names, row names, names of a vector
#' and character cells such as `"Max."` in a statistics column (for example
#' output from [summary()]).
#'
#' @inheritParams check_min_count
#' @param x A data frame, table, matrix or named vector.
#' @return A `mimic_check` listing where extremes were found.
#' @family checks
#' @export
#' @examples
#' check_extremes(summary(fake_persons$income))
#' check_extremes(data.frame(group = "a", mean = 1, max_income = 2))
check_extremes <- function(x, config = NULL, target = NULL) {
  target <- target %||% label_of(substitute(x))
  hit <- function(s) !is.na(s) & grepl(extreme_pattern, tolower(s))
  found <- data.frame(location = character(), name = character(), stringsAsFactors = FALSE)
  add <- function(loc, nm) {
    if (length(nm)) found <<- rbind(found, data.frame(location = loc, name = nm, stringsAsFactors = FALSE))
  }

  if (is.data.frame(x) || is.matrix(x)) {
    cn <- colnames(x)
    add("column", cn[hit(cn)])
    rn <- rownames(x)
    if (!is.null(rn) && !identical(rn, as.character(seq_len(nrow(x))))) add("row name", rn[hit(rn)])
    if (is.data.frame(x)) {
      for (col in names(x)[vapply(x, function(v) is.character(v) || is.factor(v), logical(1))]) {
        cells <- unique(as.character(x[[col]]))
        add(paste0("value in ", col), cells[hit(cells)])
      }
    }
  } else if (!is.null(names(x))) {
    add("name", names(x)[hit(names(x))])
  } else if (!is.null(dimnames(x))) {
    for (d in dimnames(x)) add("label", d[hit(d)])
  }

  if (!nrow(found)) {
    new_check("extremes", target, "pass", "No minimum, maximum or range values found.")
  } else {
    new_check("extremes", target, "fail",
              sprintf("Found %d minimum, maximum or range value(s). These are often one person's value.",
                      nrow(found)),
              details = found)
  }
}

#' Check that a model rests on enough observations
#'
#' A model fitted to very few observations can reveal individual values:
#' with zero residual degrees of freedom the fitted values *are* the data.
#' And a fitted model *object* carries its data: `lm` and `glm` objects hold
#' the model frame, residuals and fitted values for every person, and so
#' does `summary.lm()`. Release a table of coefficients instead.
#' The check fails when
#' * the object contains row-level components (`model`, `residuals`,
#'   `fitted.values`, `y`, `qr`, ...),
#' * residual degrees of freedom are below `config$min_df`,
#' * a factor level in the model data has fewer than `min_count`
#'   observations,
#' * for a binary outcome, an outcome class has fewer than `min_count`
#'   observations.
#'
#' It warns when a binary outcome has a cell with fewer than `min_count`
#' observations within a factor level. Objects without [stats::nobs()],
#' [stats::df.residual()] or [stats::model.frame()] methods give a warning
#' so they are reviewed by hand.
#'
#' @inheritParams check_min_count
#' @param fit A fitted model, for example from [stats::lm()] or [stats::glm()].
#' @return A `mimic_check`.
#' @family checks
#' @export
#' @examples
#' # a fitted lm object holds the data: not for release
#' fit <- lm(income ~ sex + occupation, data = fake_persons)
#' check_model(fit)
#'
#' # the observations behind it are fine once the row-level parts are removed
#' fit[c("model", "residuals", "fitted.values", "effects", "qr", "na.action")] <- NULL
#' check_model(fit)
check_model <- function(fit, config = NULL, target = NULL) {
  target <- target %||% label_of(substitute(fit))
  cfg <- as_config(config)
  safe <- function(expr) tryCatch(expr, error = function(e) NULL)
  n_obs <- safe(stats::nobs(fit))
  df_res <- safe(stats::df.residual(fit))
  mf <- safe(stats::model.frame(fit))

  if (is.null(n_obs) && is.null(df_res) && is.null(mf)) {
    return(new_check("model", target, "warn",
                     sprintf("Cannot check objects of class %s automatically; review by hand.",
                             class(fit)[1])))
  }

  issues <- character()
  severity <- character()
  note <- function(txt, sev) {
    issues <<- c(issues, txt)
    severity <<- c(severity, sev)
  }
  # A fitted model object carries the data it was fitted on
  row_parts <- c("model", "residuals", "fitted.values", "y", "x", "data", "linear.predictors",
                 "weights", "prior.weights", "working.weights", "effects", "qr", "na.action")
  if (is.list(fit)) {
    present <- intersect(row_parts, names(fit))
    present <- present[!vapply(fit[present], is.null, logical(1))]
    if (length(present)) {
      note(sprintf("the object contains row-level data (%s); release a table of coefficients instead",
                   paste(present, collapse = ", ")), "fail")
    }
  }
  if (!is.null(df_res) && !is.na(df_res) && df_res < cfg$min_df) {
    note(sprintf("residual degrees of freedom (%s) below %d", df_res, cfg$min_df), "fail")
  }
  if (!is.null(mf)) {
    y <- stats::model.response(mf)
    preds <- mf[-1]
    for (nm in names(preds)) {
      v <- preds[[nm]]
      if (is.factor(v) || is.character(v) || is.logical(v)) {
        tab <- table(v)
        if (any(tab > 0 & tab < cfg$min_count)) {
          note(sprintf("a level of %s has fewer than %d observations", nm, cfg$min_count), "fail")
        }
      }
    }
    if (!is.null(y) && is.null(dim(y)) && length(unique(y[!is.na(y)])) == 2) {
      ty <- table(y)
      if (any(ty < cfg$min_count)) {
        note(sprintf("an outcome class has fewer than %d observations", cfg$min_count), "fail")
      }
      for (nm in names(preds)) {
        v <- preds[[nm]]
        if (is.factor(v) || is.character(v) || is.logical(v)) {
          tab <- table(v, y)
          if (any(tab > 0 & tab < cfg$min_count)) {
            note(sprintf("outcome by %s has a cell with fewer than %d observations", nm, cfg$min_count), "warn")
          }
        }
      }
    }
  }

  if (!length(issues)) {
    new_check("model", target, "pass",
              sprintf("Model rests on enough observations (residual df at least %d).", cfg$min_df))
  } else {
    status <- if (any(severity == "fail")) "fail" else "warn"
    new_check("model", target, status,
              sprintf("%d issue(s) with the observations behind the model.", length(issues)),
              details = data.frame(issue = issues, severity = severity, stringsAsFactors = FALSE))
  }
}

#' Check that two tables cannot be subtracted
#'
#' A differencing attack subtracts two released tables to reveal a small
#' group: a table for all persons and one for all persons except one
#' settlement differ by exactly that settlement. The check compares counts
#' cell by cell on the shared grouping variables and also compares the
#' grand totals. It fails if any difference is between 1 and
#' `min_count - 1`.
#'
#' @inheritParams check_min_count
#' @param x,y Aggregated tables (data frames with a count column, or tables).
#' @param by Grouping variables shared by both tables. Defaults to the
#'   non-numeric columns they have in common.
#' @param count Name of the count column, the same in both tables. Detected
#'   automatically when possible.
#' @return A `mimic_check`. `details` lists the groups whose difference
#'   is small.
#' @family checks
#' @export
#' @examples
#' all <- as.data.frame(table(sex = fake_persons$sex))
#' most <- as.data.frame(table(sex = fake_persons$sex[fake_persons$settlement != "Settlement_20"]))
#' check_differencing(all, most)
check_differencing <- function(x, y, by = NULL, count = NULL, config = NULL,
                               target = NULL) {
  target <- target %||% paste(label_of(substitute(x)), "vs", label_of(substitute(y)))
  cfg <- as_config(config)
  dx <- as_check_df(x, "x")
  dy <- as_check_df(y, "y")
  cx <- count %||% guess_count(dx)
  cy <- count %||% guess_count(dy)
  if (is.null(cx) || is.null(cy)) cli::cli_abort("Could not find a count column in both tables; give {.arg count}.")
  check_columns(dx, cx, "count")
  check_columns(dy, cy, "count")
  if (is.null(by)) {
    nonnum <- function(d) names(d)[!vapply(d, is.numeric, logical(1))]
    by <- intersect(nonnum(dx), nonnum(dy))
  }
  check_columns(dx, by, "by")
  check_columns(dy, by, "by")

  small <- function(d) d > 0 & d < cfg$min_count
  total_diff <- abs(sum(dx[[cx]], na.rm = TRUE) - sum(dy[[cy]], na.rm = TRUE))

  details <- NULL
  n_fail <- 0L
  if (length(by)) {
    both <- rbind(
      data.frame(dx[by], .n = dx[[cx]], .src = 1L, stringsAsFactors = FALSE),
      data.frame(dy[by], .n = dy[[cy]], .src = 2L, stringsAsFactors = FALSE)
    )
    gi <- group_index(both, by)
    f <- factor(gi$index, levels = seq_len(gi$n))
    nx <- tapply(ifelse(both$.src == 1L, both$.n, 0), f, sum, na.rm = TRUE)
    ny <- tapply(ifelse(both$.src == 2L, both$.n, 0), f, sum, na.rm = TRUE)
    d <- abs(as.numeric(nx) - as.numeric(ny))
    fail <- small(d)
    n_fail <- sum(fail)
    if (n_fail) details <- gi$groups[fail, , drop = FALSE]
  }
  total_fail <- small(total_diff)

  if (!n_fail && !total_fail) {
    new_check("differencing", target, "pass",
              sprintf("No difference between the tables is between 1 and %d.", cfg$min_count - 1))
  } else {
    parts <- c(if (n_fail) sprintf("%d cell(s)", n_fail), if (total_fail) "the grand totals")
    new_check("differencing", target, "fail",
              sprintf("Subtracting the tables reveals fewer than %d persons in %s.",
                      cfg$min_count, paste(parts, collapse = " and ")),
              details = details)
  }
}

#' Check keys of linked tables
#'
#' Data quality check before [profile_data()]: are key columns present and
#' complete, is the main table unique on its key, and do the other tables
#' only refer to persons in the main table? Persons who are missing from a
#' linked table (no contacts, no admissions) are normal and not reported.
#'
#' @param tables A named list of data frames.
#' @param keys Key column. Either one name used in every table, or a named
#'   character vector mapping table names to key columns, for example
#'   `c(persons = "person_id", contacts = "pid")`.
#' @param main Name of the table with one row per person. By default the
#'   first table whose key is unique.
#' @param config A [mimic_config()]; `NULL` uses the defaults.
#' @param target Label used in reports.
#' @return A `mimic_check`. Missing keys and duplicates in the main table
#'   fail; rows that refer to unknown persons give a warning.
#' @family checks
#' @export
#' @examples
#' check_keys(list(persons = fake_persons, contacts = fake_contacts,
#'                 admissions = fake_admissions),
#'            keys = "person_id")
check_keys <- function(tables, keys, main = NULL, config = NULL, target = "keys") {
  cfg <- as_config(config)
  tables <- as_table_list(tables)
  k <- resolve_keys(tables, keys, main)

  issues <- list()
  note <- function(tbl, txt, n, sev) {
    issues[[length(issues) + 1L]] <<- data.frame(table = tbl, issue = txt,
                                                 rows = fmt_count(n, cfg), severity = sev,
                                                 stringsAsFactors = FALSE)
  }
  main_keys <- if (!is.na(k$main)) as.character(tables[[k$main]][[k$key[[k$main]]]]) else NULL

  for (tn in names(tables)) {
    key <- k$key[[tn]]
    if (is.na(key)) next
    v <- as.character(tables[[tn]][[key]])
    n_na <- sum(is.na(v))
    if (n_na) note(tn, "missing key values", n_na, "fail")
    if (identical(tn, k$main)) {
      n_dup <- sum(duplicated(v[!is.na(v)]))
      if (n_dup) note(tn, "duplicated keys in the main table", n_dup, "fail")
    } else if (!is.null(main_keys)) {
      n_orph <- sum(!is.na(v) & !v %in% main_keys)
      if (n_orph) note(tn, "keys not found in the main table", n_orph, "warn")
    }
  }
  if (is.na(k$main) && sum(!is.na(k$key)) > 1) {
    note("(all)", "no main table with a unique key; links cannot be checked", 0, "warn")
  }

  if (!length(issues)) {
    return(new_check("keys", target, "pass",
                     if (is.na(k$main)) "Keys are complete." else
                       sprintf("Keys are complete and all linked tables refer to persons in '%s'.", k$main)))
  }
  details <- do.call(rbind, issues)
  status <- if (any(details$severity == "fail")) "fail" else "warn"
  new_check("keys", target, status,
            sprintf("%d key issue(s) found.", nrow(details)), details = details)
}

# --- tables with suppressed cells and totals -----------------------------------

# Labels that mark a total row or column: "Total", "All", "I alt", ...
total_labels <- c("total", "totals", "all", "overall", "sum", "i alt", "alle", "tilsammen")
is_total <- function(v) !is.na(v) & tolower(trimws(as.character(v))) %in% total_labels

# How suppressed cells are written: "<5", ".", "..", "x", "*" or empty.
suppression_marker <- "^\\s*(<\\s*[0-9]+|\\.{1,3}|[xX*]|NA|-{2})?\\s*$"

# A count column with suppression markers read as text becomes numeric, with
# the suppressed cells as NA. Other columns are returned unchanged.
as_suppressed_numeric <- function(v) {
  if (is.numeric(v) || !(is.character(v) || is.factor(v))) return(v)
  s <- as.character(v)
  marker <- is.na(s) | grepl(suppression_marker, s)
  num <- suppressWarnings(as.numeric(gsub(" ", "", s, fixed = TRUE)))
  if (!any(marker & !is.na(s)) || any(!marker & is.na(num)) || all(marker)) return(v)
  ifelse(marker, NA_real_, num)
}

# Cells of a table: one row per combination of `by` with its count in
# `.n`, NA when suppressed. Microdata (no `count`) are counted in persons.
table_cells <- function(df, by, count, id = NULL) {
  if (!is.null(count)) {
    check_columns(df, count, "count")
    if (is.null(by)) by <- setdiff(names(df)[!vapply(df, is.numeric, logical(1))], count)
    check_columns(df, by, "by")
    gi <- group_index(df, by)
    v <- as_suppressed_numeric(df[[count]])
    # a cell with any suppressed part is unknown
    n <- as.numeric(tapply(v, factor(gi$index, levels = seq_len(gi$n)),
                           function(z) if (anyNA(z)) NA_real_ else sum(z)))
  } else {
    if (is.null(by)) cli::cli_abort("Give {.arg by} for microdata or {.arg count} for an aggregated table.")
    check_columns(df, by, "by")
    if (!is.null(id)) check_columns(df, id, "id")
    gi <- group_index(df, by)
    n <- persons_per(if (is.null(id)) NULL else df[[id]], gi$index, gi$n)
  }
  cells <- gi$groups
  cells$.n <- n
  list(cells = cells, by = by)
}

# "settlement = A, sex = F" for the rows of `groups`.
group_labels <- function(groups) {
  if (!ncol(groups)) return(rep("(all)", nrow(groups)))
  parts <- lapply(names(groups), function(v) paste(v, "=", as.character(groups[[v]])))
  do.call(paste, c(parts, sep = ", "))
}

#' Check that suppressed cells cannot be recovered
#'
#' Suppressing a small cell only helps if it cannot be worked out from the
#' rest of the table. When a row or column has a total and only one
#' suppressed cell, the cell is the total minus the others. This check
#' follows every line of the table that has a total (a row or column
#' labelled `"Total"`, `"All"`, `"I alt"`, ...), recovers what can be
#' recovered, and repeats, because a recovered cell can open the next line.
#' The usual fix is secondary suppression: suppress a second cell in each
#' line, or remove the totals.
#'
#' Suppressed cells are `NA` or written as `"<5"`, `"."`, `".."`, `"x"` or
#' `"*"`. Totals released in another table are not seen here; see
#' [check_differencing()] and the register in [inspect()].
#'
#' @inheritParams check_min_count
#' @param x An aggregated table: a data frame with a count column, or a
#'   `table`.
#' @return A `mimic_check`. `details` lists the cells that can be
#'   recovered, by label.
#' @family checks
#' @export
#' @examples
#' tab <- data.frame(sex = c("F", "M", "Total"), n = c(NA, 120, 123))
#' check_suppression(tab)
#'
#' # a second suppressed cell protects the first
#' tab$n[2] <- NA
#' check_suppression(tab)
check_suppression <- function(x, by = NULL, count = NULL, config = NULL, target = NULL) {
  target <- target %||% label_of(substitute(x))
  df <- as_check_df(x)
  count <- count %||% guess_count(df) %||% guess_count(lapply(df, as_suppressed_numeric))
  if (is.null(count)) cli::cli_abort("Could not find a count column; give {.arg count}.")
  tc <- table_cells(df, by, count)
  cells <- tc$cells
  by <- tc$by
  known <- !is.na(cells$.n)
  if (all(known)) {
    return(new_check("suppression", target, "pass", "No cells are suppressed."))
  }
  tot <- lapply(cells[by], is_total)
  recovered <- rep(FALSE, nrow(cells))
  repeat {
    changed <- FALSE
    for (b in by) {
      if (!any(tot[[b]])) next
      gi <- group_index(cells, setdiff(by, b))
      for (g in seq_len(gi$n)) {
        rows <- which(gi$index == g)
        t_rows <- rows[tot[[b]][rows]]
        c_rows <- rows[!tot[[b]][rows]]
        if (length(t_rows) != 1 || !length(c_rows)) next
        unknown <- c_rows[!known[c_rows]]
        if (known[t_rows] && length(unknown) == 1) {
          known[unknown] <- recovered[unknown] <- changed <- TRUE
        } else if (!known[t_rows] && !length(unknown)) {
          known[t_rows] <- recovered[t_rows] <- changed <- TRUE
        }
      }
    }
    if (!changed) break
  }
  n_sup <- sum(is.na(cells$.n))
  if (!any(recovered)) {
    return(new_check("suppression", target, "pass",
                     sprintf("%d suppressed cell(s) cannot be recovered from the totals in the table.", n_sup)))
  }
  new_check("suppression", target, "fail",
            sprintf("%d of %d suppressed cell(s) can be recovered from the totals; suppress a second cell in each line or remove the totals.",
                    sum(recovered), n_sup),
            details = cells[recovered, by, drop = FALSE])
}

#' Check that no group shares one value of a variable
#'
#' Group disclosure (also called attribute or class disclosure): when every
#' person in a group has the same value of a variable, anyone who knows
#' that a person belongs to the group learns that value, even though no
#' cell is small. In a table of settlement by diagnosis, a settlement where
#' all 12 persons have the same diagnosis reveals the diagnosis of
#' everyone living there.
#'
#' For each grouping variable, the check looks at every combination of the
#' other grouping variables and warns when all its persons fall in a single
#' value. Missing combinations count as 0, suppressed cells as possibly
#' positive. The result is a warning, not a failure, because some zeros are
#' structural (pregnancies by sex) and only a person can tell.
#'
#' @inheritParams check_min_count
#' @return A `mimic_check`. `details` lists the groups and the variable
#'   they share.
#' @family checks
#' @export
#' @examples
#' tab <- data.frame(settlement = rep(c("A", "B"), each = 2),
#'                   smoker = rep(c("yes", "no"), 2),
#'                   n = c(12, 0, 30, 41))
#' check_group_disclosure(tab)
check_group_disclosure <- function(x, by = NULL, count = NULL, id = NULL,
                                   config = NULL, target = NULL) {
  target <- target %||% label_of(substitute(x))
  df <- as_check_df(x)
  aggregated <- !is.null(count) || (is.null(by) && !is.null(guess_count(df))) ||
    inherits(x, c("table", "matrix"))
  if (aggregated) {
    count <- count %||% guess_count(df)
    if (is.null(count)) cli::cli_abort("Could not find a count column; give {.arg count}.")
    tc <- table_cells(df, by, count)
  } else {
    tc <- table_cells(df, by, NULL, id)
  }
  by <- tc$by
  if (length(by) < 2) {
    return(new_check("group_disclosure", target, "pass",
                     "Fewer than two grouping variables; no group can share a value."))
  }
  cells <- tc$cells
  cells <- cells[!Reduce(`|`, lapply(cells[by], is_total)), , drop = FALSE]
  positive <- is.na(cells$.n) | cells$.n > 0

  hits <- list()
  for (a in by) {
    if (length(unique(as.character(cells[[a]]))) < 2) next
    gi <- group_index(cells, setdiff(by, a))
    one <- which(tabulate(gi$index[positive], gi$n) == 1)
    if (length(one)) {
      hits[[length(hits) + 1L]] <- data.frame(group = group_labels(gi$groups[one, , drop = FALSE]),
                                              shared = a, stringsAsFactors = FALSE)
    }
  }
  if (!length(hits)) {
    return(new_check("group_disclosure", target, "pass",
                     "No group has all its persons in one value of a variable."))
  }
  details <- do.call(rbind, hits)
  new_check("group_disclosure", target, "warn",
            sprintf("In %d group(s) everyone shares one value of %s; knowing that a person is in the group reveals it. Fine if the zeros are structural.",
                    nrow(details), paste(unique(details$shared), collapse = ", ")),
            details = details)
}

# Resolve the key specification to one key column (or NA) per table and the
# name of the main table (or NA).
resolve_keys <- function(tables, keys, main = NULL) {
  tn <- names(tables)
  key <- stats::setNames(rep(NA_character_, length(tn)), tn)
  if (!is.null(keys)) {
    if (!is.character(keys) || anyNA(keys)) {
      cli::cli_abort("{.arg keys} must be a column name or a named character vector of column names.")
    }
    if (is.null(names(keys))) {
      if (length(keys) != 1) {
        cli::cli_abort("Give {.arg keys} as one column name, or name each element after its table.")
      }
      key[] <- keys
    } else {
      bad <- setdiff(names(keys), tn)
      if (length(bad)) cli::cli_abort("{.arg keys} names unknown table{?s} {.val {bad}}.")
      key[names(keys)] <- keys
    }
  }
  has <- !is.na(key)
  missing <- tn[has][!vapply(tn[has], function(t) key[[t]] %in% names(tables[[t]]), logical(1))]
  if (length(missing)) {
    msg <- cli_escape(sprintf("Table '%s' has no column '%s'.", missing, key[missing]))
    cli::cli_abort(c("Key column missing.", stats::setNames(msg, rep("x", length(msg)))))
  }

  if (!is.null(main)) {
    if (!is.character(main) || length(main) != 1 || !main %in% tn) {
      cli::cli_abort("{.arg main} must be the name of one of the tables.")
    }
    if (is.na(key[[main]])) cli::cli_abort("The main table {.val {main}} has no key column.")
  } else {
    cand <- tn[has][vapply(tn[has], function(t) {
      v <- tables[[t]][[key[[t]]]]
      !anyNA(v) && !anyDuplicated(v)
    }, logical(1))]
    main <- if (length(cand) && sum(has) > 1) cand[1] else NA_character_
  }
  list(key = key, main = main)
}
