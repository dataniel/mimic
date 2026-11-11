#' Disclosure control settings for small populations
#'
#' Creates the configuration object used by [profile_data()], [mimic()],
#' [real_run()] and [inspect()]. The defaults are deliberately stricter than in
#' tools designed for large countries: in a small population with many
#' settlements of a few hundred people, a cell of 3 is often a known
#' household.
#'
#' @section Why these defaults:
#' * `min_count = 5` is the floor used by most national statistical offices.
#'   Raise it to 10 for health data or very small settlements.
#' * The tail quantiles in `tail_coding` are never allowed to fall on fewer
#'   than `min_count` people. With 40 rows, the 1 % quantile *is* one
#'   person's value, so [profile_data()] uses `max(tail_coding[1], min_count / n)`.
#' * The dominance rule `(n = 2, k = 0.85)` and the p % rule follow the
#'   Eurostat guidelines for output checking: no two contributors may make
#'   up 85 % of a sum, and no contributor may be estimated within 10 % by the
#'   second largest.
#'
#' @param min_count Smallest number of persons allowed behind any category,
#'   cell or bin. Must be at least 3.
#' @param dominance List with `n` (number of largest contributors) and `k`
#'   (maximum share they may hold) for the (n, k) dominance rule.
#' @param p_percent Threshold for the p % rule, as a proportion.
#' @param quasi_ids Character vector of quasi-identifiers (for example age,
#'   sex, settlement). Rare combinations are reported by [profile_data()].
#' @param geo_vars Character vector of geographical variables. Small units
#'   are merged according to `min_count`.
#' @param id_vars Character vector of identifier variables. Only their type
#'   and format are stored, never values. Key variables are always treated
#'   as identifiers.
#' @param text_vars Character vector of free-text variables. Only an
#'   approximate length is stored. Character columns with many unique long
#'   values are also detected automatically.
#' @param date_unit Granularity for dates: `"month"` or `"year"`.
#' @param tail_coding Lower and upper quantile used for bottom and top coding.
#' @param round_base Counts are rounded to a multiple of this number.
#' @param signif_digits Number of significant digits kept in summary values
#'   and bin edges.
#' @param min_persons Smallest number of persons for which synthesis above
#'   level 2 is considered safe.
#' @param min_df Smallest residual degrees of freedom accepted for a model
#'   in [inspect()].
#' @param other_label Label used for merged categories.
#' @param require_description `TRUE` makes [inspect()] send back every
#'   output file that has no description from [describe_output()].
#'
#' @return An object of class `mimic_config`.
#' @export
#' @examples
#' cfg <- mimic_config(
#'   min_count = 10,
#'   quasi_ids = c("birth_date", "sex", "settlement"),
#'   geo_vars = "settlement"
#' )
#' cfg
mimic_config <- function(min_count = 5,
                             dominance = list(n = 2, k = 0.85),
                             p_percent = 0.10,
                             quasi_ids = character(),
                             geo_vars = character(),
                             id_vars = character(),
                             text_vars = character(),
                             date_unit = c("month", "year"),
                             tail_coding = c(0.01, 0.99),
                             round_base = 5,
                             signif_digits = 2,
                             min_persons = 100,
                             min_df = 10,
                             other_label = "Other",
                             require_description = FALSE) {
  date_unit <- match.arg(date_unit)

  assert_number(min_count, "min_count", min = 3)
  assert_number(p_percent, "p_percent", min = 0, max = 1)
  assert_number(round_base, "round_base", min = 1)
  assert_number(signif_digits, "signif_digits", min = 1)
  assert_number(min_persons, "min_persons", min = 1)
  assert_number(min_df, "min_df", min = 1)

  if (!is.list(dominance) || !all(c("n", "k") %in% names(dominance))) {
    cli::cli_abort("{.arg dominance} must be a list with elements {.field n} and {.field k}.")
  }
  assert_number(dominance$n, "dominance$n", min = 1)
  assert_number(dominance$k, "dominance$k", min = 0, max = 1)

  if (!is.numeric(tail_coding) || length(tail_coding) != 2 ||
      anyNA(tail_coding) || tail_coding[1] < 0 || tail_coding[2] > 1 ||
      tail_coding[1] >= tail_coding[2]) {
    cli::cli_abort("{.arg tail_coding} must be two increasing proportions, for example {.code c(0.01, 0.99)}.")
  }
  if (round_base > min_count) {
    cli::cli_abort("{.arg round_base} ({round_base}) must not exceed {.arg min_count} ({min_count}).")
  }

  for (nm in c("quasi_ids", "geo_vars", "id_vars", "text_vars")) {
    val <- get(nm)
    if (!is.character(val) || anyNA(val)) {
      cli::cli_abort("{.arg {nm}} must be a character vector of variable names.")
    }
  }
  if (!is.character(other_label) || length(other_label) != 1 ||
      is.na(other_label) || !nzchar(other_label)) {
    cli::cli_abort("{.arg other_label} must be a single non-empty string.")
  }
  if (!is.logical(require_description) || length(require_description) != 1 || is.na(require_description)) {
    cli::cli_abort("{.arg require_description} must be {.code TRUE} or {.code FALSE}.")
  }

  structure(
    list(
      min_count = min_count,
      dominance = list(n = dominance$n, k = dominance$k),
      p_percent = p_percent,
      quasi_ids = unique(quasi_ids),
      geo_vars = unique(geo_vars),
      id_vars = unique(id_vars),
      text_vars = unique(text_vars),
      date_unit = date_unit,
      tail_coding = tail_coding,
      round_base = round_base,
      signif_digits = signif_digits,
      min_persons = min_persons,
      min_df = min_df,
      other_label = other_label,
      require_description = require_description
    ),
    class = "mimic_config"
  )
}

#' @export
print.mimic_config <- function(x, ...) {
  show <- function(v) if (length(v)) paste(v, collapse = ", ") else "(none)"
  cli::cli_text("{.strong <mimic_config>}")
  cli::cli_ul()
  cli::cli_li("min_count: {x$min_count}")
  cli::cli_li("dominance: (n = {x$dominance$n}, k = {x$dominance$k}); p % rule: {x$p_percent}")
  cli::cli_li("tail coding: {x$tail_coding[1]} / {x$tail_coding[2]}; date unit: {x$date_unit}")
  cli::cli_li("rounding: counts to {x$round_base}, values to {x$signif_digits} significant digits")
  cli::cli_li("quasi-identifiers: {show(x$quasi_ids)}")
  cli::cli_li("geographical: {show(x$geo_vars)}")
  cli::cli_li("identifiers: {show(x$id_vars)}")
  cli::cli_li("free text: {show(x$text_vars)}")
  cli::cli_li("min persons for synthesis: {x$min_persons}; min model df: {x$min_df}")
  cli::cli_li("merged category label: {.val {x$other_label}}")
  cli::cli_li("descriptions of output: {if (isTRUE(x$require_description)) 'required' else 'optional'}")
  cli::cli_end()
  invisible(x)
}

# Accept NULL (defaults) or a mimic_config; anything else is an error.
as_config <- function(config, arg = "config") {
  if (is.null(config)) return(mimic_config())
  if (!inherits(config, "mimic_config")) {
    cli::cli_abort("{.arg {arg}} must be created with {.fn mimic_config}.")
  }
  config
}

assert_number <- function(x, arg, min = -Inf, max = Inf) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < min || x > max) {
    range <- if (is.finite(max)) paste0("between ", min, " and ", max) else paste0("at least ", min)
    cli::cli_abort("{.arg {arg}} must be a single number {range}.")
  }
  invisible(x)
}
