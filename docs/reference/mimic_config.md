# Disclosure control settings for small populations

Creates the configuration object used by
[`profile_data()`](profile_data.md), [`mimic()`](mimic.md),
[`real_run()`](real_run.md) and [`inspect()`](inspect.md). The defaults
are deliberately stricter than in tools designed for large countries: in
a small population with many settlements of a few hundred people, a cell
of 3 is often a known household.

## Usage

``` r
mimic_config(
  min_count = 5,
  dominance = list(n = 2, k = 0.85),
  p_percent = 0.1,
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
  require_description = FALSE
)
```

## Arguments

- min_count:

  Smallest number of persons allowed behind any category, cell or bin.
  Must be at least 3.

- dominance:

  List with `n` (number of largest contributors) and `k` (maximum share
  they may hold) for the (n, k) dominance rule.

- p_percent:

  Threshold for the p % rule, as a proportion.

- quasi_ids:

  Character vector of quasi-identifiers (for example age, sex,
  settlement). Rare combinations are reported by
  [`profile_data()`](profile_data.md).

- geo_vars:

  Character vector of geographical variables. Small units are merged
  according to `min_count`.

- id_vars:

  Character vector of identifier variables. Only their type and format
  are stored, never values. Key variables are always treated as
  identifiers.

- text_vars:

  Character vector of free-text variables. Only an approximate length is
  stored. Character columns with many unique long values are also
  detected automatically.

- date_unit:

  Granularity for dates: `"month"` or `"year"`.

- tail_coding:

  Lower and upper quantile used for bottom and top coding.

- round_base:

  Counts are rounded to a multiple of this number.

- signif_digits:

  Number of significant digits kept in summary values and bin edges.

- min_persons:

  Smallest number of persons for which synthesis above level 2 is
  considered safe.

- min_df:

  Smallest residual degrees of freedom accepted for a model in
  [`inspect()`](inspect.md).

- other_label:

  Label used for merged categories.

- require_description:

  `TRUE` makes [`inspect()`](inspect.md) send back every output file
  that has no description from
  [`describe_output()`](describe_output.md).

## Value

An object of class `mimic_config`.

## Why these defaults

- `min_count = 5` is the floor used by most national statistical
  offices. Raise it to 10 for health data or very small settlements.

- The tail quantiles in `tail_coding` are never allowed to fall on fewer
  than `min_count` people. With 40 rows, the 1 % quantile *is* one
  person's value, so [`profile_data()`](profile_data.md) uses
  `max(tail_coding[1], min_count / n)`.

- The dominance rule `(n = 2, k = 0.85)` and the p % rule follow the
  Eurostat guidelines for output checking: no two contributors may make
  up 85 % of a sum, and no contributor may be estimated within 10 % by
  the second largest.

## Examples

``` r
cfg <- mimic_config(
  min_count = 10,
  quasi_ids = c("birth_date", "sex", "settlement"),
  geo_vars = "settlement"
)
cfg
#> <mimic_config>
#> • min_count: 10
#> • dominance: (n = 2, k = 0.85); p % rule: 0.1
#> • tail coding: 0.01 / 0.99; date unit: month
#> • rounding: counts to 5, values to 2 significant digits
#> • quasi-identifiers: birth_date, sex, settlement
#> • geographical: settlement
#> • identifiers: (none)
#> • free text: (none)
#> • min persons for synthesis: 100; min model df: 10
#> • merged category label: "Other"
#> • descriptions of output: optional
```
