# Check that a model rests on enough observations

A model fitted to very few observations can reveal individual values:
with zero residual degrees of freedom the fitted values *are* the data.
And a fitted model *object* carries its data: `lm` and `glm` objects
hold the model frame, residuals and fitted values for every person, and
so does [`summary.lm()`](https://rdrr.io/r/stats/summary.lm.html).
Release a table of coefficients instead. The check fails when

- the object contains row-level components (`model`, `residuals`,
  `fitted.values`, `y`, `qr`, ...),

- residual degrees of freedom are below `config$min_df`,

- a factor level in the model data has fewer than `min_count`
  observations,

- for a binary outcome, an outcome class has fewer than `min_count`
  observations.

## Usage

``` r
check_model(fit, config = NULL, target = NULL)
```

## Arguments

- fit:

  A fitted model, for example from
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html) or
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html).

- config:

  A [`mimic_config()`](mimic_config.md); `NULL` uses the defaults.

- target:

  Label used in reports.

## Value

A `mimic_check`.

## Details

It warns when a binary outcome has a cell with fewer than `min_count`
observations within a factor level. Objects without
[`stats::nobs()`](https://rdrr.io/r/stats/nobs.html),
[`stats::df.residual()`](https://rdrr.io/r/stats/df.residual.html) or
[`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html)
methods give a warning so they are reviewed by hand.

## See also

Other checks: [`check_differencing()`](check_differencing.md),
[`check_dominance()`](check_dominance.md),
[`check_extremes()`](check_extremes.md),
[`check_group_disclosure()`](check_group_disclosure.md),
[`check_k_anonymity()`](check_k_anonymity.md),
[`check_keys()`](check_keys.md),
[`check_min_count()`](check_min_count.md),
[`check_suppression()`](check_suppression.md)

## Examples

``` r
# a fitted lm object holds the data: not for release
fit <- lm(income ~ sex + occupation, data = fake_persons)
check_model(fit)
#> ✖ model (fit): 1 issue(s) with the observations behind the model.
#>                                                                                                                                  issue
#>  the object contains row-level data (model, residuals, fitted.values, effects, qr, na.action); release a table of coefficients instead
#>  severity
#>      fail

# the observations behind it are fine once the row-level parts are removed
fit[c("model", "residuals", "fitted.values", "effects", "qr", "na.action")] <- NULL
check_model(fit)
#> ✔ model (fit): Model rests on enough observations (residual df at least 10).
```
