# A table of model coefficients that is safe to release

A fitted model object carries its data: an `lm` or `glm` object holds
the model frame, residuals and fitted values of every person, so it is
never released. `safe_model()` returns the coefficient table instead,
after checking with [`check_model()`](check_model.md) that the model
rests on enough observations: residual degrees of freedom of at least
`min_df`, every factor level and, for a binary outcome, both outcome
classes with at least `min_count` observations.

## Usage

``` r
safe_model(fit, config = NULL)
```

## Arguments

- fit:

  A fitted model with a coefficient table in `summary(fit)`, for example
  from [`stats::lm()`](https://rdrr.io/r/stats/lm.html),
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html) or
  [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html).

- config:

  A [`mimic_config()`](mimic_config.md); `NULL` uses the rules staff
  gave [`trial_run()`](trial_run.md) or [`real_run()`](real_run.md), see
  [safe_output](safe_output.md).

## Value

A data frame with one row per coefficient: `term`, the columns of the
coefficient table (`estimate`, `std_error`, `statistic`, `p_value` for
`lm` and `glm`), `observations` (rounded number of observations) and
`df_residual`.

## Details

The function stops when a rule is broken, so nothing is written that
would be sent back. A binary outcome with a small cell within a factor
level gives a warning: the coefficient is then estimated from very few
persons, and the checker will look at it.

## See also

Other safe output: [`describe_output()`](describe_output.md),
[`safe_count()`](safe_count.md), [`safe_summary()`](safe_summary.md)

## Examples

``` r
fit <- lm(income ~ sex + occupation, data = fake_persons)
safe_model(fit)
#>                       term   estimate std_error   statistic     p_value
#> 1              (Intercept) 324520.977 104045.41  3.11903206 0.001841293
#> 2                     sexM  21577.394  13376.67  1.61306194 0.106894706
#> 3   occupationConstruction   5081.208 106898.90  0.04753284 0.962093484
#> 4        occupationFishing -45644.392 105034.87 -0.43456417 0.663927436
#> 5        occupationHunting -52591.829 108350.67 -0.48538535 0.627458175
#> 6         occupationMining -43318.765 112990.13 -0.38338540 0.701476298
#> 7  occupationPublic sector -44086.654 104783.96 -0.42073857 0.673992886
#> 8        occupationRetired -65921.760 105922.59 -0.62235788 0.533780161
#> 9        occupationStudent -36226.991 105642.93 -0.34291920 0.731696631
#> 10       occupationTourism -11699.593 116209.83 -0.10067645 0.919817777
#> 11         occupationTrade -49937.265 106131.92 -0.47052071 0.638036268
#> 12    occupationUnemployed -40818.524 107387.75 -0.38010411 0.703910001
#>    observations df_residual
#> 1          1940        1928
#> 2          1940        1928
#> 3          1940        1928
#> 4          1940        1928
#> 5          1940        1928
#> 6          1940        1928
#> 7          1940        1928
#> 8          1940        1928
#> 9          1940        1928
#> 10         1940        1928
#> 11         1940        1928
#> 12         1940        1928
```
