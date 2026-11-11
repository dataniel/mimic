# Check for minimum, maximum and other single-person values

Minimum and maximum values almost always belong to one person. This
check looks for them by name: column names, row names, names of a vector
and character cells such as `"Max."` in a statistics column (for example
output from [`summary()`](https://rdrr.io/r/base/summary.html)).

## Usage

``` r
check_extremes(x, config = NULL, target = NULL)
```

## Arguments

- x:

  A data frame, table, matrix or named vector.

- config:

  A [`mimic_config()`](mimic_config.md); `NULL` uses the defaults.

- target:

  Label used in reports.

## Value

A `mimic_check` listing where extremes were found.

## See also

Other checks: [`check_differencing()`](check_differencing.md),
[`check_dominance()`](check_dominance.md),
[`check_group_disclosure()`](check_group_disclosure.md),
[`check_k_anonymity()`](check_k_anonymity.md),
[`check_keys()`](check_keys.md),
[`check_min_count()`](check_min_count.md),
[`check_model()`](check_model.md),
[`check_suppression()`](check_suppression.md)

## Examples

``` r
check_extremes(summary(fake_persons$income))
#> ✖ extremes (summary(fake_persons$income)): Found 2 minimum, maximum or range value(s). These are often one person's value.
#>  location name
#>      name Min.
#>      name Max.
check_extremes(data.frame(group = "a", mean = 1, max_income = 2))
#> ✖ extremes (data.frame(group = "a", mean = 1, max...): Found 1 minimum, maximum or range value(s). These are often one person's value.
#>  location       name
#>    column max_income
```
