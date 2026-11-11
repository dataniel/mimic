# Check that no group shares one value of a variable

Group disclosure (also called attribute or class disclosure): when every
person in a group has the same value of a variable, anyone who knows
that a person belongs to the group learns that value, even though no
cell is small. In a table of settlement by diagnosis, a settlement where
all 12 persons have the same diagnosis reveals the diagnosis of everyone
living there.

## Usage

``` r
check_group_disclosure(
  x,
  by = NULL,
  count = NULL,
  id = NULL,
  config = NULL,
  target = NULL
)
```

## Arguments

- x:

  A data frame, or a `table`/matrix of counts.

- by:

  Grouping variables. For an aggregated table the default is all
  non-numeric columns.

- count:

  Name of the count column in an aggregated table. Detected
  automatically when a column is called `n`, `count`, `freq` or similar.
  Leave `NULL` with `by` for microdata.

- id:

  Person identifier in microdata. Persons, not rows, are counted.
  Without `id` each row is one person.

- config:

  A [`mimic_config()`](mimic_config.md); `NULL` uses the defaults.

- target:

  Label used in reports.

## Value

A `mimic_check`. `details` lists the groups and the variable they share.

## Details

For each grouping variable, the check looks at every combination of the
other grouping variables and warns when all its persons fall in a single
value. Missing combinations count as 0, suppressed cells as possibly
positive. The result is a warning, not a failure, because some zeros are
structural (pregnancies by sex) and only a person can tell.

## See also

Other checks: [`check_differencing()`](check_differencing.md),
[`check_dominance()`](check_dominance.md),
[`check_extremes()`](check_extremes.md),
[`check_k_anonymity()`](check_k_anonymity.md),
[`check_keys()`](check_keys.md),
[`check_min_count()`](check_min_count.md),
[`check_model()`](check_model.md),
[`check_suppression()`](check_suppression.md)

## Examples

``` r
tab <- data.frame(settlement = rep(c("A", "B"), each = 2),
                  smoker = rep(c("yes", "no"), 2),
                  n = c(12, 0, 30, 41))
check_group_disclosure(tab)
#> ! group_disclosure (tab): In 2 group(s) everyone shares one value of settlement, smoker; knowing that a person is in the group reveals it. Fine if the zeros are structural.
#>           group     shared
#>     smoker = no settlement
#>  settlement = A     smoker
```
