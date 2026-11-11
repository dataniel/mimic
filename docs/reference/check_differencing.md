# Check that two tables cannot be subtracted

A differencing attack subtracts two released tables to reveal a small
group: a table for all persons and one for all persons except one
settlement differ by exactly that settlement. The check compares counts
cell by cell on the shared grouping variables and also compares the
grand totals. It fails if any difference is between 1 and
`min_count - 1`.

## Usage

``` r
check_differencing(x, y, by = NULL, count = NULL, config = NULL, target = NULL)
```

## Arguments

- x, y:

  Aggregated tables (data frames with a count column, or tables).

- by:

  Grouping variables shared by both tables. Defaults to the non-numeric
  columns they have in common.

- count:

  Name of the count column, the same in both tables. Detected
  automatically when possible.

- config:

  A [`mimic_config()`](mimic_config.md); `NULL` uses the defaults.

- target:

  Label used in reports.

## Value

A `mimic_check`. `details` lists the groups whose difference is small.

## See also

Other checks: [`check_dominance()`](check_dominance.md),
[`check_extremes()`](check_extremes.md),
[`check_group_disclosure()`](check_group_disclosure.md),
[`check_k_anonymity()`](check_k_anonymity.md),
[`check_keys()`](check_keys.md),
[`check_min_count()`](check_min_count.md),
[`check_model()`](check_model.md),
[`check_suppression()`](check_suppression.md)

## Examples

``` r
all <- as.data.frame(table(sex = fake_persons$sex))
most <- as.data.frame(table(sex = fake_persons$sex[fake_persons$settlement != "Settlement_20"]))
check_differencing(all, most)
#> ✖ differencing (all vs most): Subtracting the tables reveals fewer than 5 persons in 1 cell(s) and the grand totals.
#>  sex
#>    F
```
