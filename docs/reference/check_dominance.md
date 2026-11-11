# Check that no few contributors dominate a sum

Applies two rules from the Eurostat guidelines for output checking to
every group:

- **(n, k) rule:** the `n` largest contributors may not make up more
  than `k` of the total (default: 2 contributors, 85 %).

- **p % rule:** the total minus the two largest contributions must be at
  least `p` of the largest, so the second largest contributor cannot
  estimate the largest within `p`. Groups with fewer than 3 contributors
  always fail this rule.

## Usage

``` r
check_dominance(x, value, by = NULL, id = NULL, config = NULL, target = NULL)
```

## Arguments

- x:

  A data frame, or a `table`/matrix of counts.

- value:

  Numeric variable that is summed.

- by:

  Grouping variables. For an aggregated table the default is all
  non-numeric columns.

- id:

  Contributor identifier. Contributions are summed per contributor
  first. Without `id` each row is one contributor.

- config:

  A [`mimic_config()`](mimic_config.md); `NULL` uses the defaults.

- target:

  Label used in reports.

## Value

A `mimic_check`. `details` lists failing groups and the rule they break.

## Details

Use it before releasing sums, means or totals of skewed variables such
as income, costs or catch.

## See also

Other checks: [`check_differencing()`](check_differencing.md),
[`check_extremes()`](check_extremes.md),
[`check_group_disclosure()`](check_group_disclosure.md),
[`check_k_anonymity()`](check_k_anonymity.md),
[`check_keys()`](check_keys.md),
[`check_min_count()`](check_min_count.md),
[`check_model()`](check_model.md),
[`check_suppression()`](check_suppression.md)

## Examples

``` r
check_dominance(fake_persons, value = "income", by = "settlement",
                id = "person_id")
#> ✖ dominance (fake_persons): 3 of 22 groups of income are dominated by few contributors (n = 2, k = 0.85) and p = 0.1.
#>     settlement    rule_broken
#>  Settlement_19 (n, k) and p %
#>  Settlement_18 (n, k) and p %
#>  Settlement_20 (n, k) and p %
```
