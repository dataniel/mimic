# Check that every cell has enough persons

Works on microdata (one row per person or observation, give `by`) and on
aggregated tables (one row per cell, with a count column). Empty cells
(0) are allowed; cells with between 1 and `min_count - 1` persons fail.

## Usage

``` r
check_min_count(
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

A `mimic_check`. `details` lists the failing groups by label, never
their counts.

## See also

Other checks: [`check_differencing()`](check_differencing.md),
[`check_dominance()`](check_dominance.md),
[`check_extremes()`](check_extremes.md),
[`check_group_disclosure()`](check_group_disclosure.md),
[`check_k_anonymity()`](check_k_anonymity.md),
[`check_keys()`](check_keys.md), [`check_model()`](check_model.md),
[`check_suppression()`](check_suppression.md)

## Examples

``` r
check_min_count(fake_persons, by = "settlement", id = "person_id")
#> ✖ min_count (fake_persons): 5 of 22 cells have fewer than 5 persons.
#>     settlement
#>  Settlement_17
#>  Settlement_16
#>  Settlement_19
#>  Settlement_18
#>  Settlement_20

tab <- as.data.frame(table(sex = fake_persons$sex))
check_min_count(tab)
#> ✔ min_count (tab): All 2 cells have 0 or at least 5 persons.
```
