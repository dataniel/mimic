# Check combinations of quasi-identifiers (k-anonymity)

Counts how many persons share each combination of the quasi-identifiers.
Persons in a combination shared by fewer than `min_count` persons are at
risk of re-identification. Dates are compared at `date_unit` granularity
and numeric variables with many values in deciles; group them yourself
if you need another granularity.

## Usage

``` r
check_k_anonymity(x, quasi_ids = NULL, id = NULL, config = NULL, target = NULL)
```

## Arguments

- x:

  A data frame, or a `table`/matrix of counts.

- quasi_ids:

  Quasi-identifiers. Defaults to `config$quasi_ids`.

- id:

  Person identifier in microdata. Persons, not rows, are counted.
  Without `id` each row is one person.

- config:

  A [`mimic_config()`](mimic_config.md); `NULL` uses the defaults.

- target:

  Label used in reports.

## Value

A `mimic_check`. `details` contains totals only, never the rare
combinations.

## Details

The result also shows how many persons would be at risk if each
quasi-identifier were dropped, which points to the variable to coarsen.

## See also

Other checks: [`check_differencing()`](check_differencing.md),
[`check_dominance()`](check_dominance.md),
[`check_extremes()`](check_extremes.md),
[`check_group_disclosure()`](check_group_disclosure.md),
[`check_keys()`](check_keys.md),
[`check_min_count()`](check_min_count.md),
[`check_model()`](check_model.md),
[`check_suppression()`](check_suppression.md)

## Examples

``` r
check_k_anonymity(fake_persons, c("birth_date", "sex", "settlement"),
                  id = "person_id")
#> ✖ k_anonymity (fake_persons): 2,000 persons (about 100 %) are in combinations of birth_date, sex, settlement shared by fewer than 5 persons.
#>                      variables combinations rare_combinations persons_at_risk
#>  birth_date + sex + settlement        1,782             1,782           2,000
#>             without birth_date           41                11              25
#>                    without sex        1,599             1,597           1,990
#>             without settlement        1,309             1,301           1,960
```
