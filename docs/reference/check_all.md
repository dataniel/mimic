# Collect checks into one report

Collect checks into one report

## Usage

``` r
check_all(...)
```

## Arguments

- ...:

  `mimic_check` objects or lists of them.

## Value

A data frame of class `mimic_report` with one row per check (`rule`,
`target`, `status`, `message`). The full checks are kept in the
attribute `"checks"`.

## Examples

``` r
check_all(
  check_min_count(fake_persons, by = "settlement", id = "person_id"),
  check_keys(list(persons = fake_persons, contacts = fake_contacts),
             keys = "person_id")
)
#> ✖ min_count (fake_persons): 5 of 22 cells have fewer than 5 persons.
#>     settlement
#>  Settlement_17
#>  Settlement_16
#>  Settlement_19
#>  Settlement_18
#>  Settlement_20
#> ✔ keys (keys): Keys are complete and all linked tables refer to persons in 'persons'.
#> ────────────────────────────────────────────────────────────────────────────────
#> 1 passed, 0 warnings, 1 failed.
```
