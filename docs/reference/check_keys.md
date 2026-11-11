# Check keys of linked tables

Data quality check before [`profile_data()`](profile_data.md): are key
columns present and complete, is the main table unique on its key, and
do the other tables only refer to persons in the main table? Persons who
are missing from a linked table (no contacts, no admissions) are normal
and not reported.

## Usage

``` r
check_keys(tables, keys, main = NULL, config = NULL, target = "keys")
```

## Arguments

- tables:

  A named list of data frames.

- keys:

  Key column. Either one name used in every table, or a named character
  vector mapping table names to key columns, for example
  `c(persons = "person_id", contacts = "pid")`.

- main:

  Name of the table with one row per person. By default the first table
  whose key is unique.

- config:

  A [`mimic_config()`](mimic_config.md); `NULL` uses the defaults.

- target:

  Label used in reports.

## Value

A `mimic_check`. Missing keys and duplicates in the main table fail;
rows that refer to unknown persons give a warning.

## See also

Other checks: [`check_differencing()`](check_differencing.md),
[`check_dominance()`](check_dominance.md),
[`check_extremes()`](check_extremes.md),
[`check_group_disclosure()`](check_group_disclosure.md),
[`check_k_anonymity()`](check_k_anonymity.md),
[`check_min_count()`](check_min_count.md),
[`check_model()`](check_model.md),
[`check_suppression()`](check_suppression.md)

## Examples

``` r
check_keys(list(persons = fake_persons, contacts = fake_contacts,
                admissions = fake_admissions),
           keys = "person_id")
#> ✔ keys (keys): Keys are complete and all linked tables refer to persons in 'persons'.
```
