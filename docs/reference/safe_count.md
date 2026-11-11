# Count persons with small cells suppressed

Counts persons in every combination of `by`, suppresses cells with fewer
than `min_count` persons and rounds the other counts to `round_base`.
With `total = TRUE` it adds totals labelled `"Total"` and suppresses
further cells where needed, so no suppressed cell can be worked out from
a total and the cells next to it. The result is ready for
[`utils::write.csv()`](https://rdrr.io/r/utils/write.table.html).

## Usage

``` r
safe_count(data, by, id = NULL, total = TRUE, config = NULL)
```

## Arguments

- data:

  A data frame, for example one of the tables in the script.

- by:

  Names of the columns to count by.

- id:

  Person identifier. Persons, not rows, are counted, so give it for a
  table with several rows per person (contacts, admissions). Without
  `id` each row is one person.

- total:

  Add totals: for one variable a `"Total"` row, for several the totals
  over each variable and their combinations.

- config:

  A [`mimic_config()`](mimic_config.md); `NULL` uses the rules staff
  gave [`trial_run()`](trial_run.md) or [`real_run()`](real_run.md), see
  [safe_output](safe_output.md).

## Value

A data frame with the `by` columns (as text when `total = TRUE`) and
`n`, the rounded number of persons, `NA` where suppressed. A message
says how many cells were suppressed.

## How cells are suppressed

1.  **Primary:** a cell with between 1 and `min_count - 1` persons is
    suppressed. Empty combinations are not listed; a cell of 0 is not a
    disclosure.

2.  **Secondary** (with totals): in every row or column of the table
    that has a total, one suppressed cell alone would be the total minus
    the others. The smallest other cell in that line is then suppressed
    too, or the total when there is no other cell. This repeats until no
    line gives a cell back, and the result is confirmed with
    [`check_suppression()`](check_suppression.md).

3.  **Rounding:** the remaining counts, totals included, are rounded to
    `round_base`, after suppression, so a rounded count of 5 is never a
    rounded 3.

Totals count distinct persons. When one person can be in several cells
(contacts by diagnosis), a total is smaller than the sum of its cells;
secondary suppression still treats the cells as adding up, which
suppresses more than needed but never too little.

Suppressed cells are `NA`, which [`inspect()`](inspect.md) reads as
suppressed.

## See also

Other safe output: [`describe_output()`](describe_output.md),
[`safe_model()`](safe_model.md), [`safe_summary()`](safe_summary.md)

## Examples

``` r
safe_count(fake_persons, "sex")
#>     sex    n
#> 1     F  985
#> 2     M 1015
#> 3 Total 2000
safe_count(fake_persons, c("settlement", "sex"))
#> Suppressed 16 cells with fewer than 5 persons, and 2 more so they cannot be
#> worked out from the totals.
#>       settlement   sex    n
#> 1  Settlement_01     F   55
#> 2  Settlement_01     M   65
#> 3  Settlement_02     F   45
#> 4  Settlement_02     M   45
#> 5  Settlement_03     F   40
#> 6  Settlement_03     M   40
#> 7  Settlement_04     F   35
#> 8  Settlement_04     M   25
#> 9  Settlement_05     F   25
#> 10 Settlement_05     M   30
#> 11 Settlement_06     F   15
#> 12 Settlement_06     M   25
#> 13 Settlement_07     F   15
#> 14 Settlement_07     M   20
#> 15 Settlement_08     F   15
#> 16 Settlement_08     M   15
#> 17 Settlement_09     F   15
#> 18 Settlement_09     M   10
#> 19 Settlement_10     F   10
#> 20 Settlement_10     M   10
#> 21 Settlement_11     F    5
#> 22 Settlement_11     M   10
#> 23 Settlement_12     F    5
#> 24 Settlement_12     M   10
#> 25 Settlement_13     F   NA
#> 26 Settlement_13     M   NA
#> 27 Settlement_14     F   NA
#> 28 Settlement_14     M   NA
#> 29 Settlement_15     F   NA
#> 30 Settlement_15     M   NA
#> 31 Settlement_16     M   NA
#> 32 Settlement_17     F   NA
#> 33 Settlement_17     M   NA
#> 34 Settlement_18     F   NA
#> 35 Settlement_18     M   NA
#> 36 Settlement_19     F   NA
#> 37 Settlement_20     F   NA
#> 38        Town_A     F  450
#> 39        Town_A     M  470
#> 40        Town_B     F  235
#> 41        Town_B     M  215
#> 42         Total     F  985
#> 43         Total     M 1015
#> 44 Settlement_01 Total  120
#> 45 Settlement_02 Total   90
#> 46 Settlement_03 Total   80
#> 47 Settlement_04 Total   60
#> 48 Settlement_05 Total   55
#> 49 Settlement_06 Total   40
#> 50 Settlement_07 Total   35
#> 51 Settlement_08 Total   30
#> 52 Settlement_09 Total   25
#> 53 Settlement_10 Total   20
#> 54 Settlement_11 Total   20
#> 55 Settlement_12 Total   15
#> 56 Settlement_13 Total   10
#> 57 Settlement_14 Total   10
#> 58 Settlement_15 Total    5
#> 59 Settlement_16 Total   NA
#> 60 Settlement_17 Total   NA
#> 61 Settlement_18 Total   NA
#> 62 Settlement_19 Total   NA
#> 63 Settlement_20 Total   NA
#> 64        Town_A Total  925
#> 65        Town_B Total  450
#> 66         Total Total 2000

# persons, not contacts
safe_count(fake_contacts, "contact_type", id = "person_id")
#>   contact_type    n
#> 1       Clinic 1250
#> 2    Emergency  535
#> 3        Nurse 1085
#> 4        Phone  910
#> 5 Telemedicine   70
#> 6        Total 1630
```
