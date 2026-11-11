# Summarise a numeric variable without small or dominated groups

Gives the number of persons and chosen statistics of `value` for every
group of `by`. A group with fewer than `min_count` persons has all its
statistics suppressed. Means, sums and standard deviations are also
suppressed where one or two persons dominate the group by the rules in
[`check_dominance()`](check_dominance.md): the mean income of a
settlement where one person earns most of the money is close to that
person's income.

## Usage

``` r
safe_summary(
  data,
  value,
  by = NULL,
  id = NULL,
  stats = c("mean", "sd", "median"),
  config = NULL
)
```

## Arguments

- data:

  A data frame, for example one of the tables in the script.

- value:

  Name of the numeric column to summarise. Missing values are left out,
  and persons are counted among the rows with a value.

- by:

  Names of the columns to group by; `NULL` for one row for all.

- id:

  Person identifier. Persons are counted, and for the dominance rules
  each person's values are added up first. Without `id` each row is one
  person.

- stats:

  Statistics to give: any of `"mean"`, `"sd"`, `"median"`, `"p25"`,
  `"p75"` and `"sum"`.

- config:

  A [`mimic_config()`](mimic_config.md); `NULL` uses the rules staff
  gave [`trial_run()`](trial_run.md) or [`real_run()`](real_run.md), see
  [safe_output](safe_output.md).

## Value

A data frame with the `by` columns, `n` (rounded number of persons, `NA`
when the group is too small) and one column per statistic, `NA` where
suppressed. A message says how many groups were suppressed.

## Details

Minimum and maximum are not offered: they almost always belong to one
person. Use the quartiles (`p25`, `p75`) or the median instead.

Statistics are not rounded; the number of persons is rounded to
`round_base`. Column names are `mean_<value>`, `median_<value>` and so
on, which [`inspect()`](inspect.md) matches to the variable when it
checks dominance on the real data.

## See also

Other safe output: [`describe_output()`](describe_output.md),
[`safe_count()`](safe_count.md), [`safe_model()`](safe_model.md)

## Examples

``` r
safe_summary(fake_persons, "income", by = "sex")
#>   sex   n mean_income sd_income median_income
#> 1   F 960    282416.3  179948.3        237100
#> 2   M 980    303660.0  372472.7        237700
safe_summary(fake_persons, "income", by = "settlement", stats = c("median", "p25", "p75"))
#> Suppressed 5 groups with fewer than 5 persons.
#>       settlement   n median_income p25_income p75_income
#> 1  Settlement_01 115        253800     162100     377600
#> 2  Settlement_02  90        222200     151950     370300
#> 3  Settlement_03  80        230700     149000     351800
#> 4  Settlement_04  60        254150     165925     377000
#> 5  Settlement_05  55        221800     145950     292350
#> 6  Settlement_06  40        231000     166600     304750
#> 7  Settlement_07  35        214800     146400     300700
#> 8  Settlement_08  30        237250     150225     317625
#> 9  Settlement_09  25        283400     154200     333700
#> 10 Settlement_10  15        220300     128800     324900
#> 11 Settlement_11  20        211600     134775     323900
#> 12 Settlement_12  15        255400     162650     463000
#> 13 Settlement_13  10        247000     192850     318000
#> 14 Settlement_14  10        188900     180600     361700
#> 15 Settlement_15   5        455200     300200     633950
#> 16 Settlement_16  NA            NA         NA         NA
#> 17 Settlement_17  NA            NA         NA         NA
#> 18 Settlement_18  NA            NA         NA         NA
#> 19 Settlement_19  NA            NA         NA         NA
#> 20 Settlement_20  NA            NA         NA         NA
#> 21        Town_A 900        231300     158150     362350
#> 22        Town_B 430        254200     170000     348850
```
