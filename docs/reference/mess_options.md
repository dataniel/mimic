# Choose which kinds of mess to add

Choose which kinds of mess to add

## Usage

``` r
mess_options(
  missing = TRUE,
  invalid_codes = TRUE,
  extremes = TRUE,
  empty_strings = TRUE,
  odd_dates = TRUE,
  orphans = TRUE,
  rate = 0.01
)
```

## Arguments

- missing:

  Missing values in the same proportion as the real data.

- invalid_codes:

  Invalid or odd codes in categorical variables.

- extremes:

  Extreme values in numeric variables.

- empty_strings:

  Empty and blank strings in character variables.

- odd_dates:

  Implausible dates.

- orphans:

  Rows in linked tables whose key is not in the main table.

- rate:

  Share of rows affected per variable by each kind of mess other than
  missing values. At least one row is always affected, so the script
  meets the problem.

## Value

An object of class `mimic_mess` for [`mimic()`](mimic.md).

## Examples

``` r
mess_options(orphans = FALSE, rate = 0.05)
#> <mimic_mess> missing, invalid_codes, extremes, empty_strings, odd_dates; rate
#> 0.05
```
