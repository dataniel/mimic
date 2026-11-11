# Check that suppressed cells cannot be recovered

Suppressing a small cell only helps if it cannot be worked out from the
rest of the table. When a row or column has a total and only one
suppressed cell, the cell is the total minus the others. This check
follows every line of the table that has a total (a row or column
labelled `"Total"`, `"All"`, `"I alt"`, ...), recovers what can be
recovered, and repeats, because a recovered cell can open the next line.
The usual fix is secondary suppression: suppress a second cell in each
line, or remove the totals.

## Usage

``` r
check_suppression(x, by = NULL, count = NULL, config = NULL, target = NULL)
```

## Arguments

- x:

  An aggregated table: a data frame with a count column, or a `table`.

- by:

  Grouping variables. For an aggregated table the default is all
  non-numeric columns.

- count:

  Name of the count column in an aggregated table. Detected
  automatically when a column is called `n`, `count`, `freq` or similar.
  Leave `NULL` with `by` for microdata.

- config:

  A [`mimic_config()`](mimic_config.md); `NULL` uses the defaults.

- target:

  Label used in reports.

## Value

A `mimic_check`. `details` lists the cells that can be recovered, by
label.

## Details

Suppressed cells are `NA` or written as `"<5"`, `"."`, `".."`, `"x"` or
`"*"`. Totals released in another table are not seen here; see
[`check_differencing()`](check_differencing.md) and the register in
[`inspect()`](inspect.md).

## See also

Other checks: [`check_differencing()`](check_differencing.md),
[`check_dominance()`](check_dominance.md),
[`check_extremes()`](check_extremes.md),
[`check_group_disclosure()`](check_group_disclosure.md),
[`check_k_anonymity()`](check_k_anonymity.md),
[`check_keys()`](check_keys.md),
[`check_min_count()`](check_min_count.md),
[`check_model()`](check_model.md)

## Examples

``` r
tab <- data.frame(sex = c("F", "M", "Total"), n = c(NA, 120, 123))
check_suppression(tab)
#> ✖ suppression (tab): 1 of 1 suppressed cell(s) can be recovered from the totals; suppress a second cell in each line or remove the totals.
#>  sex
#>    F

# a second suppressed cell protects the first
tab$n[2] <- NA
check_suppression(tab)
#> ✔ suppression (tab): 2 suppressed cell(s) cannot be recovered from the totals in the table.
```
