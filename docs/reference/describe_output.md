# Describe an output file for the checker

Called in the researcher's script after a file is written. Records what
the file shows, which column holds the number of persons, which columns
group the table, where the numbers behind a figure are, and what changed
since the last request. [`inspect()`](inspect.md) reads the
descriptions: it checks a table by the columns named here instead of
guessing them from the column names, checks the numbers behind a figure,
and shows the descriptions to the approver in
[`write_report()`](write_report.md).

## Usage

``` r
describe_output(
  file,
  note,
  count = NULL,
  by = NULL,
  data = NULL,
  changes = NULL,
  dir = "."
)
```

## Arguments

- file:

  Path of the file written, relative to `dir`.

- note:

  What the file shows, in a sentence or two.

- count:

  For a table: the column with the number of persons per row.

- by:

  For a table: the columns that group it (one row per combination).

- data:

  For a figure: the numbers behind it, as a data frame (written to
  `<figure>_data.csv`) or the path of a file in `dir` that holds them.

- changes:

  What changed since the output was last requested.

- dir:

  The output directory. In [`trial_run()`](trial_run.md) and
  [`real_run()`](real_run.md) the working directory is the output
  directory.

## Value

The path of the description file, invisibly.

## Why it exists

A checker cannot release what they do not understand. OpenSAFELY reports
that about 25 % of rejected requests lack context for the output and 10
% lack the data behind a figure
(<https://docs.opensafely.org/output-checking/>).
[`inspect()`](inspect.md) also has to guess: a count column is found by
its name (`n`, `count`, ...), and a figure cannot be read at all. A
description removes the guesswork.

## Where descriptions are kept

All descriptions go into one text file, `output_descriptions.dcf`, in
`dir`: the working directory, which is the output directory when the
script runs in [`trial_run()`](trial_run.md) or
[`real_run()`](real_run.md). Describing a file again replaces its
description, so the script can be run again. The file is output too: it
is checked and released with the rest, so the note must not mention
values of persons.

## Figures

A figure cannot be checked automatically, but the numbers behind it can.
Give them as `data`, a data frame such as the one the figure was drawn
from: it is written next to the figure as `<figure>_data.csv` and
checked like any table. A figure whose numbers need changes needs
changes too; a figure is never approved automatically, because only a
person can see whether it shows more than its numbers (single points in
a scatter plot, a small bar). Give the counts behind the figure, not
row-level data.

## See also

Other safe output: [`safe_count()`](safe_count.md),
[`safe_model()`](safe_model.md), [`safe_summary()`](safe_summary.md)

## Examples

``` r
out <- file.path(tempdir(), "describe_example")
dir.create(out, showWarnings = FALSE)
tab <- safe_count(fake_persons, c("sex", "occupation"))
#> Suppressed 1 cell with fewer than 5 persons, and 3 more so it cannot be worked
#> out from the totals.
write.csv(tab, file.path(out, "sex_occupation.csv"), row.names = FALSE)
describe_output("sex_occupation.csv", "Persons by sex and occupation, 2024.",
                count = "n", by = c("sex", "occupation"), dir = out)
inspect(out)
#> <mimic_inspection> 2 files in
#> C:/Users/danie/AppData/Local/Temp/RtmpCclG4q/describe_example
#> ℹ Checked without the real data: dominance and text checks are limited.
#> ℹ No register: tables were not compared with earlier releases.
#> ! REVIEW BY HAND: output_descriptions.dcf
#> • Descriptions written by the researcher cannot be searched without the real
#> data; read them for values of persons.
#> ✔ APPROVED: sex_occupation.csv
#> ────────────────────────────────────────────────────────────────────────────────
#> 1 approved, 0 need changes, 1 to review by hand.
```
