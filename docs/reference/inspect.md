# Check output before release

The final inspection before anything leaves the secure environment.
`inspect()` reads every file in `output_dir`, runs the relevant
[checks](checks.md) and gives each file one verdict for the person who
approves the release:

## Usage

``` r
inspect(output_dir, config = NULL, real_data = NULL, register = NULL)
```

## Arguments

- output_dir:

  Directory with the output, normally from [`real_run()`](real_run.md).

- config:

  A [`mimic_config()`](mimic_config.md); `NULL` uses the defaults.

- real_data:

  Optional real data (a data frame or a named list of data frames) for
  the checks that need microdata.

- register:

  Optional directory with the register of earlier releases kept by
  [`record_release()`](record_release.md). A directory that does not
  exist yet holds no releases.

## Value

An object of class `mimic_inspection`: `files`, a data frame with one
row per file (`file`, `type`, `verdict`, `reasons`, `description`),
`checks`, the full checks per file, and `tables`, the tables read from
the files. It holds output that is not yet approved and stays in the
secure environment. Write it for the approver with
[`write_report()`](write_report.md).

## Details

- **APPROVED**: every check passed.

- **NEEDS CHANGES**: a rule is broken; the reasons say which and where.

- **REVIEW BY HAND**: the file could not be checked automatically
  (images, spreadsheets, unknown objects) or something needs a human
  judgement. When in doubt, mimic does not approve.

## What is checked

- **Tables** (`.csv`, `.tsv`, data frames in `.rds`/`.RData`):

  - cells below `min_count` with
    [`check_min_count()`](check_min_count.md): a count column named `n`,
    `count`, `freq` or similar, or a cross table whose numeric cells are
    all whole numbers,

  - minimum, maximum and range values with
    [`check_extremes()`](check_extremes.md),

  - tables of magnitudes (sums, means, medians) without a count column
    are sent for review by hand, because the cell sizes behind them are
    unknown,

  - row-level data: identifier columns, rows that repeat a combination
    of the grouping columns (an aggregated table has one row per
    combination), or (with `real_data`) many rows or identifier values
    from the real data.

  - suppressed cells (`NA`, `"<5"`, `"."`, ...) that can be recovered
    from totals in the table with
    [`check_suppression()`](check_suppression.md),

  - groups whose persons all share one value with
    [`check_group_disclosure()`](check_group_disclosure.md),

  - rows with a suppressed count that still show a share, sum, count of
    part of the group or other number (`n = "<5"` next to `pct = 0.15`
    or `n_female = 2`),

  - counts of part of the group (`n_female` next to `n`) checked for
    small cells like the count, and a suppressed one that is the count
    minus the other parts (`n_female = "<5"` next to `n` and `n_male`),

  - tables with neither counts nor numbers (a list of dates, places or
    notes) are sent for review by hand: they list values rather than
    aggregate them,

- **Models** in `.rds`/`.RData` with [`check_model()`](check_model.md).

- **Pairs of tables** with
  [`check_differencing()`](check_differencing.md), when they have the
  same grouping columns or one grouping is part of the other (the finer
  table is summed up; rows labelled `"Total"` are left out).

- **Text** (the run log, `.txt`, `.md`, `.html`): with `real_data`,
  searched for values that could point to a person.

## Descriptions

When the researcher described the files with
[`describe_output()`](describe_output.md), `inspect()`

- checks a table by the count and grouping columns named there instead
  of guessing them, and sends it back when a named column is missing,

- checks the numbers behind a figure, and sends the figure back when
  they need changes or are missing (the figure itself is still reviewed
  by hand),

- checks the description file like other text, because the researcher
  wrote it,

- shows the descriptions in [`write_report()`](write_report.md).

With `require_description = TRUE` in
[`mimic_config()`](mimic_config.md), every output file without a
description is sent back.

A table of model coefficients is approved when it carries `df_residual`
(and `observations`) at or above the limits, as tables from
[`safe_model()`](safe_model.md) do; otherwise it is reviewed by hand.

## With the real data

Some rules can only be checked against the microdata. Give `real_data`
and `inspect()` also

- applies the dominance rules with
  [`check_dominance()`](check_dominance.md) to tables of sums and means:
  a column such as `mean_income` or `sum_income` is matched to the
  variable `income`, and the table's grouping columns to the variables
  of the same name,

- flags numbers in tables that equal a value held by fewer than
  `min_count` persons (often a median or extreme of a small group),

- searches text files, the text cells of tables and file names for
  identifiers, free text and rare values (a day-precise date, a code or
  a settlement held by fewer than `min_count` persons).

Persons are found through identifier columns in the real data: names
like `person_id`, `cpr...` or `pnr...`, and the `id_vars` of the
configuration. Add a key with another name (`lbnr`) to `id_vars`, or
rare values are counted in rows.

## Counts are taken as persons

A count column is read as a number of persons, because that is what
`min_count` is about. A table of contacts per settlement can pass with
12 contacts that all belong to one person. Without the real data
`inspect()` cannot tell; with `real_data` a label held by fewer than
`min_count` persons is found whatever the count next to it says. Ask for
counts of persons, not of rows, when the script works on a linked table.

## Earlier releases

The most realistic differencing attack does not use two tables from one
run but two runs: the same table for all persons, and a month later for
all persons except one settlement. Each run has its own empty output
directory, so `inspect()` only sees it with a register. Record what is
released with [`record_release()`](record_release.md) and give the same
`register` every time: each table is then also compared with every table
released before.

## Examples

``` r
out <- file.path(tempdir(), "inspect_example")
dir.create(out, showWarnings = FALSE)
tab <- as.data.frame(table(settlement = fake_persons$settlement))
names(tab)[2] <- "n"
write.csv(tab, file.path(out, "by_settlement.csv"), row.names = FALSE)
write.csv(data.frame(sex = c("F", "M"), n = c(985, 1015)),
          file.path(out, "by_sex.csv"), row.names = FALSE)
res <- inspect(out, real_data = list(persons = fake_persons))
res
#> <mimic_inspection> 2 files in
#> C:/Users/danie/AppData/Local/Temp/RtmpCclG4q/inspect_example
#> ℹ No register: tables were not compared with earlier releases.
#> ✖ NEEDS CHANGES: by_settlement.csv
#> • 5 of 22 cells have fewer than 5 persons.
#> • Contains 5 value(s) that could point to a person (free text, or a date, code
#> or label held by fewer than 5 persons).
#> ✔ APPROVED: by_sex.csv
#> ────────────────────────────────────────────────────────────────────────────────
#> 1 approved, 1 needs changes, 0 to review by hand.
```
