# Run a script on the real data

Runs the researcher's script on the real data in a fresh, isolated R
session inside the secure environment. Output is kept in `output_dir`,
and a log of the code (optionally with its printed output, cleaned of
data values) is placed next to the output. Check the output with
[`inspect()`](inspect.md) before anything is released.

## Usage

``` r
real_run(
  script,
  real_data,
  output_dir,
  timeout = 3600,
  config = NULL,
  raw_log = NULL,
  log = c("code_only", "cleaned")
)
```

## Arguments

- script:

  Path to an R script, or R code as text.

- real_data:

  The real data: a data frame (available to the script as `data`) or a
  named list of data frames (available under their names).

- output_dir:

  Directory for the output. Created if needed; must be empty, so only
  output from this run can be released.

- timeout:

  Maximum run time in seconds.

- config:

  A [`mimic_config()`](mimic_config.md); `NULL` uses the defaults. The
  script's [`safe_count()`](safe_count.md),
  [`safe_summary()`](safe_summary.md) and
  [`safe_model()`](safe_model.md) follow it.

- raw_log:

  Optional path where the uncleaned log is saved for staff. It must lie
  outside `output_dir` and must not be released.

- log:

  `"code_only"` (the default) keeps only the code; `"cleaned"` also
  keeps its printed output with data values removed. See What the log
  contains.

## Value

An object of class `mimic_run` that is safe to pass on to the
researcher: `status` (`"ok"` or `"error"`), `error` (the safe error
report, see below), `files` written, `blocked` writes, `log_file` (the
cleaned log in `output_dir`), `redactions` and `warnings` (cleaned).

## Safe error report

When the script fails, the researcher cannot see the data to find out
why. `real_run()` returns what helps without revealing anything:

- the error class and the function that raised it,

- the line in the script,

- the error message with data values removed: quoted parts that are not
  words from the script or names in the data become `<value>`, every
  number becomes `<n>`, and identifiers, free text and rare values are
  removed,

- for every variable mentioned in the script or the message: class,
  share missing (rounded), number of distinct values, whether there are
  empty strings, and whether it is entirely missing.

The full call is never kept, because a call can carry a whole data
frame.

## What the log contains

The script is run with `source(echo = TRUE)`. By default
(`log = "code_only"`) the log keeps only the code, and each stretch of
printed output becomes `<output not kept>`. The researcher sees where
the script got to, but no output; results must come as files that
[`inspect()`](inspect.md) checks.

With `log = "cleaned"` the printed output is kept too, with values that
could point to a person replaced by `<value>`: identifiers, free text,
and any value (string, date or number with at least three digits, or a
rare age or other small number in a quasi-identifier) held by fewer than
`min_count` persons. Persons are counted on the identifier columns of
each table, so a code one person has on many contacts is still rare.
Numbers are compared at the precision they are printed with, so the
minimum shown by [`summary()`](https://rdrr.io/r/base/summary.html) as
`38390` is found even when the value is `38389.6`, and also when written
with thousands separators (`9.800.000`, `38,390`). Dates are found in
the common ways of writing them (`1959-01-31`, `31.01.1959`,
`31/01/1959`, `31011959`), and free text also when only its first part
is printed. A line with two or more such values is removed entirely: it
is most likely a printed row, and the common values left in it (sex,
settlement, occupation) could still single out a person in a small
place. Common values elsewhere are left, so the log stays readable.

Cleaning is a search for known values, so it cannot find what a script
has transformed: a name in capitals, a date with the month spelled out,
a piece cut from the middle of a note, a value divided by 1000. A line
with a single rare value also keeps its common values, and those can
still be a rare combination when a script prints rows. That is why the
code-only log is the default; choose the cleaned log for scripts staff
have read.

The names of files the script wrote, and of writes that were blocked,
are cleaned the same way, because a script can name a file after a
person.

`real_run()` does not know the keys of the tables. Persons are found
through identifier columns: names like `person_id`, `id`, `cpr...` or
`pnr...`, and the `id_vars` of the configuration. When the key has
another name (`lbnr`, `person`), add it to `id_vars`; otherwise rare
values are counted in rows, and a code one person has on twelve contacts
looks common.

## Limits

Write calls in base R, utils, grDevices and common packages (data.table,
readr, writexl, openxlsx, haven, arrow, ggplot2, qs, fst, ...) are
blocked when they target a path outside `output_dir`. This is a safety
net, not a sandbox: a determined script can write elsewhere through
other functions. File system permissions in the secure environment must
enforce what may be written where.

The data are passed to the child session through a temporary file that
callr deletes afterwards; it lies in the session's temporary directory
inside the secure environment.

## Examples

``` r
# \donttest{
out <- file.path(tempdir(), "run1")
r <- real_run(c(
  "tab <- table(persons$sex)",
  "write.csv(as.data.frame(tab), 'sex.csv', row.names = FALSE)"
), list(persons = fake_persons), out)
r
#> <mimic_run> (code)
#> ✔ The script ran through on the real data in under a second.
#> ℹ Output files: sex.csv
#> ℹ Check the output with `inspect()` before release.

# a failing script gets a safe error report
r2 <- real_run("fit <- lm(income ~ settlment, data = persons)",
               list(persons = fake_persons), file.path(tempdir(), "run2"))
r2
#> <mimic_run> (code)
#> ✖ The script failed at line 1.
#> Message (data values removed): object 'settlment' not found
#> Variables involved:
#>    table variable   class missing    distinct empty_strings all_missing
#>  persons   income numeric     3 % about 1,615            NA       FALSE
#> ℹ No output files.
#> ℹ Check the output with `inspect()` before release.
# }
```
