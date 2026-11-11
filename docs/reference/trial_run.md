# Try a script on the synthetic copy

Runs the researcher's script on the copy from [`mimic()`](mimic.md) in a
fresh, isolated R session and reports whether it runs through, which
packages it uses, which files it writes, and whether it tries to save
data at row level. Run it before sending a script for
[`real_run()`](real_run.md): a script that fails here will fail on the
real data too, and a script that saves rows will be stopped at output
checking.

## Usage

``` r
trial_run(
  script,
  fake_data,
  output_dir = NULL,
  timeout = 600,
  config = NULL,
  check_output = TRUE
)
```

## Arguments

- script:

  Path to an R script, or R code as text.

- fake_data:

  The synthetic copy: a data frame or a named list of data frames,
  normally from [`mimic()`](mimic.md).

- output_dir:

  Directory the script writes to. By default a new temporary directory,
  returned in the result so the output can be looked at.

- timeout:

  Maximum run time in seconds.

- config:

  A [`mimic_config()`](mimic_config.md); `NULL` uses the defaults. The
  output check uses the same rules, so give the configuration staff use.
  The script's [`safe_count()`](safe_count.md),
  [`safe_summary()`](safe_summary.md) and
  [`safe_model()`](safe_model.md) follow it too.

- check_output:

  Run the written files through [`inspect()`](inspect.md); see the
  section above.

## Value

An object of class `mimic_trial` with `status` (`"ok"`, `"warn"` or
`"error"`), `error` (message and script line), `packages`, `files`,
`writes`, `row_level` (findings), `inspection` (the
[`inspect()`](inspect.md) result for the written files, or `NULL`),
`warnings`, `log` (the console output) and `output_dir`. A file that
needs changes at the output check makes the status `"warn"`.

## How the script sees the data

Each table is available as an object named after it (`persons`,
`contacts`, ...); a single data frame is called `data`. The working
directory is the output directory, also available as `output_dir`. Write
all output there with relative paths, exactly as on the real run.

## How row-level output is detected

- a write call
  ([`write.csv()`](https://rdrr.io/r/utils/write.table.html),
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html), `fwrite()`, ...)
  receives a table with many rows: at least 100 and at least half the
  rows of the smallest input table,

- a written file contains identifier values from the copy. Text files,
  `.rds` and `.RData` files are searched; other formats are listed as
  not scanned.

## The output check in advance

With `check_output = TRUE` the files the script writes are also run
through [`inspect()`](inspect.md), as staff will do with the output from
the real run. The researcher sees from outside that a table shows a
minimum, keeps a cell that is not suppressed or saves a model with
everyone's residuals, and can fix the script before a run on the real
data is spent on it.

The check reads only the output, not the copy: the rules that compare
with the microdata (dominance, values held by few persons) need the real
data and are left for [`inspect()`](inspect.md) inside. The counts come
from the copy, so a cell can be small here and large in the real data,
or the other way round. The mess [`mimic()`](mimic.md) adds (odd codes
such as `"?"` and `"999"`, empty strings) gives small cells in a table
that tabulates the raw variable; that is a hint to clean the codes in
the script, and a copy made with `mess = FALSE` shows whether anything
else is wrong. A file that needs changes because of how it is built (a
minimum column, a model object, a suppressed cell the totals give back)
will need changes inside too; a file approved here is not yet approved.

## Examples

``` r
# \donttest{
m <- profile_data(list(persons = fake_persons, contacts = fake_contacts),
                  keys = "person_id",
                  config = mimic_config(geo_vars = "settlement", text_vars = "note"))
copy <- mimic(m, seed = 1)
fit <- trial_run(c(
  "tab <- table(persons$sex)",
  "write.csv(as.data.frame(tab), 'sex.csv')"
), copy)
fit
#> <mimic_trial> (code)
#> ✔ The script ran through in 0 seconds.
#> ℹ Files written to C:/Users/danie/AppData/Local/Temp/RtmpCclG4q/trial_run_7b841d6b2da0: sex.csv
#> ✖ Output check on the copy: 1 file would be sent back:
#> sex.csv
#> • 5 of 10 cells have fewer than 5 persons.
#> ℹ The copy has made-up mess (odd codes, empty strings): small cells may come from it. Clean the codes in the script, or try a copy made with `mimic(mess = FALSE)`.
#> See `$inspection` for every check.
# }
```
