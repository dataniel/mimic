# Record released output in a register

Once output is released it can be combined with everything released
after it. The classic case is two runs of the same script: a table for
all persons, and later one for all persons except a settlement. Each is
safe; subtracted, they reveal the settlement. `record_release()` keeps
the released tables in a register, and [`inspect()`](inspect.md) with
the same `register` compares every new table with them.

## Usage

``` r
record_release(x, register, files = NULL)
```

## Arguments

- x:

  A `mimic_inspection` from [`inspect()`](inspect.md).

- register:

  Directory of the register. Created if needed; it must lie outside the
  output directory.

- files:

  Files that were released. By default those that were approved; files
  for review by hand are then named in a warning, because a released
  file missing from the register is not compared with later output.

## Value

The path of the recorded release, invisibly.

## Details

Call it after the approver has decided, with the files that actually
left the secure environment. Files that need changes cannot be recorded.
Files sent for review by hand can, once a person has approved them.

## The register

A directory in the secure environment with one `.rds` file per release:
its id, date, output directory, file names and the tables read from
them. Only the tables are kept, so figures and text are recorded by name
only. The register holds released output, but in full and in one place,
so it stays inside like the real data.

## Examples

``` r
reg <- file.path(tempdir(), "register")
out1 <- file.path(tempdir(), "release_1")
dir.create(out1, showWarnings = FALSE)
write.csv(data.frame(sex = c("F", "M"), n = c(985, 1015)),
          file.path(out1, "by_sex.csv"), row.names = FALSE)
record_release(inspect(out1, register = reg), reg)
#> ✔ Recorded 1 file with 1 table as release 20261111-210327.

# a month later, the same table without one small settlement
out2 <- file.path(tempdir(), "release_2")
dir.create(out2, showWarnings = FALSE)
write.csv(data.frame(sex = c("F", "M"), n = c(983, 1014)),
          file.path(out2, "by_sex.csv"), row.names = FALSE)
inspect(out2, register = reg)
#> <mimic_inspection> 1 file in
#> C:/Users/danie/AppData/Local/Temp/RtmpCclG4q/release_2
#> ℹ Checked without the real data: dominance and text checks are limited.
#> ℹ Compared with 1 earlier release.
#> ✖ NEEDS CHANGES: by_sex.csv
#> • Subtracting the tables reveals fewer than 5 persons in 2 cell(s) and the
#> grand totals. Compared with by_sex.csv from release 20261111-210327.
#> ────────────────────────────────────────────────────────────────────────────────
#> 0 approved, 1 needs changes, 0 to review by hand.
```
