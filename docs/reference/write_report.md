# Write an inspection report for the approver

Writes the result of [`inspect()`](inspect.md) as a Markdown file: a
summary table with one verdict per file, followed by the details of
every check that did not pass. Save it outside the output directory, so
it is not released with the output.

## Usage

``` r
write_report(x, path)
```

## Arguments

- x:

  A `mimic_inspection` from [`inspect()`](inspect.md).

- path:

  File to write, for example `"inspection.md"`.

## Value

`path`, invisibly.

## Examples

``` r
out <- file.path(tempdir(), "report_example")
dir.create(out, showWarnings = FALSE)
write.csv(data.frame(sex = c("F", "M"), n = c(985, 3)),
          file.path(out, "by_sex.csv"), row.names = FALSE)
res <- inspect(out)
write_report(res, file.path(tempdir(), "inspection.md"))
```
